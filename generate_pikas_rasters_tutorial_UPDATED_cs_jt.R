#!/usr/bin/env Rscript
###############################################################################
# generate_pikas_rasters_tutorial.R
#
# Purpose
# -------
# Generate the “accumulation-season SWE threshold metric” discussed with
# Chris Smith (UNR) + Adrian Harpold:
#
#   Metric definition (per water year):
#     For each month in Oct–Mar (6 months), count the number of DAILY timesteps
#     where SWE is below a threshold (default threshold defined below).
#
#   Output (per water year HDF5 file):
#     A 6-band GeoTIFF raster stack:
#       Band 1: Oct  count of days SWE < threshold
#       Band 2: Nov  count of days SWE < threshold
#       Band 3: Dec  count of days SWE < threshold
#       Band 4: Jan  count of days SWE < threshold
#       Band 5: Feb  count of days SWE < threshold
#       Band 6: Mar  count of days SWE < threshold
#
# Why this is useful
# ------------------
# This monthly “days below threshold” metric captures how often conditions are
# *not* above the threshold during early/mid accumulation season (Oct–Mar). It
# can be used as a covariate in pika / small-mammal habitat and survival models.
#
# Data assumptions
# ----------------
# 1) Input is a set of annual HDF5 files (one per water year) with dataset "/SWE"
#    shaped [rows x cols x days].
# 2) The daily axis starts on Oct 1 and runs through Sep 30 of that water year.
# 3) SWE units are millimeters (mm).
# 4) Missing/no-data values use -32768.
# 5) Filenames contain WY as "WY1993" etc (e.g., SN_SWE_WY1993.h5).
#    Start date is inferred as Oct 1 of (WY - 1), i.e. WY1993 => 1992-10-01.
#
# Performance approach
# --------------------
# These cubes are huge. To reduce RAM:
#   - Read the SWE cube in two latitude “halves” (top/bottom).
#   - Apply the metric pixel-wise on each half using base::apply().
#   - Bind halves and write a GeoTIFF.
#
# Parallelism note (IMPORTANT)
# ---------------------------
# Many geospatial libraries (GDAL/PROJ, used under-the-hood by terra) are not
# reliably “fork-safe” on some systems. If you see errors like:
#   “all scheduled cores encountered errors in user code”
# use PARALLEL_MODE = "psock" (default below), which starts separate R sessions.
#
# Updated: 2026-01-09
###############################################################################

###############################################################################
# Package installation (auto-install if missing)
###############################################################################

options(repos = c(CRAN = "https://cloud.r-project.org"))

install_if_missing_cran <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) > 0) {
    message("Installing missing CRAN packages: ", paste(missing, collapse = ", "))
    install.packages(missing, dependencies = TRUE)
  }
}

install_if_missing_bioc <- function(pkgs) {
  if (!"BiocManager" %in% rownames(installed.packages())) {
    message("Installing BiocManager (required for Bioconductor packages).")
    install.packages("BiocManager")
  }
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) > 0) {
    message("Installing missing Bioconductor packages: ", paste(missing, collapse = ", "))
    BiocManager::install(missing, ask = FALSE, update = FALSE)
  }
}

# CRAN packages used in this script
install_if_missing_cran(c("terra", "pbmcapply", "pbapply"))

# Bioconductor package used in this script
install_if_missing_bioc(c("rhdf5"))

suppressPackageStartupMessages({
  library(rhdf5)      # HDF5 I/O
  library(terra)      # raster handling + GeoTIFF writing
  library(parallel)   # detectCores + PSOCK clusters
  library(pbapply)    # progress bar for PSOCK cluster runs
  library(pbmcapply)  # progress bar for fork runs (Unix-like OS)
})

###############################################################################
# 0) USER SETTINGS (edit these paths if Chris runs elsewhere)
###############################################################################

ROOT_DIR <- "~/ch1_margulis"
SWE_HDF_DIR <- file.path(ROOT_DIR, "swe/hdf")
STATIC_DIR  <- file.path(ROOT_DIR, "rasters/static")
SNOW_METRICS_DIR <- file.path(ROOT_DIR, "rasters/snow_metrics")

# Metric settings
SWE_THRESHOLD_MM <- 166  # Example: 16.6 cm SWE in mm (site-average density assumption)
SNOW_METRIC_NAME <- paste0("pikas_days_swe_lt_", SWE_THRESHOLD_MM, "mm_oct_mar")
LAYER_NAMES <- paste0(
  "days_swe_lt_", SWE_THRESHOLD_MM, "mm_",
  c("Oct","Nov","Dec","Jan","Feb","Mar")
)

# Margulis CA domain extent (xmin, xmax, ymin, ymax)
EXTENT_CA <- c(-123.3, -117.6, 35.4, 42)

# Parallel controls
PARALLEL_MODE <- "psock"   # "psock" (recommended) or "fork"
NCORES <- min(5, max(1, parallel::detectCores() - 1))

###############################################################################
# 1) INPUT DISCOVERY + CRS (from DEM)
###############################################################################

setwd(ROOT_DIR)

swe_list <- list.files(SWE_HDF_DIR, pattern = "\\.h5$", full.names = TRUE)
if (length(swe_list) == 0) stop("No .h5 files found in: ", SWE_HDF_DIR)
message("Found ", length(swe_list), " HDF5 water-year SWE files.")

static_list <- list.files(STATIC_DIR, pattern = "\\.tif$", full.names = TRUE)
if (length(static_list) == 0) stop("No .tif static rasters found in: ", STATIC_DIR)

dem_candidates <- static_list[grepl("dem", basename(static_list), ignore.case = TRUE)]
dem_path <- if (length(dem_candidates) > 0) dem_candidates[1] else static_list[1]
dem <- terra::rast(dem_path)
dem_crs_wkt <- terra::crs(dem)
message("Using DEM/static raster for CRS: ", dem_path)

###############################################################################
# 2) HELPERS + METRIC FUNCTION
###############################################################################

# Infer the water-year start date from the filename:
#   SN_SWE_WY1993.h5 => WY=1993 => starts 1992-10-01
wy_start_date_from_path <- function(path) {
  wy <- as.integer(sub(".*WY([0-9]{4}).*", "\\1", basename(path)))
  if (is.na(wy)) stop("Could not parse WY from filename: ", basename(path))
  as.Date(sprintf("%d-10-01", wy - 1))
}

# Metric: days SWE < threshold per month for Oct–Mar
accum_swe_below_thres_oct_mar <- function(x, start_date, swe_thres) {
  x <- as.numeric(x)
  x[x == -32768] <- NA
  if (all(is.na(x))) return(rep(NA_integer_, 6))
  
  dates <- seq.Date(as.Date(start_date), by = "day", length.out = length(x))
  month_num <- as.integer(format(dates, "%m"))
  
  months <- c(10, 11, 12, 1, 2, 3)  # Oct–Mar
  out <- rep(NA_integer_, length(months))
  
  for (i in seq_along(months)) {
    v <- x[month_num == months[i]]
    out[i] <- if (length(v) == 0 || all(is.na(v))) NA_integer_ else sum(v < swe_thres, na.rm = TRUE)
  }
  out
}

###############################################################################
# 3) RASTER GENERATOR: READ HDF5 IN TWO HALVES + APPLY METRIC + WRITE GeoTIFF
###############################################################################

generate_snow_metric_rasters <- function(
    swe_h5_path,
    snow_metric_function,
    snow_metric_name,
    layer_names = NULL,
    dem_crs = NULL,
    extent_vec = NULL,
    swe_thres = SWE_THRESHOLD_MM
) {
  if (!file.exists(swe_h5_path)) stop("Missing HDF5 file: ", swe_h5_path)
  if (is.null(dem_crs)) stop("dem_crs (WKT) must be provided.")
  if (is.null(extent_vec) || length(extent_vec) != 4) stop("extent_vec must be length-4 (xmin,xmax,ymin,ymax).")
  
  # Confirm dataset exists
  test <- rhdf5::h5ls(swe_h5_path)
  swe_row <- which(test$name == "SWE" & test$group == "/")
  if (length(swe_row) == 0) swe_row <- which(test$name == "SWE")[1]
  if (length(swe_row) == 0 || is.na(swe_row)) stop("Could not find /SWE dataset in: ", swe_h5_path)
  
  # Robust dim parsing: expects something like "6601 x 5701 x 365"
  dims_chr <- as.character(test$dim[swe_row])
  parts <- as.integer(trimws(unlist(strsplit(dims_chr, "x"))))
  if (length(parts) < 3 || anyNA(parts)) stop("Could not parse dims: ", dims_chr)
  
  nrow <- parts[1]
  ncol <- parts[2]
  nday <- parts[3]
  if (nday < 300) stop("Parsed nday looks wrong: ", nday, " from dims: ", dims_chr)
  
  # Dynamic split into two halves
  split_row <- floor(nrow / 2)
  north_rows <- 1:split_row
  south_rows <- (split_row + 1):nrow
  cols <- 1:ncol
  days <- 1:nday
  
  # Infer date axis from filename (WY-based)
  start_date <- wy_start_date_from_path(swe_h5_path)
  
  # Read + compute on first half
  c1 <- rhdf5::h5read(swe_h5_path, "/SWE", index = list(north_rows, cols, days))
  message(basename(swe_h5_path), " | c1 read (rows 1-", split_row, ")")
  res_c1 <- apply(
    c1, c(1, 2),
    snow_metric_function,
    start_date = start_date,
    swe_thres = swe_thres
  )
  message(basename(swe_h5_path), " | c1 metric")
  rm(c1)
  
  # Read + compute on second half
  c2 <- rhdf5::h5read(swe_h5_path, "/SWE", index = list(south_rows, cols, days))
  message(basename(swe_h5_path), " | c2 read (rows ", split_row + 1, "-", nrow, ")")
  res_c2 <- apply(
    c2, c(1, 2),
    snow_metric_function,
    start_date = start_date,
    swe_thres = swe_thres
  )
  message(basename(swe_h5_path), " | c2 metric")
  rm(c2)
  
  # Close any open handles in this R session
  rhdf5::h5closeAll()
  
  # Convert results into a SpatRaster
  # If FUN returns length K, apply() typically yields array [K, rows, cols]
  if (length(dim(res_c1)) != 3) {
    stop("Unexpected metric output shape for ", basename(swe_h5_path),
         ". Expected a 3D array from apply() because metric returns 6 values.")
  }
  
  if (!is.null(layer_names) && dim(res_c1)[1] != length(layer_names)) {
    stop("Metric returned ", dim(res_c1)[1], " layers but layer_names has ", length(layer_names))
  }
  
  arr1 <- aperm(res_c1, c(2, 3, 1))  # [rows, cols, layers]
  arr2 <- aperm(res_c2, c(2, 3, 1))
  
  r1 <- terra::rast(arr1)
  r2 <- terra::rast(arr2)
  r  <- terra::rbind(r1, r2)
  
  if (!is.null(layer_names)) terra::names(r) <- layer_names
  
  terra::ext(r) <- extent_vec
  terra::crs(r) <- dem_crs
  
  # Output naming
  name <- gsub("\\.h5$", "", basename(swe_h5_path))
  good_name <- gsub("SN_SWE", snow_metric_name, name)
  
  out_dir <- file.path(SNOW_METRICS_DIR, snow_metric_name)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_path <- file.path(out_dir, paste0(good_name, ".tif"))
  terra::writeRaster(r, out_path, overwrite = TRUE)
  message("Wrote: ", out_path)
  
  out_path
}

###############################################################################
# 4) RUN ACROSS YEARS (robust parallel options + error capture)
###############################################################################

message("Running metric: ", SNOW_METRIC_NAME)
message("Threshold (mm): ", SWE_THRESHOLD_MM)
message("Parallel mode: ", PARALLEL_MODE, " | cores: ", NCORES)

run_one_year <- function(h5_path) {
  tryCatch(
    generate_snow_metric_rasters(
      swe_h5_path = h5_path,
      snow_metric_function = accum_swe_below_thres_oct_mar,
      snow_metric_name = SNOW_METRIC_NAME,
      layer_names = LAYER_NAMES,
      dem_crs = dem_crs_wkt,
      extent_vec = EXTENT_CA,
      swe_thres = SWE_THRESHOLD_MM
    ),
    error = function(e) {
      msg <- paste0("FAILED for ", basename(h5_path), " | ", conditionMessage(e))
      message(msg)
      structure(msg, class = "pikas_metric_error")
    }
  )
}

if (NCORES <= 1) {
  
  out_files <- lapply(swe_list, run_one_year)
  
} else if (tolower(PARALLEL_MODE) == "fork" && Sys.info()[["sysname"]] %in% c("Linux","Darwin")) {
  
  out_files <- pbmcapply::pbmclapply(
    swe_list, run_one_year,
    mc.cores = NCORES, mc.cleanup = TRUE, mc.preschedule = FALSE
  )
  
} else {
  
  cl <- parallel::makeCluster(NCORES)
  
  # Load required pkgs on workers
  parallel::clusterEvalQ(cl, {
    suppressPackageStartupMessages({
      library(rhdf5)
      library(terra)
    })
    NULL
  })
  
  # Export functions + constants needed on workers
  parallel::clusterExport(
    cl,
    varlist = c(
      "wy_start_date_from_path",
      "accum_swe_below_thres_oct_mar",
      "generate_snow_metric_rasters",
      "SNOW_METRICS_DIR",
      "SNOW_METRIC_NAME",
      "LAYER_NAMES",
      "SWE_THRESHOLD_MM",
      "dem_crs_wkt",
      "EXTENT_CA"
    ),
    envir = environment()
  )
  
  worker_fun <- function(h5_path) {
    tryCatch(
      generate_snow_metric_rasters(
        swe_h5_path = h5_path,
        snow_metric_function = accum_swe_below_thres_oct_mar,
        snow_metric_name = SNOW_METRIC_NAME,
        layer_names = LAYER_NAMES,
        dem_crs = dem_crs_wkt,
        extent_vec = EXTENT_CA,
        swe_thres = SWE_THRESHOLD_MM
      ),
      error = function(e) paste0("FAILED for ", basename(h5_path), " | ", conditionMessage(e))
    )
  }
  
  out_files <- pbapply::pblapply(swe_list, worker_fun, cl = cl)
  parallel::stopCluster(cl)
}

is_fail <- vapply(
  out_files,
  function(x) inherits(x, "pikas_metric_error") || (is.character(x) && grepl("^FAILED", x)),
  logical(1)
)

message("Finished. Successful years: ", sum(!is_fail), " | Failed years: ", sum(is_fail))

if (any(!is_fail)) {
  message("Example output file:\n", out_files[[which(!is_fail)[1]]])
} else {
  message("No successful outputs yet. First failure:\n", out_files[[1]])
}

###############################################################################
# 5) OPTIONAL: single-file smoke test (uncomment to run just one year)
###############################################################################
run_one_year("~/ch1_margulis/swe/hdf/SN_SWE_WY1993.h5")
