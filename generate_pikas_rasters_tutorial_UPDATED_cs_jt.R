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
#     For each month in Oct–Feb (5 months), count the number of DAILY timesteps
#     where SWE is below a threshold (default: 20 cm SWE = 200 mm).
#
#   Output (per water year HDF5 file):
#     A 5-band GeoTIFF raster stack:
#       Band 1: Oct  count of days SWE < threshold
#       Band 2: Nov  count of days SWE < threshold
#       Band 3: Dec  count of days SWE < threshold
#       Band 4: Jan  count of days SWE < threshold
#       Band 5: Feb  count of days SWE < threshold
#
# Why this is useful
# ------------------
# This monthly “days below threshold” metric captures how often conditions are
# *not* above the threshold during the accumulation season (Oct–Feb). It can be
# used as a covariate in pika / small-mammal habitat and survival models.
#
# Data assumptions
# ----------------
# 1) Input is a set of annual HDF5 files (one per water year) with dataset "/SWE"
#    shaped [rows x cols x days] matching the Margulis CA domain.
# 2) The daily axis starts on Oct 1 and runs through Sep 30 of that water year.
# 3) SWE units are millimeters (mm). (So 20 cm SWE = 200 mm.)
# 4) Missing/no-data values use -32768.
#
# Performance approach
# --------------------
# These cubes are huge. To reduce RAM:
#   - We read the SWE cube in two latitude “halves” (north/south).
#   - We apply the metric pixel-wise on each half using base::apply().
#   - We bind halves and write a GeoTIFF.
#
# Parallelism note (IMPORTANT)
# ---------------------------
# Many geospatial libraries (GDAL/PROJ, used under-the-hood by terra) are not
# reliably “fork-safe” on some systems. If you see errors like:
#   “all scheduled cores encountered errors in user code”
# use PARALLEL_MODE = "psock" (default below), which starts separate R sessions.
#
# Updated: 2025-12-22
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
SWE_THRESHOLD_MM <- 166  ##255 kg/m3  (sd 85.4) avg density SWE in sierra's (40 snotel sites; Oct1-Apr1) = 16.6 cm depth
SNOW_METRIC_NAME <- paste0("pikas_days_swe_lt_", SWE_THRESHOLD_MM, "mm_oct_mar")
LAYER_NAMES <- paste0("days_swe_lt_", SWE_THRESHOLD_MM, "mm_", c("Oct","Nov","Dec","Jan","Feb","Mar"))

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
# 2) METRIC FUNCTION: DAYS SWE < THRESHOLD PER MONTH (Oct–Feb)
###############################################################################

accum_swe_below_thres_oct_mar <- function(x, swe_thres = 166) {
  x <- as.numeric(x)
  x[x == -32768] <- NA
  if (all(is.na(x))) return(rep(NA_integer_, 5))

  start_date <- if (length(x) == 366) as.Date("2003-10-01") else as.Date("2001-10-01")
  dates <- seq.Date(start_date, by = "day", length.out = length(x))
  month_num <- as.integer(format(dates, "%m"))

  months <- c(10, 11, 12, 1, 2, 3)  # Oct–Feb

  out <- integer(length(months))
  for (i in seq_along(months)) {
    v <- x[month_num == months[i]]
    out[i] <- if (all(is.na(v))) NA_integer_ else sum(v < swe_thres, na.rm = TRUE)
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
  ...
) {
  if (!file.exists(swe_h5_path)) stop("Missing HDF5 file: ", swe_h5_path)
  if (is.null(dem_crs)) stop("dem_crs (WKT) must be provided.")
  if (is.null(extent_vec) || length(extent_vec) != 4) stop("extent_vec must be length-4 (xmin,xmax,ymin,ymax).")

  test <- rhdf5::h5ls(swe_h5_path)
  swe_row <- which(test$name == "SWE" & test$group == "/")
  if (length(swe_row) == 0) swe_row <- which(test$name == "SWE")[1]
  if (is.na(swe_row)) stop("Could not find /SWE dataset in: ", swe_h5_path)

  dims <- test$dim[swe_row]
  nday <- as.integer(sub("6601 x 5701 x ", "", dims))
  if (is.na(nday) || nday < 300) stop("Could not parse nday from HDF5 dims: ", dims)

  north_rows <- 1:3300
  south_rows <- 3301:6601
  cols <- 1:5701
  days <- 1:nday

  c1 <- rhdf5::h5read(swe_h5_path, "/SWE", index = list(north_rows, cols, days))
  message(basename(swe_h5_path), " | c1 read")
  res_c1 <- apply(c1, c(1,2), snow_metric_function, ...)
  message(basename(swe_h5_path), " | c1 metric")
  rm(c1)

  c2 <- rhdf5::h5read(swe_h5_path, "/SWE", index = list(south_rows, cols, days))
  message(basename(swe_h5_path), " | c2 read")
  res_c2 <- apply(c2, c(1,2), snow_metric_function, ...)
  message(basename(swe_h5_path), " | c2 metric")
  rm(c2)

  rhdf5::h5closeAll()

  if (length(dim(res_c1)) == 2) {
    full_res <- rbind(as.matrix(res_c1), as.matrix(res_c2))
    r <- terra::rast(full_res)
    rm(full_res)
  } else {
    arr1 <- aperm(res_c1, c(2,3,1))
    arr2 <- aperm(res_c2, c(2,3,1))
    r1 <- terra::rast(arr1)
    r2 <- terra::rast(arr2)
    r  <- terra::rbind(r1, r2)
    if (!is.null(layer_names)) terra::names(r) <- layer_names
    rm(arr1, arr2, r1, r2)
  }

  r[r == -32768] <- NA
  terra::ext(r) <- extent_vec
  terra::crs(r) <- dem_crs

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
  parallel::clusterEvalQ(cl, { library(rhdf5); library(terra); NULL })
  parallel::clusterExport(
    cl,
    varlist = c(
      "generate_snow_metric_rasters",
      "accum_swe_below_thres_oct_mar",
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
  message("Example output file:
", out_files[[which(!is_fail)[1]]])
} else {
  message("No successful outputs yet. First failure:
", out_files[[1]])
}
