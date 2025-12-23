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
#   Output (per water year):
#     A 5-band GeoTIFF raster stack:
#       Band 1: Oct  count of days SWE < threshold
#       Band 2: Nov  count of days SWE < threshold
#       Band 3: Dec  count of days SWE < threshold
#       Band 4: Jan  count of days SWE < threshold
#       Band 5: Feb  count of days SWE < threshold
#
# Data assumptions
# ----------------
# 1) Input is a set of annual HDF5 files (one per water year) with dataset "/SWE"
#    shaped [rows x cols x days].
# 2) The daily axis starts on Oct 1 and runs through Sep 30 of that water year.
# 3) SWE units are millimeters (mm). (So 20 cm SWE = 200 mm.)
# 4) Missing/no-data values use -32768.
#
# Performance approach
# --------------------
# - Read each annual SWE cube in two row halves (top/bottom) to reduce RAM.
# - Compute the metric pixel-wise on each half via apply().
# - Convert each half to a 5-band SpatRaster with the correct HALF extent.
# - Merge halves into a full-domain raster and write a GeoTIFF.
#
# Parallelism note
# ---------------
# Parallelizing across years can speed things up, but each worker reads huge
# arrays, so RAM can become the limiting factor. Use fewer cores if needed.
#
# Updated: 2025-12-23
###############################################################################

###############################################################################
# Packages (auto-install if missing)
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

install_if_missing_cran(c("terra", "pbapply", "pbmcapply"))
install_if_missing_bioc(c("rhdf5"))

suppressPackageStartupMessages({
  library(terra)
  library(rhdf5)
  library(parallel)
  library(pbapply)
  library(pbmcapply)
})

###############################################################################
# 0) USER SETTINGS
###############################################################################
ROOT_DIR <- "~/ch1_margulis"
SWE_HDF_DIR <- file.path(ROOT_DIR, "swe/hdf")
STATIC_DIR  <- file.path(ROOT_DIR, "rasters/static")
SNOW_METRICS_DIR <- file.path(ROOT_DIR, "rasters/snow_metrics")

# Metric settings
SWE_THRESHOLD_MM <- 200
SNOW_METRIC_NAME <- paste0("pikas_days_swe_lt_", SWE_THRESHOLD_MM, "mm_oct_feb")
LAYER_NAMES <- paste0("days_swe_lt_", SWE_THRESHOLD_MM, "mm_", c("Oct","Nov","Dec","Jan","Feb"))

# Extent handling: safest is DEM extent (matches grid)
USE_DEM_EXTENT <- TRUE
EXTENT_OVERRIDE <- c(-123.3, -117.6, 35.4, 42)  # only used if USE_DEM_EXTENT=FALSE

# Output write options (int16 is plenty: max days in a month ~31)
WRITE_OPTS <- list(datatype = "INT2S", NAflag = -32768)
# Optional compression:
# WRITE_OPTS <- c(WRITE_OPTS, list(gdal = "COMPRESS=LZW"))

# Parallel controls (for the PARALLEL batch option at the end)
PARALLEL_MODE <- "psock"   # "psock" (recommended) or "fork"
NCORES <- min(5, max(1, parallel::detectCores() - 1))

###############################################################################
# 1) INPUT DISCOVERY + CRS/EXTENT FROM DEM
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
dem_extent  <- terra::ext(dem)

message("Using DEM/static raster for CRS/extent: ", dem_path)
message("DEM CRS (first 120 chars): ", substr(dem_crs_wkt, 1, 120), "...")
message("DEM extent: ", paste(c(dem_extent[1], dem_extent[2], dem_extent[3], dem_extent[4]), collapse = ", "))

EXTENT_VEC <- if (USE_DEM_EXTENT) c(dem_extent[1], dem_extent[2], dem_extent[3], dem_extent[4]) else EXTENT_OVERRIDE
message("OUTPUT extent set to: ", paste(EXTENT_VEC, collapse = ", "))

###############################################################################
# 2) METRIC FUNCTION (pixel time-series -> length-5 integer vector)
###############################################################################
accum_swe_below_thres_oct_feb <- function(x, swe_thres = 200) {
  x <- as.numeric(x)
  x[x == -32768] <- NA
  if (all(is.na(x))) return(rep(NA_integer_, 5))
  
  # Only need month boundaries; choose leap/non-leap reference year based on length
  start_date <- if (length(x) == 366) as.Date("2003-10-01") else as.Date("2001-10-01")
  dates <- seq.Date(start_date, by = "day", length.out = length(x))
  month_num <- as.integer(format(dates, "%m"))
  
  months <- c(10, 11, 12, 1, 2)  # Oct–Feb
  out <- integer(5)
  for (i in seq_along(months)) {
    v <- x[month_num == months[i]]
    out[i] <- if (all(is.na(v))) NA_integer_ else as.integer(sum(v < swe_thres, na.rm = TRUE))
  }
  out
}

###############################################################################
# 3) GENERATOR: HDF5 -> (top half raster + bottom half raster) -> merge -> GeoTIFF
###############################################################################
generate_snow_metric_rasters <- function(
    swe_h5_path,
    snow_metric_function,
    snow_metric_name,
    layer_names = NULL,
    dem_crs = NULL,
    extent_vec = NULL,
    out_base_dir = NULL,
    write_opts = NULL,
    ...
) {
  if (!file.exists(swe_h5_path)) stop("Missing HDF5 file: ", swe_h5_path)
  if (is.null(dem_crs)) stop("dem_crs must be provided.")
  if (is.null(extent_vec) || length(extent_vec) != 4) stop("extent_vec must be length-4 (xmin,xmax,ymin,ymax).")
  if (is.null(out_base_dir)) stop("out_base_dir must be provided.")
  if (is.null(write_opts)) write_opts <- list()
  
  # Locate SWE dataset + parse dims robustly (e.g. "6601 x 5701 x 365")
  test <- rhdf5::h5ls(swe_h5_path)
  swe_row <- which(test$name == "SWE" & test$group == "/")
  if (length(swe_row) == 0) swe_row <- which(test$name == "SWE")[1]
  if (length(swe_row) == 0 || is.na(swe_row)) stop("Could not find /SWE dataset in: ", swe_h5_path)
  
  dims  <- test$dim[swe_row]
  parts <- trimws(strsplit(dims, "x")[[1]])
  if (length(parts) < 3) stop("Unexpected HDF5 dims string: ", dims)
  
  nrow <- as.integer(parts[1])
  ncol <- as.integer(parts[2])
  nday <- as.integer(parts[3])
  if (any(is.na(c(nrow, ncol, nday))) || nday < 300) stop("Bad parsed dims from: ", dims)
  
  # Split rows into two halves
  mid   <- floor(nrow / 2)
  rows1 <- 1:mid
  rows2 <- (mid + 1):nrow
  cols  <- 1:ncol
  days  <- 1:nday
  
  # Compute split extents (terra rows are top->bottom; y decreases from ymax to ymin)
  xmin <- extent_vec[1]; xmax <- extent_vec[2]; ymin <- extent_vec[3]; ymax <- extent_vec[4]
  yres <- (ymax - ymin) / nrow
  ymid <- ymax - mid * yres
  ext1 <- terra::ext(xmin, xmax, ymid, ymax)  # top half
  ext2 <- terra::ext(xmin, xmax, ymin, ymid)  # bottom half
  
  # Output path
  name <- gsub("\\.h5$", "", basename(swe_h5_path))
  good_name <- gsub("SN_SWE", snow_metric_name, name)
  out_dir <- file.path(out_base_dir, snow_metric_name)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(out_dir, paste0(good_name, ".tif"))
  
  # ----- half 1 -----
  c1 <- rhdf5::h5read(swe_h5_path, "/SWE", index = list(rows1, cols, days))
  message(basename(swe_h5_path), " | half1 read (rows ", min(rows1), "-", max(rows1), ")")
  res1 <- apply(c1, c(1, 2), snow_metric_function, ...)
  message(basename(swe_h5_path), " | half1 metric")
  rm(c1); gc()
  
  # res1 is [K x nrow_half x ncol] -> [nrow_half x ncol x K]
  K <- 5L
  arr1 <- aperm(res1, c(2, 3, 1))
  storage.mode(arr1) <- "integer"
  r1 <- terra::rast(arr1)
  terra::ext(r1) <- ext1
  terra::crs(r1) <- dem_crs
  if (!is.null(layer_names)) names(r1) <- layer_names
  rm(res1, arr1); gc()
  
  # ----- half 2 -----
  c2 <- rhdf5::h5read(swe_h5_path, "/SWE", index = list(rows2, cols, days))
  message(basename(swe_h5_path), " | half2 read (rows ", min(rows2), "-", max(rows2), ")")
  res2 <- apply(c2, c(1, 2), snow_metric_function, ...)
  message(basename(swe_h5_path), " | half2 metric")
  rm(c2); gc()
  
  arr2 <- aperm(res2, c(2, 3, 1))
  storage.mode(arr2) <- "integer"
  r2 <- terra::rast(arr2)
  terra::ext(r2) <- ext2
  terra::crs(r2) <- dem_crs
  if (!is.null(layer_names)) names(r2) <- layer_names
  rm(res2, arr2); gc()
  
  rhdf5::h5closeAll()
  
  # Merge halves (no overlap; stacks vertically)
  r <- terra::merge(r1, r2)
  rm(r1, r2); gc()
  
  terra::writeRaster(r, out_path, overwrite = TRUE, wopt = write_opts)
  message("Wrote: ", out_path)
  out_path
}

###############################################################################
# 4) RUNNER + QUICK VALIDATION
###############################################################################
run_one_year <- function(h5_path) {
  tryCatch(
    generate_snow_metric_rasters(
      swe_h5_path = h5_path,
      snow_metric_function = accum_swe_below_thres_oct_feb,
      snow_metric_name = SNOW_METRIC_NAME,
      layer_names = LAYER_NAMES,
      dem_crs = dem_crs_wkt,
      extent_vec = EXTENT_VEC,
      out_base_dir = SNOW_METRICS_DIR,
      write_opts = WRITE_OPTS,
      swe_thres = SWE_THRESHOLD_MM
    ),
    error = function(e) {
      msg <- paste0("FAILED for ", basename(h5_path), " | ", conditionMessage(e))
      message(msg)
      structure(msg, class = "pikas_metric_error")
    }
  )
}

validate_output_quick <- function(tif_path) {
  if (!file.exists(tif_path)) stop("Missing output tif: ", tif_path)
  r <- terra::rast(tif_path)
  message("Output raster: ", tif_path)
  message("  nlyr: ", terra::nlyr(r))
  message("  names: ", paste(names(r), collapse = ", "))
  ex <- terra::ext(r)
  message("  extent: ", paste(c(ex[1], ex[2], ex[3], ex[4]), collapse = ", "))
  print(terra::global(r, fun = range, na.rm = TRUE))
  invisible(r)
}

###############################################################################
# 5) EXECUTION: SMOKE TEST + BATCH PROCESSORS (SEQUENTIAL + PARALLEL)
###############################################################################
message("Running metric: ", SNOW_METRIC_NAME)
message("Threshold (mm): ", SWE_THRESHOLD_MM)
message("Parallel mode configured: ", PARALLEL_MODE, " | cores: ", NCORES)

# --- Smoke test ---
test_file <- swe_list[1]
message("\n=== SMOKE TEST: ", basename(test_file), " ===")
out_test <- run_one_year(test_file)

if (inherits(out_test, "pikas_metric_error")) {
  stop("Smoke test failed. Fix before running all years:\n", out_test)
} else {
  validate_output_quick(out_test)
  message("Smoke test finished OK.")
}

# --- Batch processor (SEQUENTIAL; safest) ---
message("\n=== BATCH RUN (SEQUENTIAL) ===")
results_seq <- vector("list", length(swe_list))
for (i in seq_along(swe_list)) {
  message("\n[", i, "/", length(swe_list), "] ", basename(swe_list[i]))
  results_seq[[i]] <- run_one_year(swe_list[i])
}
fails_seq <- vapply(results_seq, inherits, logical(1), what = "pikas_metric_error")
if (any(fails_seq)) {
  message("\n=== SEQUENTIAL FAILURES (", sum(fails_seq), "/", length(fails_seq), ") ===")
  print(results_seq[fails_seq])
} else {
  message("\nSequential run: all years completed successfully.")
}

# --- Batch processor (PARALLEL; faster, higher RAM) ---
#
# Use ONLY if you have enough memory. Start with NCORES=2 or 3 if unsure.
#
message("\n=== BATCH RUN (PARALLEL) ===")
if (PARALLEL_MODE == "psock") {
  cl <- parallel::makeCluster(NCORES)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  
  # Load libs on workers
  parallel::clusterEvalQ(cl, { library(terra); library(rhdf5) })
  
  # Export everything workers need
  parallel::clusterExport(
    cl,
    varlist = c(
      "generate_snow_metric_rasters",
      "accum_swe_below_thres_oct_feb",
      "SNOW_METRIC_NAME", "LAYER_NAMES",
      "dem_crs_wkt", "EXTENT_VEC",
      "SNOW_METRICS_DIR", "WRITE_OPTS",
      "SWE_THRESHOLD_MM", "run_one_year"
    ),
    envir = environment()
  )
  
  results_par <- pbapply::pblapply(swe_list, run_one_year, cl = cl)
} else {
  # fork mode can be less stable with GDAL/PROJ stacks; use only if you trust it
  results_par <- pbmcapply::pbmclapply(swe_list, run_one_year, mc.cores = NCORES)
}

fails_par <- vapply(results_par, inherits, logical(1), what = "pikas_metric_error")
if (any(fails_par)) {
  message("\n=== PARALLEL FAILURES (", sum(fails_par), "/", length(fails_par), ") ===")
  print(results_par[fails_par])
} else {
  message("\nParallel run: all years completed successfully.")
}
