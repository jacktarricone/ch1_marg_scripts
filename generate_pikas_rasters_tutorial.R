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
# Key update (2025-12-23)
# -----------------------
# Your error:
#   "no method for coercing this S4 class to a vector"
# was caused by trying to use base::rbind() on SpatRaster objects (no method),
# which falls back to the default rbind() that attempts to coerce S4 to vector.
#
# Fix:
#   Do NOT row-bind SpatRasters.
#   Instead, stream-write the output GeoTIFF in two row blocks using:
#     writeStart() + writeValues() + writeStop()
#
# This is also more memory-efficient than building a full 3D array in RAM.
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

install_if_missing_cran(c("terra"))
install_if_missing_bioc(c("rhdf5"))

suppressPackageStartupMessages({
  library(rhdf5)
  library(terra)
  library(parallel)
})

###############################################################################
# 0) USER SETTINGS (edit these paths if Chris runs elsewhere)
###############################################################################
ROOT_DIR <- "~/ch1_margulis"
SWE_HDF_DIR <- file.path(ROOT_DIR, "swe/hdf")
STATIC_DIR  <- file.path(ROOT_DIR, "rasters/static")
SNOW_METRICS_DIR <- file.path(ROOT_DIR, "rasters/snow_metrics")

# Metric settings
SWE_THRESHOLD_MM <- 200
SNOW_METRIC_NAME <- paste0("pikas_days_swe_lt_", SWE_THRESHOLD_MM, "mm_oct_feb")
LAYER_NAMES <- paste0(
  "days_swe_lt_", SWE_THRESHOLD_MM, "mm_",
  c("Oct", "Nov", "Dec", "Jan", "Feb")
)

# Extent handling:
# Safer default: use DEM extent (matches grid). If you are CERTAIN your raster is
# lon/lat and want to hard-code extent, set USE_DEM_EXTENT <- FALSE.
USE_DEM_EXTENT <- TRUE
EXTENT_OVERRIDE <- c(-123.3, -117.6, 35.4, 42)  # only used if USE_DEM_EXTENT=FALSE

###############################################################################
# 1) INPUT DISCOVERY + CRS/EXTENT (from DEM)
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
message("DEM extent: ", paste(dem_extent[1], dem_extent[2], dem_extent[3], dem_extent[4], sep = ", "))

EXTENT_VEC <- if (USE_DEM_EXTENT) c(dem_extent[1], dem_extent[2], dem_extent[3], dem_extent[4]) else EXTENT_OVERRIDE
message("OUTPUT extent set to: ", paste(EXTENT_VEC, collapse = ", "))

###############################################################################
# 2) METRIC FUNCTION: DAYS SWE < THRESHOLD PER MONTH (Oct–Feb)
###############################################################################
accum_swe_below_thres_oct_feb <- function(x, swe_thres = 200) {
  x <- as.numeric(x)
  x[x == -32768] <- NA
  if (all(is.na(x))) return(rep(NA_integer_, 5))
  
  # We only need correct month boundaries for Oct–Feb. Use a leap vs non-leap
  # reference year solely to make Feb have 29 vs 28 days when length==366.
  start_date <- if (length(x) == 366) as.Date("2003-10-01") else as.Date("2001-10-01")
  dates <- seq.Date(start_date, by = "day", length.out = length(x))
  month_num <- as.integer(format(dates, "%m"))
  
  months <- c(10, 11, 12, 1, 2)  # Oct–Feb
  out <- integer(length(months))
  
  for (i in seq_along(months)) {
    v <- x[month_num == months[i]]
    out[i] <- if (all(is.na(v))) NA_integer_ else as.integer(sum(v < swe_thres, na.rm = TRUE))
  }
  out
}

###############################################################################
# 3) RASTER GENERATOR (stream-write output in two row blocks)
###############################################################################
generate_snow_metric_rasters <- function(
    swe_h5_path,
    snow_metric_function,
    snow_metric_name,
    layer_names = NULL,
    dem_crs = NULL,
    extent_vec = NULL,
    out_base_dir = NULL,
    ...
) {
  if (!file.exists(swe_h5_path)) stop("Missing HDF5 file: ", swe_h5_path)
  if (is.null(dem_crs)) stop("dem_crs (WKT) must be provided.")
  if (is.null(extent_vec) || length(extent_vec) != 4) {
    stop("extent_vec must be length-4 (xmin,xmax,ymin,ymax).")
  }
  if (is.null(out_base_dir)) stop("out_base_dir must be provided.")
  
  # Find SWE dataset + parse dims robustly (e.g. "6601 x 5701 x 365")
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
  
  if (any(is.na(c(nrow, ncol, nday))) || nday < 300) {
    stop("Could not parse valid nrow/ncol/nday from HDF5 dims: ", dims)
  }
  
  # Split rows into two halves (robust, not hard-coded)
  mid   <- floor(nrow / 2)
  rows1 <- 1:mid
  rows2 <- (mid + 1):nrow
  cols  <- 1:ncol
  days  <- 1:nday
  
  # Output path
  name <- gsub("\\.h5$", "", basename(swe_h5_path))
  good_name <- gsub("SN_SWE", snow_metric_name, name)
  
  out_dir <- file.path(out_base_dir, snow_metric_name)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_path <- file.path(out_dir, paste0(good_name, ".tif"))
  
  # Prepare an empty on-disk raster with correct geometry and 5 layers
  K <- 5L
  r_out <- terra::rast(nrows = nrow, ncols = ncol, nlyrs = K)
  terra::ext(r_out) <- extent_vec
  terra::crs(r_out) <- dem_crs
  if (!is.null(layer_names)) terra::names(r_out) <- layer_names
  
  # Write options: small ints + explicit nodata.
  # NOTE: day counts fit in INT2S easily (0..31). NAflag is stored as NoData.
  wopt <- list(datatype = "INT2S", NAflag = -32768)
  
  r_out <- terra::writeStart(r_out, filename = out_path, overwrite = TRUE, wopt = wopt)
  
  # ---- half 1: read, compute, write ----
  c1 <- rhdf5::h5read(swe_h5_path, "/SWE", index = list(rows1, cols, days))
  message(basename(swe_h5_path), " | half1 read (rows ", min(rows1), "-", max(rows1), ")")
  
  res_c1 <- apply(c1, c(1, 2), snow_metric_function, ...)
  message(basename(swe_h5_path), " | half1 metric")
  rm(c1); gc()
  
  # res_c1 is [K x nrow_half x ncol]. Convert to (ncell_half x K) in terra cell order.
  # Terra expects values ordered by row (top->bottom) and within each row by column (left->right).
  nr1 <- length(rows1)
  vals1 <- matrix(NA_integer_, nrow = nr1 * ncol, ncol = K)
  for (k in 1:K) {
    # res_c1[k,,] is [nrow_half x ncol]. Use t() then as.vector for row-major order.
    vals1[, k] <- as.integer(as.vector(t(res_c1[k, , ])))
  }
  rm(res_c1); gc()
  
  r_out <- terra::writeValues(r_out, vals1, start = 1)
  rm(vals1); gc()
  
  # ---- half 2: read, compute, write ----
  c2 <- rhdf5::h5read(swe_h5_path, "/SWE", index = list(rows2, cols, days))
  message(basename(swe_h5_path), " | half2 read (rows ", min(rows2), "-", max(rows2), ")")
  
  res_c2 <- apply(c2, c(1, 2), snow_metric_function, ...)
  message(basename(swe_h5_path), " | half2 metric")
  rm(c2); gc()
  
  nr2 <- length(rows2)
  vals2 <- matrix(NA_integer_, nrow = nr2 * ncol, ncol = K)
  for (k in 1:K) {
    vals2[, k] <- as.integer(as.vector(t(res_c2[k, , ])))
  }
  rm(res_c2); gc()
  
  # start row for second half is mid+1
  r_out <- terra::writeValues(r_out, vals2, start = mid + 1)
  rm(vals2); gc()
  
  r_out <- terra::writeStop(r_out)
  rhdf5::h5closeAll()
  
  message("Wrote: ", out_path)
  out_path
}

###############################################################################
# 4) RUNNERS + QUICK VALIDATION
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
  message("  names: ", paste(terra::names(r), collapse = ", "))
  message("  extent: ", paste(c(terra::ext(r)[1], terra::ext(r)[2], terra::ext(r)[3], terra::ext(r)[4]), collapse = ", "))
  print(terra::global(r, fun = range, na.rm = TRUE))
  invisible(r)
}

###############################################################################
# 5) EXECUTION
###############################################################################

# ---- (A) Smoke test first file ----
test_file <- swe_list[1]
message("\n=== SMOKE TEST: ", basename(test_file), " ===")
out_test <- run_one_year(test_file)

if (inherits(out_test, "pikas_metric_error")) {
  stop("Smoke test failed. Fix before running all years:\n", out_test)
} else {
  validate_output_quick(out_test)
  message("Smoke test finished OK.")
}

# ---- (B) Batch run all years (sequential; safest) ----
message("\n=== BATCH RUN (SEQUENTIAL) ===")
results <- vector("list", length(swe_list))
for (i in seq_along(swe_list)) {
  message("\n[", i, "/", length(swe_list), "] ", basename(swe_list[i]))
  results[[i]] <- run_one_year(swe_list[i])
}

fails <- vapply(results, inherits, logical(1), what = "pikas_metric_error")
if (any(fails)) {
  message("\n=== FAILURES (", sum(fails), "/", length(fails), ") ===")
  print(results[fails])
} else {
  message("\nAll years completed successfully.")
}
