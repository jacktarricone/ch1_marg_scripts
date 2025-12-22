#!/usr/bin/env Rscript
###############################################################################
# generate_pikas_rasters_tutorial.R
#
# Purpose
# -------
# This script generates the “accumulation-season SWE threshold metric”
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
# Context / why this is useful
# ----------------------------
# For pika / small-mammal analyses, a common “hiemal threshold” is ~15–20 cm
# snow depth; we’re using SWE here because the Margulis SWE product is SWE (mm).
# This monthly “days below threshold” metric captures how often conditions are
# *not* above the threshold during the accumulation season (Oct–Feb), and can be
# used as a covariate in habitat / survival models.
#
# Data assumptions
# ----------------
# 1) Input is a set of annual HDF5 files (one per water year) with a dataset
#    "/SWE" shaped [rows x cols x days], matching the Margulis et al. CA domain.
# 2) The daily axis starts on Oct 1 and runs through Sep 30 of that water year.
# 3) SWE units are millimeters (mm). (So 20 cm SWE = 200 mm.)
# 4) Missing/no-data values use -32768 (common in these cubes).
#
# Performance / how it runs
# -------------------------
# These cubes are big. To keep RAM reasonable, we:
#   - Read the cube in two latitude “halves” (north and south).
#   - Apply the metric pixel-wise on each half using base::apply() across the
#     day dimension.
#   - Bind halves back together and write to disk.
#
# Parallelism
# -----------
# The script parallelizes over YEARS (HDF5 files) using pbmcapply::pbmclapply.
# This uses forked processes and works on macOS/Linux. On Windows, fork-based
# parallelism won’t work; you can run with mc.cores = 1 (single-thread) or swap
# to a PSOCK cluster approach.
#
# Author / provenance
# -------------------
# Jack Tarricone (adapted from your generate_pikas_rasters.R + snow_metric_functions.R)
# Updated: 2025-12-22
###############################################################################


###############################################################################
# Package installation (auto-install if missing)
#
# Why this exists:
#   Chris (or any collaborator) may run this on a clean R install or on a machine
#   without the needed packages. This block checks for packages and installs
#   them as needed.
#
# Notes:
#   - 'rhdf5' is a Bioconductor package, so we install it via BiocManager.
#   - The rest are CRAN packages.
#   - If you're behind a proxy / using a locked-down environment, you may need
#     to set CRAN repos or configure your network settings.
###############################################################################

# Choose a CRAN mirror explicitly for reproducibility (you can change this)
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
install_if_missing_cran(c("terra", "pbmcapply"))

# 'parallel' is part of base R, but we keep it in library() for clarity.
# Bioconductor package used in this script
install_if_missing_bioc(c("rhdf5"))


suppressPackageStartupMessages({
  library(rhdf5)      # HDF5 I/O
  library(terra)      # raster handling + GeoTIFF writing
  library(parallel)   # detectCores
  library(pbmcapply)  # progress-bar multi-core lapply (Unix-like OS)
})

###############################################################################
# 0) USER SETTINGS (edit these paths if Chris runs elsewhere)
###############################################################################

# Project root folder (matches your existing scripts)
ROOT_DIR <- "~/*******SET TO LOCAL FOLDER NAME********"

# Where the annual SWE HDF5 cubes live (relative to ROOT_DIR)
SWE_HDF_DIR <- file.path(ROOT_DIR, "swe/hdf")

# Where static rasters live (relative to ROOT_DIR); DEM provides CRS
STATIC_DIR <- file.path(ROOT_DIR, "rasters/static")

# Output metric folder root (relative to ROOT_DIR)
SNOW_METRICS_DIR <- file.path(ROOT_DIR, "rasters/snow_metrics")

# Threshold: 20 cm SWE = 200 mm (change to 500 for "50 cm SWE", etc.)
SWE_THRESHOLD_MM <- 200

# Name used for the output subfolder + filename substitution
# NOTE: Your prior convention replaces "SN_SWE" in the HDF filename with this string.
SNOW_METRIC_NAME <- paste0("pikas_days_swe_lt_", SWE_THRESHOLD_MM, "mm_oct_feb")

# Band names written into the GeoTIFF
LAYER_NAMES <- paste0("days_swe_lt_", SWE_THRESHOLD_MM, "mm_", c("Oct", "Nov", "Dec", "Jan", "Feb"))

# Cores to use (parallelizes over years)
# Rule of thumb: start conservative (e.g., 4–8 cores) because each worker reads
# big cubes from disk and will use substantial RAM.
NCORES <- min(5, max(1, parallel::detectCores() - 1))

###############################################################################
# 1) INPUT DISCOVERY + DEM (for CRS)
###############################################################################

setwd(ROOT_DIR)

# List HDF5 files (annual water-year cubes)
swe_list <- list.files(SWE_HDF_DIR, pattern = "\\.h5$", full.names = TRUE)
if (length(swe_list) == 0) stop("No .h5 files found in: ", SWE_HDF_DIR)
message("Found ", length(swe_list), " HDF5 water-year SWE files.")

# Load a DEM (or any georeferenced raster) to copy CRS from.
#   - Prefer a file with "dem" in the name if present
#   - Otherwise use the first .tif found
static_list <- list.files(STATIC_DIR, pattern = "\\.tif$", full.names = TRUE)
if (length(static_list) == 0) stop("No .tif static rasters found in: ", STATIC_DIR)

dem_candidates <- static_list[grepl("dem", basename(static_list), ignore.case = TRUE)]
dem_path <- if (length(dem_candidates) > 0) dem_candidates[1] else static_list[1]
dem <- rast(dem_path)
message("Using DEM/static raster for CRS: ", dem_path)

###############################################################################
# 2) METRIC FUNCTION: DAYS SWE < THRESHOLD PER MONTH (Oct–Feb)
###############################################################################

# Count days per month (Oct–Feb) where SWE < threshold.
# Returns an integer vector of length 5: Oct, Nov, Dec, Jan, Feb.
#
# Why we construct a date axis:
#   The HDF cubes are indexed by water-year day (Oct 1 = day 1).
#   We create a synthetic date sequence starting Oct 1 so we can robustly
#   select the days belonging to each month, including leap years.
#
# Handling nodata:
#   -32768 is treated as NA.
#   If all values are NA for a pixel, we return NA for all 5 months.

accum_swe_below_thres_oct_feb <- function(x, swe_thres = 200) {
  
  x <- as.numeric(x)
  x[x == -32768] <- NA
  
  if (all(is.na(x))) {
    return(rep(NA_integer_, 5))
  }
  
  # Choose a leap/non-leap template to match length(x)
  start_date <- if (length(x) == 366) as.Date("2003-10-01") else as.Date("2001-10-01")
  dates <- seq.Date(start_date, by = "day", length.out = length(x))
  month_num <- as.integer(format(dates, "%m"))
  
  months <- c(10, 11, 12, 1, 2)  # Oct–Feb
  
  out <- integer(length(months))
  for (i in seq_along(months)) {
    idx <- which(month_num == months[i])
    v <- x[idx]
    
    if (all(is.na(v))) {
      out[i] <- NA_integer_
    } else {
      out[i] <- sum(v < swe_thres, na.rm = TRUE)
    }
  }
  
  return(out)
}

###############################################################################
# 3) RASTER GENERATOR: READ HDF5 IN TWO HALVES + APPLY METRIC + WRITE GeoTIFF
###############################################################################

# This is a generalized version of your generate_snow_metric_rasters() that:
#   - Works for scalar-returning metric functions (1 layer) AND
#   - Works for vector-returning metric functions (multiple layers)
#
# For our pika metric, the function returns length 5 -> we write a 5-band GeoTIFF.
#
# NOTE:
#   - We create the output directory if it does not exist.

generate_snow_metric_rasters <- function(
    swe_h5_path,
    snow_metric_function,
    snow_metric_name,
    layer_names = NULL,
    ...
) {
  
  # Safety: ensure path exists
  if (!file.exists(swe_h5_path)) stop("Missing HDF5 file: ", swe_h5_path)
  
  # Identify SWE cube dimensions
  test <- h5ls(swe_h5_path)
  
  # Try to locate the SWE dataset row robustly
  swe_row <- which(test$name == "SWE" & test$group == "/")
  if (length(swe_row) == 0) {
    # fallback: first row with name SWE
    swe_row <- which(test$name == "SWE")[1]
  }
  if (is.na(swe_row)) stop("Could not find /SWE dataset in: ", swe_h5_path)
  
  dims <- test$dim[swe_row]
  nday <- as.integer(sub("6601 x 5701 x ", "", dims))
  
  # Define split indices (north/south) to reduce RAM
  north_rows <- 1:3300
  south_rows <- 3301:6601
  cols <- 1:5701
  days <- 1:nday
  
  # ----- NORTH HALF -----
  c1 <- h5read(swe_h5_path, "/SWE", index = list(north_rows, cols, days))
  message(basename(swe_h5_path), " | c1 read into memory")
  
  res_c1 <- apply(c1, c(1, 2), snow_metric_function, ...)
  message(basename(swe_h5_path), " | c1 metric calculated")
  rm(c1)
  
  # ----- SOUTH HALF -----
  c2 <- h5read(swe_h5_path, "/SWE", index = list(south_rows, cols, days))
  message(basename(swe_h5_path), " | c2 read into memory")
  
  res_c2 <- apply(c2, c(1, 2), snow_metric_function, ...)
  message(basename(swe_h5_path), " | c2 metric calculated")
  rm(c2)
  
  h5closeAll()
  
  # Decide whether metric is scalar (matrix) or vector (array)
  # apply() returns:
  #   - matrix [row, col] if FUN returns length 1
  #   - array  [L, row, col] if FUN returns length L > 1
  if (length(dim(res_c1)) == 2) {
    
    # scalar metric
    full_res <- rbind(as.matrix(res_c1), as.matrix(res_c2))
    r <- rast(full_res)
    rm(full_res)
    
  } else {
    
    # vector metric: convert [L, row, col] -> [row, col, L] for terra
    arr1 <- aperm(res_c1, c(2, 3, 1))
    arr2 <- aperm(res_c2, c(2, 3, 1))
    r1 <- rast(arr1)
    r2 <- rast(arr2)
    r <- rbind(r1, r2)
    
    if (!is.null(layer_names)) names(r) <- layer_names
    
    rm(arr1, arr2, r1, r2)
  }
  
  # Nodata -> NA (works for multi-layer too)
  r[r == -32768] <- NA
  
  # Georeference (extent + CRS)
  # Extent for Margulis CA domain used in your original scripts:
  ext(r) <- c(-123.3, -117.6, 35.4, 42)
  crs(r) <- crs(dem)
  
  # Output naming convention:
  # - Replace "SN_SWE" in the original filename with the metric name
  name <- gsub("\\.h5$", "", basename(swe_h5_path))
  good_name <- gsub("SN_SWE", snow_metric_name, name)
  
  # Ensure output directory exists
  out_dir <- file.path(SNOW_METRICS_DIR, snow_metric_name)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_path <- file.path(out_dir, paste0(good_name, ".tif"))
  
  writeRaster(r, out_path, overwrite = TRUE)
  
  message("Wrote: ", out_path)
  return(invisible(out_path))
}

###############################################################################
# 4) RUN THE METRIC FOR ALL YEARS (PARALLEL OVER FILES)
###############################################################################

message("Running metric: ", SNOW_METRIC_NAME)
message("Threshold (mm): ", SWE_THRESHOLD_MM)
message("Cores: ", NCORES)

# my older loop passed the INDEX (1..N) into generate_snow_metric_rasters().
# This function expects a FILE PATH. Here we pass swe_list[i].

system.time({
  out_files <- pbmclapply(
    seq_along(swe_list),
    function(i) {
      generate_snow_metric_rasters(
        swe_h5_path = swe_list[i],
        snow_metric_function = accum_swe_below_thres_oct_feb,
        snow_metric_name = SNOW_METRIC_NAME,
        layer_names = LAYER_NAMES,
        swe_thres = SWE_THRESHOLD_MM
      )
    },
    mc.cores = NCORES,
    mc.cleanup = TRUE
  )
})

message("Done. Example output file:\n", out_files[[1]])

###############################################################################
# 5) QUICK SANITY CHECK (OPTIONAL)
###############################################################################
# If you want to quickly confirm the output has 5 layers and reasonable values:
#
# r_test <- rast(out_files[[1]])
# print(r_test)
# print(names(r_test))
# global(r_test, fun = "range", na.rm = TRUE)
#
###############################################################################
