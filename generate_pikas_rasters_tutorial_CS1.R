#!/usr/bin/env Rscript
###############################################################################
# generate_pikas_rasters.R
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
  library(rhdf5)
  library(terra)
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

SWE_THRESHOLD_MM <- 166
SNOW_METRIC_NAME <- paste0("pikas_days_swe_lt_", SWE_THRESHOLD_MM, "mm_oct_mar")
LAYER_NAMES <- paste0(
  "days_swe_lt_", SWE_THRESHOLD_MM, "mm_",
  c("Oct", "Nov", "Dec", "Jan", "Feb", "Mar")
)

EXTENT_CA <- c(-123.3, -117.6, 35.4, 42)

PARALLEL_MODE <- "psock"  # "psock" or "fork"
NCORES <- min(2, max(1, parallel::detectCores() - 1))

###############################################################################
# 1) INPUT DISCOVERY + CRS
###############################################################################
setwd(ROOT_DIR)

swe_list <- list.files(SWE_HDF_DIR, pattern = "\\.h5$", full.names = TRUE)
if (length(swe_list) == 0) stop("No .h5 files found in: ", SWE_HDF_DIR)
message("Found ", length(swe_list), " HDF5 water-year SWE files.")

static_list <- list.files(STATIC_DIR, pattern = "\\.tif$", full.names = TRUE)
if (length(static_list) == 0) stop("No .tif static rasters found in: ", STATIC_DIR)

dem_candidates <- static_list[grepl("dem", basename(static_list), ignore.case = TRUE)]
dem_path <- if (length(dem_candidates) > 0) dem_candidates[1] else static_list[1]
dem <- rast(dem_path)
dem_crs_wkt <- crs(dem)
message("Using DEM/static raster for CRS: ", dem_path)

###############################################################################
# 2) HELPERS + METRIC
###############################################################################
wy_start_date_from_path <- function(path) {
  wy <- as.integer(sub(".*WY([0-9]{4}).*", "\\1", basename(path)))
  if (is.na(wy)) stop("Could not parse WY from filename: ", basename(path))
  as.Date(sprintf("%d-10-01", wy - 1))
}

accum_swe_below_thres_oct_mar <- function(x, start_date, swe_thres) {
  x <- as.numeric(x)
  x[x == -32768] <- NA_real_
  
  # ALWAYS length-6 return
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
# 3) MAIN WORKER
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
  if (is.null(extent_vec) || length(extent_vec) != 4) stop("extent_vec must be length-4.")
  
  test <- rhdf5::h5ls(swe_h5_path)
  swe_row <- which(test$name == "SWE" & test$group == "/")
  if (length(swe_row) == 0) swe_row <- which(test$name == "SWE")[1]
  if (length(swe_row) == 0 || is.na(swe_row)) stop("Could not find /SWE dataset in: ", swe_h5_path)
  
  dims_chr <- as.character(test$dim[swe_row])
  parts <- as.integer(trimws(unlist(strsplit(dims_chr, "x"))))
  if (length(parts) < 3 || anyNA(parts)) stop("Could not parse dims: ", dims_chr)
  
  nrow_ <- parts[1]
  ncol_ <- parts[2]
  nday  <- parts[3]
  if (nday < 300) stop("Parsed nday looks wrong: ", nday, " from dims: ", dims_chr)
  
  message(basename(swe_h5_path), " | Parsed dims: ", nrow_, " x ", ncol_, " x ", nday)
  
  split_row <- floor(nrow_ / 2)
  north_rows <- 1:split_row
  south_rows <- (split_row + 1):nrow_
  cols <- 1:ncol_
  days <- 1:nday
  
  start_date <- wy_start_date_from_path(swe_h5_path)
  
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
  
  c2 <- rhdf5::h5read(swe_h5_path, "/SWE", index = list(south_rows, cols, days))
  message(basename(swe_h5_path), " | c2 read (rows ", split_row + 1, "-", nrow_, ")")
  res_c2 <- apply(
    c2, c(1, 2),
    snow_metric_function,
    start_date = start_date,
    swe_thres = swe_thres
  )
  message(basename(swe_h5_path), " | c2 metric")
  rm(c2)
  
  rhdf5::h5closeAll()
  
  if (length(dim(res_c1)) != 3) {
    stop("Unexpected metric output shape for ", basename(swe_h5_path),
         ". Expected 3D array [K, rows, cols].")
  }
  if (!is.null(layer_names) && dim(res_c1)[1] != length(layer_names)) {
    stop("Metric returned ", dim(res_c1)[1], " layers but layer_names has ", length(layer_names))
  }
  
  arr1 <- aperm(res_c1, c(2, 3, 1))  # [rows, cols, K]
  arr2 <- aperm(res_c2, c(2, 3, 1))
  
  k <- dim(arr1)[3]
  full_arr <- array(NA_integer_, dim = c(nrow_, ncol_, k))
  full_arr[1:split_row, , ] <- arr1
  full_arr[(split_row + 1):nrow_, , ] <- arr2
  
  r <- rast(full_arr)
  rm(full_arr, arr1, arr2, res_c1, res_c2)
  
  # IMPORTANT: do NOT namespace-qualify replacement methods on your terra version
  if (!is.null(layer_names)) names(r) <- layer_names
  ext(r) <- extent_vec
  crs(r) <- dem_crs
  
  name <- gsub("\\.h5$", "", basename(swe_h5_path))
  good_name <- gsub("SN_SWE", snow_metric_name, name)
  
  out_dir <- file.path(SNOW_METRICS_DIR, snow_metric_name)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  out_path <- file.path(out_dir, paste0(good_name, ".tif"))
  writeRaster(r, out_path, overwrite = TRUE)
  message("Wrote: ", out_path)
  
  out_path
}

###############################################################################
# 4) RUNNERS
###############################################################################
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

run_all_years <- function(paths) {
  if (PARALLEL_MODE == "fork" && .Platform$OS.type == "unix") {
    pbmcapply::pbmclapply(paths, run_one_year, mc.cores = NCORES)
  } else if (PARALLEL_MODE == "psock") {
    cl <- parallel::makeCluster(NCORES)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    parallel::clusterExport(
      cl,
      varlist = c(
        "generate_snow_metric_rasters",
        "accum_swe_below_thres_oct_mar",
        "wy_start_date_from_path",
        "SNOW_METRICS_DIR",
        "SNOW_METRIC_NAME",
        "LAYER_NAMES",
        "dem_crs_wkt",
        "EXTENT_CA",
        "SWE_THRESHOLD_MM",
        "run_one_year"
      ),
      envir = environment()
    )
    
    parallel::clusterEvalQ(cl, {
      suppressPackageStartupMessages({
        library(rhdf5)
        library(terra)
      })
      NULL
    })
    
    pbapply::pblapply(paths, run_one_year, cl = cl)
  } else {
    lapply(paths, run_one_year)
  }
}

###############################################################################
# 5) ENTRYPOINT
###############################################################################
message("Running metric: ", SNOW_METRIC_NAME)
message("Threshold (mm): ", SWE_THRESHOLD_MM)
message("Parallel mode: ", PARALLEL_MODE, " | cores: ", NCORES)

# --- Smoke test ---
lapply(swe_list, run_one_year)


# --- Full run ---
# out_files <- run_all_years(swe_list)
# 
# is_fail <- vapply(
#   out_files,
#   function(x) inherits(x, "pikas_metric_error") || (is.character(x) && grepl("^FAILED", x)),
#   logical(1)
# )
# 
# message("Finished. Successful years: ", sum(!is_fail), " | Failed years: ", sum(is_fail))
# 
# if (any(!is_fail)) {
#   message("Example output file:\n", out_files[[which(!is_fail)[1]]])
# } else if (length(out_files) > 0) {
#   message("No successful outputs yet. First failure:\n", out_files[[1]])
# }

