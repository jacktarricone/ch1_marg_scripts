#!/usr/bin/env Rscript
###############################################################################
# pikas_166mm_averages_terra.R
#
# PURPOSE
#   Compute and save (terra-first) climatological averages from a set of annual
#   6-layer GeoTIFFs (Oct–Mar) produced by your pikas SWE<166mm metric script.
#
# INPUTS
#   Yearly rasters (each has 6 layers: Oct, Nov, Dec, Jan, Feb, Mar), e.g.:
#     ./rasters/snow_metrics/pikas_days_swe_lt_166mm_oct_mar/
#       pikas_days_swe_lt_166mm_oct_mar_WY1985.tif
#       ...
#       pikas_days_swe_lt_166mm_oct_mar_WY2016.tif
#
# OUTPUTS (written to)
#   ./rasters/snow_metrics_averages/pikas_166mm_averages/
#
#   1) Annual climatology raster (single-layer GeoTIFF)
#        annual_mean_days_swe_lt_166mm_octmar_WY1985_WY2016.tif
#      - For each WY file, compute per-pixel mean across the 6 months (Oct–Mar),
#        then average those annual-per-pixel means across all years.
#
#   2) Monthly climatology raster (6-layer GeoTIFF; layers Oct–Mar)
#        monthly_mean_days_swe_lt_166mm_octmar_WY1985_WY2016.tif
#      - For each month, average that month’s per-pixel values across all years.
#
# NOTES
#   - This script uses terra operations (rast/app/global) and avoids pulling large
#     arrays into base R. terra will stream from disk where possible.
#   - Values in the input rasters are "days in month with SWE < 166 mm" (counts).
###############################################################################

suppressPackageStartupMessages({
  library(terra)
})

###############################################################################
# 0) USER SETTINGS
###############################################################################

# Root working directory (matches your generator script style). If you prefer to
# run from elsewhere, set ROOT_DIR <- getwd() and adjust paths accordingly.
ROOT_DIR <- "~/ch1_margulis"
setwd(ROOT_DIR)

IN_DIR  <- "./rasters/snow_metrics/pikas_days_swe_lt_166mm_oct_mar"
OUT_DIR <- "./rasters/snow_metrics_averages/pikas_166mm_averages"

# Expected month layers (6 layers total)
MONTHS <- c("Oct","Nov","Dec","Jan","Feb","Mar")

# File pattern for yearly products
PATTERN <- "pikas_days_swe_lt_166mm_oct_mar_WY[0-9]{4}\\.tif$"

# terra options: increase temp dir or memory fraction if desired
# terraOptions(tempdir = "~/terra_temp", memfrac = 0.6)

###############################################################################
# 1) DISCOVER INPUT FILES
###############################################################################
files <- list.files(IN_DIR, pattern = PATTERN, full.names = TRUE)
if (!length(files)) stop("No input rasters found in: ", IN_DIR)

# Parse WY from filename and sort
get_wy <- function(f) as.integer(sub(".*WY([0-9]{4})\\.tif$", "\\1", basename(f)))
wys <- vapply(files, get_wy, integer(1))
ord <- order(wys)
files <- files[ord]
wys   <- wys[ord]

wy_min <- min(wys)
wy_max <- max(wys)

message("Found ", length(files), " yearly rasters (WY", wy_min, "–WY", wy_max, ").")

###############################################################################
# 2) HELPERS
###############################################################################

# Standardize layer names and ensure consistent ordering Oct..Mar.
# Your generator uses names like "days_swe_lt_166mm_Oct", etc.
standardize_layers <- function(r) {
  if (nlyr(r) != 6) stop("Expected 6 layers (Oct–Mar); got ", nlyr(r), " layers.")
  
  nms <- names(r)
  
  # Map each layer name to a month token by searching for month strings.
  mapped <- vapply(nms, function(x) {
    hit <- MONTHS[sapply(MONTHS, function(m) grepl(m, x, fixed = TRUE))]
    if (length(hit) == 1) hit else NA_character_
  }, character(1))
  
  if (anyNA(mapped)) {
    # Fallback: assume current order is already Oct..Mar
    names(r) <- MONTHS
    return(r)
  }
  
  # Reorder explicitly to Oct..Mar and rename layers
  r <- r[[match(MONTHS, mapped)]]
  names(r) <- MONTHS
  r
}

###############################################################################
# 3) PREP OUTPUT DIR
###############################################################################
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

###############################################################################
# 4) BUILD LAYER REFERENCES (READ EACH FILE ONCE)
#
# We avoid loading full rasters into memory by keeping SpatRaster "references"
# to layers on disk. terra will read blocks as needed during app().
###############################################################################

# Store per-year annual-mean rasters (single-layer) as SpatRaster references
annual_mean_layers <- vector("list", length(files))

# Store month-layer references by month (each entry becomes a stack across years)
month_layers <- setNames(vector("list", length(MONTHS)), MONTHS)
for (m in MONTHS) month_layers[[m]] <- vector("list", length(files))

for (i in seq_along(files)) {
  r <- standardize_layers(rast(files[i]))
  
  # (A) Annual mean for this WY: per-pixel mean across the 6 month layers
  # app() operates cell-wise across layers in this single SpatRaster
  annual_mean_layers[[i]] <- app(r, fun = mean, na.rm = TRUE)
  
  # (B) Month-wise layer references for monthly climatologies
  for (m in MONTHS) {
    month_layers[[m]][[i]] <- r[[m]]
  }
  
  message("Prepared references for WY", wys[i], " (", basename(files[i]), ")")
}

###############################################################################
# 5) ANNUAL CLIMATOLOGY (ONE RASTER)
#
# Stack the per-year annual means (32 layers, one per WY), then average them.
###############################################################################
annual_stack <- rast(annual_mean_layers)   # multi-layer stack across years
annual_clim  <- app(annual_stack, mean, na.rm = TRUE)
names(annual_clim) <- "annual_mean_OctMar"

annual_out <- file.path(
  OUT_DIR,
  sprintf("annual_mean_days_swe_lt_166mm_octmar_WY%d_WY%d.tif", wy_min, wy_max)
)
writeRaster(annual_clim, annual_out, overwrite = TRUE)
message("Wrote annual climatology: ", annual_out)

###############################################################################
# 6) MONTHLY CLIMATOLOGY (6-LAYER RASTER)
#
# For each month, stack that month across years and average per pixel.
###############################################################################
monthly_clim_layers <- vector("list", length(MONTHS))
names(monthly_clim_layers) <- MONTHS

for (m in MONTHS) {
  # Stack same-month layers across all years (one layer per WY)
  s <- rast(month_layers[[m]])
  
  # Pixelwise mean across years for this month
  monthly_clim_layers[[m]] <- app(s, mean, na.rm = TRUE)
  names(monthly_clim_layers[[m]]) <- m
  
  message("Computed monthly climatology for ", m)
}

monthly_clim <- rast(monthly_clim_layers)
names(monthly_clim) <- MONTHS

monthly_out <- file.path(
  OUT_DIR,
  sprintf("monthly_mean_days_swe_lt_166mm_octmar_WY%d_WY%d.tif", wy_min, wy_max)
)
writeRaster(monthly_clim, monthly_out, overwrite = TRUE)
message("Wrote monthly climatology (6 layers): ", monthly_out)

###############################################################################
# 7) DONE
###############################################################################
message("Done.")


test <-rast("/Users/jtarrico/ch1_margulis/rasters/snow_metrics_averages/pikas_166mm_averages/annual_mean_days_swe_lt_166mm_octmar_WY1985_WY2016.tif")
hist(test)
plot(test)
