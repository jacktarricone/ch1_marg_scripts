#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(terra))

###############################################################################
# SETTINGS
###############################################################################
IN_DIR <- "/Users/jtarrico/ch1_margulis/rasters/snow_metrics/pikas_days_swe_lt_166mm_oct_mar"
pattern <- "pikas_days_swe_lt_166mm_oct_mar_WY[0-9]{4}\\.tif$"

# threshold in DAYS (values are day-counts per month)
DAYS_THRESHOLD <- 0

# months present in each WY file
MONTHS <- c("Oct","Nov","Dec","Jan","Feb","Mar")

OUT_WY  <- file.path(IN_DIR, sprintf("wy_means_aboveDays_%d_terra.csv", DAYS_THRESHOLD))
OUT_MON <- file.path(IN_DIR, sprintf("monthly_climatology_aboveDays_%d_terra.csv", DAYS_THRESHOLD))

###############################################################################
# INPUT DISCOVERY
###############################################################################
files <- list.files(IN_DIR, pattern = pattern, full.names = TRUE)
if (!length(files)) stop("No files found in: ", IN_DIR)

get_wy <- function(f) as.integer(sub(".*WY([0-9]{4})\\.tif$", "\\1", basename(f)))
wys <- vapply(files, get_wy, integer(1))
ord <- order(wys)
files <- files[ord]
wys   <- wys[ord]

message("Found ", length(files), " rasters (WY ", min(wys), "–", max(wys), ")")

###############################################################################
# HELPERS
###############################################################################

# Standardize names + order to Oct..Mar (handles your existing "days_swe_lt_166mm_Oct" style)
standardize_layers <- function(r) {
  if (nlyr(r) != 6) stop("Expected 6 layers (Oct–Mar), got: ", nlyr(r))
  
  nms <- names(r)
  # map each layer to a month token
  mapped <- vapply(nms, function(x) {
    hit <- MONTHS[sapply(MONTHS, function(m) grepl(m, x, fixed = TRUE))]
    if (length(hit) == 1) hit else NA_character_
  }, character(1))
  
  if (anyNA(mapped)) {
    # fallback: assume already in Oct..Mar order
    names(r) <- MONTHS
    return(r)
  }
  
  r <- r[[match(MONTHS, mapped)]]
  names(r) <- MONTHS
  r
}

# mean of values > threshold (per layer) using terra ops
# ifel keeps values > thr, sets rest to NA, then global(mean)
mean_above <- function(r, thr) {
  r2 <- ifel(r > thr, r, NA)
  # returns a numeric vector with one value per layer
  as.numeric(global(r2, "mean", na.rm = TRUE)[,1])
}

###############################################################################
# (A) PER-WY: mean above threshold for each month layer
###############################################################################
wy_tbl <- do.call(
  rbind,
  lapply(seq_along(files), function(i) {
    r <- standardize_layers(rast(files[i]))
    vals <- mean_above(r, DAYS_THRESHOLD)
    names(vals) <- names(r)
    
    data.frame(
      WY = wys[i],
      Oct = vals["Oct"], Nov = vals["Nov"], Dec = vals["Dec"],
      Jan = vals["Jan"], Feb = vals["Feb"], Mar = vals["Mar"],
      OctMar_mean = mean(vals, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
)

write.csv(wy_tbl, OUT_WY, row.names = FALSE)
message("Wrote: ", OUT_WY)

###############################################################################
# (B) MONTHLY CLIMATOLOGY across years (terra-native)
#
# Two equivalent ways:
#   B1) stack same-month layers across years and app(mean)
#   B2) compute per-year spatial means then average (lighter on RAM)
#
# Below is B2 (usually more memory-friendly), still using terra ops for the core.
###############################################################################
mon_tbl <- data.frame(Month = MONTHS, MeanAcrossYears = NA_real_, N_years_nonNA = NA_integer_)

# compute per-year spatial mean (above thr) for each month using terra global()
per_year_month_means <- matrix(NA_real_, nrow = length(files), ncol = length(MONTHS),
                               dimnames = list(wys, MONTHS))

for (i in seq_along(files)) {
  r <- standardize_layers(rast(files[i]))
  r2 <- ifel(r > DAYS_THRESHOLD, r, NA)            # terra op
  g  <- global(r2, "mean", na.rm = TRUE)[,1]       # terra op, one per layer
  per_year_month_means[i, ] <- as.numeric(g)
}

for (m in MONTHS) {
  v <- per_year_month_means[, m]
  mon_tbl$MeanAcrossYears[mon_tbl$Month == m] <- mean(v, na.rm = TRUE)
  mon_tbl$N_years_nonNA[mon_tbl$Month == m]  <- sum(!is.na(v))
}

write.csv(mon_tbl, OUT_MON, row.names = FALSE)
message("Wrote: ", OUT_MON)

###############################################################################
# OPTIONAL: If you *also* want monthly climatology RASTERS (pixelwise mean across years)
# Uncomment this block. This is more RAM/IO heavy but fully raster-native.
###############################################################################
# OUT_DIR_CLIM <- file.path(IN_DIR, sprintf("climatology_rasters_aboveDays_%d", DAYS_THRESHOLD))
# dir.create(OUT_DIR_CLIM, showWarnings = FALSE, recursive = TRUE)
#
# for (m in MONTHS) {
#   # build a multi-year stack of that month layer
#   r_stack <- rast(lapply(files, function(f) standardize_layers(rast(f))[[m]]))
#   r_stack <- ifel(r_stack > DAYS_THRESHOLD, r_stack, NA)
#   clim_r  <- app(r_stack, mean, na.rm = TRUE)  # pixelwise mean across years
#   names(clim_r) <- paste0(m, "_clim_mean_above_", DAYS_THRESHOLD)
#   writeRaster(clim_r, file.path(OUT_DIR_CLIM, paste0("clim_", m, ".tif")), overwrite = TRUE)
# }
# message("Wrote climatology rasters to: ", OUT_DIR_CLIM)

###############################################################################
# QUICK PRINT
###############################################################################
print(head(wy_tbl, 3))
print(mon_tbl)
