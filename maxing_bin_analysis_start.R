# binning analysis test

library(terra)

setwd("~/ch1_margulis")

# bring in american shp
american <-vect("./vectors/ca_basins/american.gpkg")
plot(american)

# max swe 93, 16, 16
max93 <-rast("./rasters/snow_metrics/max_swe/years/max_swe_WY1993.tif")
max15 <-rast("./rasters/snow_metrics/max_swe/years/max_swe_WY2015.tif")
max16 <-rast("./rasters/snow_metrics/max_swe/years/max_swe_WY2016.tif")
max_stack <-rast("./rasters/snow_metrics/max_swe/max_american.tif")

# bring in aspect
bins <-rast("./rasters/categorized/american_dem_aspect_classified.tif")

# crop and mask
max93_v2 <-crop(max93, ext(bins))
max93_v3 <-mask(max93_v2, bins)
plot(max93_v3)

# # pull out bins
# ez1_n <-mask(max93_v3, bins, maskvalue = 1, inverse = TRUE)
# ez1_s <-mask(max93_v3, bins, maskvalue = 2, inverse = TRUE)
# ez2_n <-mask(max93_v3, bins, maskvalue = 3, inverse = TRUE)
# ez2_s <-mask(max93_v3, bins, maskvalue = 4, inverse = TRUE)
# ez3_n <-mask(max93_v3, bins, maskvalue = 5, inverse = TRUE)
# ez3_s <-mask(max93_v3, bins, maskvalue = 6, inverse = TRUE)
# 
# # stack bings for analysis
# bin_stack <-c(ez1_n, ez1_s, ez2_s, ez2_n, ez3_s, ez3_n)

# pull out bins
ez1_n <-mask(max_stack, bins, maskvalue = 1, inverse = TRUE)
ez1_s <-mask(max_stack, bins, maskvalue = 2, inverse = TRUE)
ez2_n <-mask(max_stack, bins, maskvalue = 3, inverse = TRUE)
ez2_s <-mask(max_stack, bins, maskvalue = 4, inverse = TRUE)
ez3_n <-mask(max_stack, bins, maskvalue = 5, inverse = TRUE)
ez3_s <-mask(max_stack, bins, maskvalue = 6, inverse = TRUE)

# stack bings for analysis
bin_stack <-c(ez1_n, ez1_s, ez2_s, ez2_n, ez3_s, ez3_n)
zone <-rep(c("ez1_n", "ez1_s", "ez2_s", "ez2_n", "ez3_s", "ez3_n"), each = 32)
year <-rep(1985:2016, 6)

# calc?
bin_means <-global(bin_stack, "mean", na.rm = TRUE)
bin_means

# bind
df <-cbind(bin_means, zone, year)
df

# calc?
test <-global(max_stack, "mean", na.rm = TRUE)
