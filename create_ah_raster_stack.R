### create absolute humidity metric
# june 20th, 2023
# jack tarricone

library(terra)

# set working directory
setwd("~/ch1_margulis")

# bring in shape files
snsr <-vect("./vectors/snsr_shp.gpkg")
snsr_basins <-vect("./vectors/ca_basins/snsr_all_basins.shp")

### read in stacks
tmean_stack <-rast("./rasters/gridmet/tmean/tmean_stack.tif")
rmean_stack <-rast("./rasters/gridmet/rmean/rmean_stack.tif")

# bring in masking
sa_mask <-rast("./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif")

# define abs_hum function from the two stacks
ah_stack <-(6.112 * exp((17.67 * tmean_stack) / (tmean_stack + 243.5)) * rmean_stack * 2.1674) / (273.15 + tmean_stack)

# absolute_humidity <- function(air_temp, rh) {
#   # https://carnotcycle.wordpress.com/2012/08/04/how-to-convert-relative-humidity-to-absolute-humidity/
#   (6.112 * exp((17.67 * air_temp) / (air_temp + 243.5)) * rh * 2.1674) / (273.15 + air_temp)
# }

ah_stack_v2 <-mask(ah_stack, sa_mask, maskvalue = NA)
plot(ah_stack_v2[[31]])
writeRaster(ah_stack_v2, "./rasters/gridmet/ah/ah_stack.tif")

# define mean function with na.rm
metric_mean <-function(x){terra::mean(x, na.rm = TRUE)}

# calculate average
ah_mean <-app(ah_stack, fun = metric_mean, cores=14)
plot(ah_mean)
plot(snsr_basins, add = TRUE, lwd = .1)

# mask
ah_mean_v2 <-mask(ah_mean, sa_mask, maskvalue = NA)
plot(ah_mean_v2)
plot(snsr_basins, add = TRUE, lwd = .1)

# save
writeRaster(ah_mean_v2, "./rasters/snow_metric_averages/ah_mean_f_25mm_27obs.tif")

