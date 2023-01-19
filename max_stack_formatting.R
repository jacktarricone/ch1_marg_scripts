# max stack formatting
# jack tarricone
# january 18, 2023

library(terra)

# set working dir
setwd("/Users/jacktarricone/ch1_margulis/")

### format stack
rast_list <-list.files("./snow_metric_rasters/terra_rasters/max_swe/years", full.names = TRUE) # NO 2k16!!!
max_stack <-rast(rast_list)
plot(max_stack[[1]])
# writeRaster(max_stack, "./snow_metric_rasters/terra_rasters/max_swe/max_stack.tif")

# read in stack 
max_stack <-rast("./snow_metric_rasters/terra_rasters/max_swe/max_stack.tif")
max_stack

# test mk code by first running it on .5 degrees lat near tahoe
american_v1 <-vect("./american_test/american.shp")
american <-project(american_v1, crs(max_stack))

# test plot
plot(max_stack[[6]])
plot(american, add = TRUE)

# crop and mask stack down to just american
max_american_v1 <-crop(max_stack, american)
max_american <-mask(max_american_v1, american)

# save
writeRaster(max_american, "./snow_metric_rasters/terra_rasters/max_swe/max_american.tif")
