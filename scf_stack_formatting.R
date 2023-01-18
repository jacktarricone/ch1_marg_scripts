# scf stack formatting
# jack tarricone
# january 18, 2023

library(terra)

# set working dir
setwd("/Users/jacktarricone/ch1_margulis/")

### format stack
rast_list <-list.files("./snow_metric_rasters/terra_rasters/scf/", pattern = ".tif", full.names = TRUE)
scf_stack <-rast(rast_list)
values(scf_stack)[values(scf_stack) == 0] = NA
# writeRaster(scf_stack, "./snow_metric_rasters/terra_rasters/scf/scf_stack.tif")

# read in stack 
scf_stack <-rast("./snow_metric_rasters/terra_rasters/scf/scf_stack.tif")
scf_stack

# test mk code by first running it on .5 degrees lat near tahoe
american_v1 <-vect("./american_test/american.shp")
american <-project(american_v1, crs(scf_stack))

# test plot
plot(scf_stack[[6]])
plot(american, add = TRUE)

# crop and mask stack down to just american
scf_american_v1 <-crop(scf_stack, american)
scf_american <-mask(scf_american_v1, american)

# save
# writeRaster(scf_american, "./snow_metric_rasters/terra_rasters/scf/scf_american.tif")
