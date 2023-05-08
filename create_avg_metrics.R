### create average snow metric rasters
# january 22nd 2022
# jack tarricone

library(terra)

# set working directory
setwd("~/ch1_margulis")

# define mean function with na.rm
metric_mean <-function(x){terra::mean(x, na.rm = TRUE)}

# bring in shape files
snsr <-vect("./vectors/snsr_shp.gpkg")
snsr_basins <-vect("./vectors/ca_basins/snsr_all_basins.shp")

##############
##### max ####
##############

# load in stack
max_paths <-list.files("./rasters/snow_metrics/max_swe/years/", pattern = ".tif", full.names = TRUE)
max_stack_v1 <-rast(max_paths)

# make values less than 1 inch (25.4 mm) = NA
max_stack_v2 <-subst(max_stack_v1, 0:25.4, NA)

# calculate number of non na obs per pixel
max_stack_n_obs <-app(max_stack_v2, function(x) sum(!is.na(x)))

# max all time series pixels that don't have 90% of obs (29 years)
max_stack_n_obs_27 <-subst(max_stack_n_obs, 0:27, NA)

# mask max stack for pixels that only have 29 obs
max_stack <-mask(max_stack_v2, max_stack_n_obs_27)
# writeRaster(max_stack, "./rasters/snow_metrics/max_swe/max_stack_f_25mm_27obs.tif")

# calculate average
max_mean <-app(max_stack, fun = metric_mean, cores=14)
plot(max_mean)

# save
#writeRaster(max_mean, "./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif")

max_mean <-rast("./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif")

####################
####### fm #########
####################

# load in stack
fm_paths <-list.files("./rasters/snow_metrics/fm_apr1/", pattern = ".tif", full.names = TRUE)
fm_stack_v1 <-rast(fm_paths)

# mask with max
fm_stack_v2 <-mask(fm_stack_v1, max_mean)
# writeRaster(fm_stack_v2, "./rasters/snow_metrics/fm_apr1/fm_stack_f_25mm_27obs.tif")

# calculate average
fm_mean <-app(fm_stack_v2, fun = metric_mean, cores = 14)
plot(fm_mean)

# writeRaster(fm_mean, "./rasters/snow_metric_averages/fm_mean_f_25mm_27obs.tif")

####################
#####   dom    ####
####################

max_stack <-rast("./rasters/snow_metrics/max_swe/max_stack_f_25mm_27obs.tif")

# load in stack
dom_paths <-list.files("./rasters/snow_metrics/max_swe_dowy/", pattern = ".tif", full.names = TRUE)
dom_stack_v1 <-rast(dom_paths)

# make values less than 1 inch (25.4 mm) = NA
dom_stack_v2 <-mask(dom_stack_v1, max_stack_v2)

# calculate number of non na obs per pixel
dom_stack_n_obs <-app(dom_stack_v2, function(x) sum(!is.na(x)))

# dom all time series pixels that don't have 90% of obs (29 years)
dom_stack_n_obs_27 <-subst(dom_stack_n_obs, 0:27, NA)

# mask dom stack for pixels that only have 29 obs
dom_stack <-mask(dom_stack_v1, max_stack)
# writeRaster(dom_stack, "./rasters/snow_metrics/max_swe_dowy/dom_stack_f_25mm_27obs.tif")

# calculate average
dom_mean <-app(dom_stack, fun = metric_mean, cores=14)
plot(dom_mean)

# save
writeRaster(dom_mean, "./rasters/snow_metric_averages/dom_mean_f_25mm_27obs.tif")

##############
##### mwa ####
##############

dom_stack <-rast("./rasters/snow_metrics/max_swe_dowy/dom_stack_f_25mm_27obs.tif")

mwa_list <-list.files('./rasters/snow_metrics/mwa_djfm_total/', pattern = '.tif', full.names = TRUE)
mwa_stack <-rast(mwa_list)
mwa_stack

# make values less than 1 inch (25.4 mm) = NA
mwa_stack_v2 <-mask(mwa_stack, dom_stack)
plot(mwa_stack_v2[[8]])

# calculate number of non na obs per pixel
mwa_stack_n_obs <-app(mwa_stack_v2, function(x) sum(!is.na(x)), cores = 14)

# dom all time series pixels that don't have 90% of obs (29 years)
mwa_stack_n_obs_27 <-subst(mwa_stack_n_obs, 0:27, NA)

# calculate average
mwa_mean <-app(mwa_stack_v2, fun = metric_mean, cores = 14)
plot(mwa_mean)
writeRaster(mwa_mean, "./rasters/snow_metric_averages/mwa_djfm_v1.tif")

##############
##### fm ####
##############

dom_stack <-rast("./rasters/snow_metrics/max_swe_dowy/dom_stack_f_25mm_27obs.tif")

fm_list <-list.files('./rasters/snow_metrics/fm_apr1/', pattern = '.tif', full.names = TRUE)
fm_stack <-rast(fm_list)
fm_stack

# make values less than 1 inch (25.4 mm) = NA
fm_stack_v2 <-mask(fm_stack, dom_stack)
plot(fm_stack_v2[[8]])

# calculate number of non na obs per pixel
# fm_stack_n_obs <-app(fm_stack_v2, function(x) sum(!is.na(x)), cores = 14)

# dom all time series pixels that don't have 90% of obs (29 years)
# fm_stack_n_obs_27 <-subst(fm_stack_n_obs, 0:27, NA)

# calculate average
fm_mean_v1 <-app(fm_stack, fun = metric_mean, cores = 14)
fm_mean <-mask(fm_mean_v1, dom_stack[[1]])
plot(fm_mean)
plot(fm_mean_v1)

writeRaster(fm_mean, "./rasters/snow_metric_averages/fm_mean_v1.tif")
