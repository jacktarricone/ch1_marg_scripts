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

# plot(dom_stack_n_obs_27)
# plot(snsr, add = TRUE)
# plot(snsr_basins, add = TRUE)

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

mwa_list <-list.files('./rasters/snow_metrics/mwa', pattern = '.tif', full.names = TRUE)
mwa <-rast(mwa_list)
mwa

# mean
mwa_mean <-app(mwa, fun = metric_mean, cores=5)
plot(mwa_mean)
writeRaster(mwa_mean, "./rasters/snow_metric_averages/mwa_mean_v2.tif")

##############
##### max ####
##############

max_list <-list.files('./rasters/snow_metrics/max', pattern = '.tif', full.names = TRUE)
max <-rast(max_list)

# mean
max_mean <-app(max, fun = metric_mean, cores=5)
plot(max_mean)
writeRaster(max_mean, "./averages/max_mean_v2.tif")

##############
##### sdd ####
##############

sdd_list <-list.files('./sdd/years', pattern = '.tif', full.names = TRUE)
sdd <-rast(sdd_list)

# mean
sdd_mean <-app(sdd, fun = "mean", cores=5)
values(sdd_mean)[values(sdd_mean) == 0] <- NA # change no data to NA
plot(sdd_mean)
writeRaster(sdd_mean, "./averages/sdd_mean.tif")

# median
sdd_med <-app(sdd, fun = 'median', cores=5)
values(sdd_med)[values(sdd_med) == 0] <- NA # change no data to NA
plot(sdd_med)
writeRaster(sdd_med, "./averages/sdd_med.tif")

# compare
global(sdd_mean, 'mean', na.rm = TRUE)
global(sdd_med, 'mean', na.rm = TRUE)
hist(sdd_mean, breaks = 100)
hist(sdd_med, breaks = 100)



