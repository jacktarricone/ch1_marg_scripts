### create average snow metric rasters
# january 22nd 2022
# jack tarricone

library(terra)

# set working directory
setwd("~/ch1_margulis")

# define mean function with na.rm
metric_mean <-function(x){terra::mean(x, na.rm = TRUE)}

####################
#####  max dowy ####
####################

dowy_list <-list.files('./rasters/snow_metrics/max_swe_dowy/', pattern = '.tif', full.names = TRUE)
dowy_stack <-rast(dowy_list)
dowy_mean <-app(dowy_stack, fun = metric_mean, cores = 14)
plot(dowy_mean)
plot(snsr, add = TRUE)
# writeRaster(dowy_mean, "./rasters/snow_metric_averages/max_dowy_mean_v2.tif", overwrite = T)

# calculate number of non na obs per pixel
dowy_n_obs <-app(dowy_stack, function(x) sum(!is.na(x)))
dowy_n_obs_v2 <-subst(dowy_n_obs, 0:10, NA)
plot(dowy_n_obs_v2)
writeRaster(dowy_n_obs_v2, "./rasters/snow_metrics/n_obs/max_dowy_n_obs_10.tif")

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



