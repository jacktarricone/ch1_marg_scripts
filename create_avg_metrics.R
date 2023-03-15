### create average snow metric rasters
# january 22nd 2022
# jack tarricone

library(terra)

#set working directory
setwd("~/ch1_margulis/snow_metric_rasters/terra_rasters/")

##### compute metrics

####################
#####  max dowy ####
####################

dowy_list <-list.files('./snow_metric_rasters/terra_rasters/max_swe_dowy/', pattern = '.tif', full.names = TRUE)
dowy_stack <-rast(dowy_list)

# mean
dowy_mean <-app(dowy_stack, fun = "mean", cores = 8)
plot(dowy_mean)
hist(dowy_mean, breaks = 200)
# writeRaster(dowy_mean, "./rasters/snow_metric_averages/max_dowy_mean.tif")

# median
mwa_med <-app(mwa, fun = 'median', cores=5)
plot(mwa_med)
writeRaster(mwa_med, "./averages/mwa_med.tif")

##############
##### mwa ####
##############

mwa_list <-list.files('./mwa/years', pattern = '.tif', full.names = TRUE)
mwa <-rast(mwa_list)

mwa_mean <-rast('./averages/mwa_mean.tif')

# mean
mwa_mean <-app(mwa, fun = "mean", cores=5)
plot(mwa_mean)
# writeRaster(mwa_mean, "./averages/mwa_mean.tif")

# median
mwa_med <-app(mwa, fun = 'median', cores=5)
plot(mwa_med)
writeRaster(mwa_med, "./averages/mwa_med.tif")

# compare
global(mwa_mean, 'mean', na.rm = TRUE)
global(mwa_med, 'mean', na.rm = TRUE)
hist(mwa_mean, breaks = 100)
hist(mwa_med, breaks = 100)

##############
##### max ####
##############

max_list <-list.files('./max_swe/years', pattern = '.tif', full.names = TRUE)
max <-rast(max_list)

# mean
max_mean <-app(max, fun = "mean", cores=5)
plot(max_mean)
writeRaster(max_mean, "./averages/max_mean.tif")

# median
max_med <-app(max, fun = 'median', cores=5)
plot(max_med)
writeRaster(max_med, "./averages/max_med.tif")

# compare
global(max_mean, 'mean', na.rm = TRUE)
global(max_med, 'mean', na.rm = TRUE)
hist(max_mean, breaks = 100)
hist(max_med, breaks = 100)

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



