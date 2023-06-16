# format gridmet data to SNSR
# jack tarricone

library(terra)
library(ncdf4)
library(climateR)
library(sf)

setwd("~/ch1_margulis")

# load in snsr shape and dem
# with sf
snsr_v1 <-st_read("./vectors/snsr_shp.gpkg")
snsr_sf <-st_geometry(snsr_v1)
snsr <-vect("./vectors/snsr_shp.gpkg")
dem <-rast("./rasters/static/SNSR_DEM.tif")

## loop for downloading gridmet data
# and formatting to cold season values at 90 m
# not for temp vars
for (year in 1985:2016){
  
  # define variable
  var <-"rmax"
  
  # Define start and end dates
  start_date <- paste0(year - 1, "-10-01")
  end_date <- paste0(year, "-03-31")
  
  # dl data from oct 1 - march 30
  data_list <-getGridMET(AOI = snsr_sf, 
                         varname = var,
                         startDate = start_date,
                         endDate = end_date)
  
  # convert from list to rast stack
  var_stack <-rast(data_list)
  var_stack

  # calc mean values
  var_mean <-app(var_stack, 'mean')
  vm_1 <-project(resample(var_mean, dem, method = "bilinear", threads = TRUE), crs(dem))
  vm_2 <-mask(vm_1, snsr)
  plot(vm_2)

  # save
  saving_location2 <-paste0('./rasters/gridmet/',var,"/",var,"_ondjfm_",year,".tif")
  writeRaster(vm_2, saving_location2)
  print(paste0(year, " ", var, " is done!"))
}

### for temp vars
for (year in 1985:2016){
  
  # define variable
  var <-"tmmn"
  
  # Define start and end dates
  start_date <- paste0(year - 1, "-10-01")
  end_date <- paste0(year, "-03-31")
  
  # dl data from oct 1 - march 30
  data_list <-getGridMET(AOI = snsr_sf, 
                         varname = var,
                         startDate = start_date,
                         endDate = end_date)
  
  # convert from list to rast stack
  var_stack <-rast(data_list)
  var_stack
  
  # calc mean values
  var_mean <-app(var_stack, 'mean')
  vm_1 <-project(resample(var_mean, dem, method = "bilinear", threads = TRUE), crs(dem))
  vm_2 <-mask(vm_1, snsr)
  vm_3 <- vm_2 - 273.15
  plot(vm_3)
  
  # save
  saving_location2 <-paste0('./rasters/gridmet/',var,"/",var,"_ondjfm_",year,".tif")
  writeRaster(vm_3, saving_location2)
  print(paste0(year, " ", var, " is done!"))
}

############### make tmean rasters
tmmn_list <-list.files('./rasters/gridmet/tmmn', full.names = TRUE)
tmmn_stack <-rast(tmmn_list)

tmmx_list <-list.files('./rasters/gridmet/tmmx', full.names = TRUE)
tmmx_stack <-rast(tmmx_list)

# add and divide
tmean_stack <-(tmmx_stack+tmmn_stack)/2
tmean_mean <-app(tmean_stack, mean)
# save
writeRaster(tmean_stack, "./rasters/gridmet/tmean/tmean_stack.tif")
writeRaster(tmean_mean, "./rasters/gridmet/tmean/tmean_mean.tif")

# tmmn
tmmn_mean <-app(tmmn_stack, mean)
writeRaster(tmmn_mean, "./rasters/gridmet/var_means/tmmn_mean.tif")

# tmmx
tmmx_mean <-app(tmmx_stack, mean)
writeRaster(tmmx_mean, "./rasters/gridmet/var_means/tmmx_mean.tif")


############## make rmean rasters
rmin_list <-list.files('./rasters/gridmet/rmin', full.names = TRUE)
rmin_stack <-rast(rmin_list)

rmax_list <-list.files('./rasters/gridmet/rmax', full.names = TRUE)
rmax_stack <-rast(rmax_list)

# add and divide
rmean_stack <-(rmax_stack+rmin_stack)/2
rmean_mean <-app(rmean_stack, mean)
plot(rmean_mean)
hist(rmean_mean, breaks = 100)

# save
writeRaster(rmean_stack, "./rasters/gridmet/rmean/rmean_stack.tif")
writeRaster(rmean_mean, "./rasters/gridmet/var_means/rmean_mean.tif")

# others
rmax_mean <-app(rmax_stack, mean)
plot(rmax_mean)
writeRaster(rmax_mean, "./rasters/gridmet/var_means/rmax_mean.tif")

# others
rmin_mean <-app(rmin_stack, mean)
plot(rmin_mean)
writeRaster(rmin_mean, "./rasters/gridmet/var_means/rmin_mean.tif")


############## make sph_mean
sph_list <-list.files('./rasters/gridmet/sph', full.names = TRUE)
sph_stack <-rast(sph_list)

# add and divide
sph_mean <-app(sph_stack, mean)
plot(sph_mean)
hist(sph_mean, breaks = 100)

# save
writeRaster(sph_mean, "./rasters/gridmet/var_means/sph_mean.tif")

############## make srad_mean
srad_list <-list.files('./rasters/gridmet/srad', full.names = TRUE)
srad_stack <-rast(srad_list)

# add and divide
srad_mean <-app(srad_stack, mean)
plot(srad_mean)
hist(srad_mean, breaks = 100)

# save
writeRaster(srad_mean, "./rasters/gridmet/var_means/srad_mean.tif")

# calculate full dataset means
tmean_sm <-global(tmean_stack, mean, na.rm = TRUE)
colnames(tmean_sm)[1] <-"temp_c"

rmean_sm <-global(rmean_stack, mean, na.rm = TRUE)
colnames(rmean_sm)[1] <-"rh_%"

srad_sm  <-global(srad_stack, mean, na.rm = TRUE)
colnames(srad_sm)[1] <-"srad"

sph_sm  <-global(sph_stack, mean, na.rm = TRUE)
colnames(sph_sm)[1] <-"sph"

wy <-as.data.frame(1985:2016)
colnames(wy)[1] <-"wy"

# make df
means_df <-cbind(wy, tmean_sm, rmean_sm, sph_sm, srad_sm)
means_df

library(ggplot2)

ggplot(means_df, aes(x = wy, y = sph))+
  geom_line()+
  geom_point()+
  scale_x_continuous(limits = c(1985,2016))
  
