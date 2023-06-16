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
tmean_mean_stack <-app(tmean_stack, mean)
plot(tmean_mean_stack)
hist(tmean_mean_stack, breaks = 100)

# save
writeRaster(tmean_stack, "./rasters/gridmet/tmean/tmean_stack.tif")
writeRaster(tmean_mean_stack, "./rasters/gridmet/tmean/tmean_mean.tif")

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
writeRaster(tmean_mean_stack, "./rasters/gridmet/rmean/rmean_mean.tif")

############## make sph_mean
sph_list <-list.files('./rasters/gridmet/sph', full.names = TRUE)
sph_stack <-rast(sph_list)

# add and divide
sph_mean <-app(sph_stack, mean)
plot(sph_mean)
hist(sph_mean, breaks = 100)

# save
writeRaster(sph_mean, "./rasters/gridmet/sph/sph_mean.tif")

############## make srad_mean
srad_list <-list.files('./rasters/gridmet/srad', full.names = TRUE)
srad_stack <-rast(srad_list)

# add and divide
srad_mean <-app(srad_stack, mean)
plot(srad_mean)
hist(srad_mean, breaks = 100)

# save
writeRaster(srad_mean, "./rasters/gridmet/srad/srad_mean.tif")


