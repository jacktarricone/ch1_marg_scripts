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
for (year in 1985:2016) {
  
  # define variable
  var <-"tmmx"
  
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
for (year in 1985:2016) {
  
  # define variable
  var <-"tmmx"
  
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
  vm3 <- vm2 - 273.15
  plot(vm_3)
  
  # save
  saving_location2 <-paste0('./rasters/gridmet/',var,"/",var,"_ondjfm_",year,".tif")
  writeRaster(vm_3, saving_location2)
  print(paste0(year, " ", var, " is done!"))
}




