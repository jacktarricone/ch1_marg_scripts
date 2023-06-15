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
dem <-rast("./rasters/static/SNSR_DEM.tif")



# srad_stack <-data_list$daily_mean_shortwave_radiation_at_surface, srad
# tmmx_stack <-data_list$daily_maximum_temperature, tmmx
# tmmn_stack <-data_list$daily_minimum_temperature, tmmn
# rmax_stack <-data_list$daily_maximum_relative_humidity, rmax
# rmin_stack <-data_list$daily_minimum_relative_humidity, rmin
# sph_stack  <-data_list$daily_mean_specific_humidity, sph



## starting function for annual values

var_full <-"daily_mean_shortwave_radiation_at_surface"
var <-"srad"
  
wy_list <-seq(1985,2016,1)
year <-wy_list[[10]]
start_date <-paste0(year - 1,"-10-01")
end_date <-paste0(year,"-03-30")

data_list <-getGridMET(snsr_sf, 
               varname = var,
               startDate = start_date,
               endDate = end_date)

var_stack <-data_list[[1]]
var_stack

var_stack_v2 <-resample(var_stack, dem, method = "bilinear", threads = TRUE)



# tmmx
tmmx <-rast('./rasters/gridmet/tmmx/tmmx_1985.nc')

# open file
ncin <- nc_open('./rasters/gridmet/tmmx/tmmx_1985.nc')

# bring swe variable into array
lat_min <- ncvar_get(ncin,"global attritubes") # read in
mean_swe_array <-swe_array[,,1,] # pull out first stat or "mean SWE"
dowy200 <-mean_swe_array[,,200] # day of water year 200


test <-subst(tmmx[[180]], 32767, NA)
plot(test)
