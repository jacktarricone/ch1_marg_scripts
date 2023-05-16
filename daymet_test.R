# download and format daymet data
# may 15th

library(daymetr)
library(terra)
library(raster)

# set working
setwd("~/ch1_margulis")

# load in snsr shape and dem
snsr <-vect("./vectors/snsr_shp.gpkg")
dem <-rast("./rasters/static/SNSR_DEM.tif")

# # test download
# download_daymet_ncss(location = c(41.8758638835292,-123.065041220838,35.4219961410279,-117.651990878793),
#                      start = 1995,
#                      end = 2016,
#                      param = "srad",
#                      path = "./rasters/daymet/")
# 
# # test download
# download_daymet_ncss(location = c(41.8758638835292,-123.065041220838,35.4219961410279,-117.651990878793),
#                      start = 1985,
#                      end = 1994,
#                      param = "tmax",
#                      frequency = "monthly",
#                      path = "./rasters/daymet/")


# list tmax files
tmax_list <-list.files("./rasters/daymet/tmax/nc", full.names = TRUE)

# formatting and saving function
format_nc_tif <-function(x){
  
  # read in annaul monthly stack
  tmax_raster <-raster::stack(x)
  
  # reset crs bc it's wrong, +datum=WGS84, and units = km
  crs(tmax_raster) <-"+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs" # this works!!
  
  # reproject and covert to rast
  tmax_v2 <-raster::projectRaster(tmax_raster, crs = "+init=epsg:4326")
  tmax_v3 <-rast(tmax_v2)
  
  # resample to 90 m SNSR res, and mask for shape file
  tmax_v4 <-resample(tmax_v3, dem, method = "bilinear")
  tmax_v5 <-mask(tmax_v4, snsr)
  
  # name and save
  name_v1 <-basename(x)
  name <-gsub("*s.nc","s.tif",name_v1)
  writeRaster(tmax_v5, paste0("./rasters/daymet/tmax/tif_stacks/",name))
  print(paste0(name, " saving complete!"))
}

lapply(tmax_list[2:32], format_nc_tif)


