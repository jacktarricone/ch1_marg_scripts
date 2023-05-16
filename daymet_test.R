# download and format daymet data
# may 15th

library(daymetr)
library(terra)
library(raster)
library(parallel)
library(pbmcapply)

# set working
setwd("~/ch1_margulis")

# load in snsr shape and dem
snsr <-vect("./vectors/snsr_shp.gpkg")
dem <-rast("./rasters/static/SNSR_DEM.tif")

x <-1

calc_ondjfm_mean <-function(x){
  
  # read in list
  list <-list.files("./rasters/daymet/tmin/tif_stacks", full.names = TRUE)
  
  # read in two calendar years to complete wy
  y1 <-list[x]
  y2 <-list[x+1]
  
  # bind and trim down to oct-march
  rast_in <-rast(c(y1,y2))
  rast_trim <-rast_in[[4:9]] # pull out correct months
  
  # calc mean
  mean <-terra::app(rast_trim, fun = 'mean')
  
  # name and save
  name_v1 <-basename(y2)
  name <-gsub("tmax_monavg","tmax_mean_ondjfm",name_v1)
  writeRaster(mean, paste0("./rasters/daymet/tmin/wy_mean/",name))
  print(paste0(name, " saving complete!"))
}

# run
lapply(seq(1,33,1), calc_ondjfm_mean)

x <-1

# mean min and max
calc_yearly_mean <-function(x){
  
  # read in list
  min_list <-list.files("./rasters/daymet/tmin/wy_mean", full.names = TRUE)
  max_list <-list.files("./rasters/daymet/tmax/wy_mean", full.names = TRUE)
  
  # read in two calendar years to complete wy
  min_rast <-min_list[x]
  max_rast <-max_list[x]
  
  # bind and trim down to oct-march
  rast_in <-rast(c(min_rast,max_rast))
  
  # calc mean
  mean <-terra::app(rast_in, fun = 'mean')
  
  # name and save
  name_v1 <-basename(max_rast)
  name <-gsub("tmax_mean_ondjfm","tmean_ondjfm",name_v1)
  writeRaster(mean, paste0("./rasters/daymet/tmean/",name))
  print(paste0(name, " saving complete!"))
}

# calc
lapply(seq(1,32,1), calc_yearly_mean)

# calculate normal over the study period
tmean_rast <-rast(list.files("./rasters/daymet/tmean/", full.names = TRUE))
tmean_normal <-app(tmean_rast, fun = 'mean')
writeRaster(tmean_normal, "./rasters/daymet/tmean_normal_1985_2016.tif")

# test download
download_daymet_ncss(location = c(41.8758638835292,-123.065041220838,35.4219961410279,-117.651990878793),
                     start = 1995,
                     end = 2016,
                     param = "srad",
                     path = "./rasters/daymet/")

# test download
download_daymet_ncss(location = c(41.8758638835292,-123.065041220838,35.4219961410279,-117.651990878793),
                     start = 1984,
                     end = 1984,
                     param = "tmax",
                     frequency = "monthly",
                     path = "./rasters/daymet/tmax/nc/")

# test download
download_daymet_ncss(location = c(41.8758638835292,-123.065041220838,35.4219961410279,-117.651990878793),
                     start = 1984,
                     end = 2016,
                     param = "tmin",
                     frequency = "monthly",
                     path = "./rasters/daymet/tmin/nc/")



# list tmax files
tmin_list <-list.files("./rasters/daymet/tmin/nc", full.names = TRUE)

# formatting and saving function
format_nc_tif <-function(x){
  
  # read in annaul monthly stack
  tmax_raster <-raster::stack(x)
  
  # reset crs bc it's wrong, +datum=WGS84, and units = km
  crs(tmax_raster) <-"+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs" # this works!!
  
  # reproject and covert to rast
  tmax_v2 <-raster::projectRaster(tmax_raster, crs = "+init=epsg:4326")
  tmax_v3 <-rast(tmax_v2)
  tmax_v4 <-c(tmax_v3[[1:3]],tmax_v3[[10:12]])
  
  # resample to 90 m SNSR res, and mask for shape file
  tmax_v5 <-resample(tmax_v4, dem, method = "bilinear")
  tmax_v6 <-mask(tmax_v5, snsr)
  
  # name and save
  name_v1 <-basename(x)
  name <-gsub("*s.nc","s.tif",name_v1)
  writeRaster(tmax_v6, paste0("./rasters/daymet/tmin/tif_stacks/",name))
  print(paste0(name, " saving complete!"))
}

# mcapply function 
# set number of cores to use
ncores <-6
# lapply(tmax_list[1], format_nc_tif)

# run function using progress bar (pb) multi-core lapply
# make sure to give it proper metric name and function
system.time(pbmclapply(tmax_list, format_nc_tif,
                       mc.cores = ncores, mc.cleanup = TRUE))




