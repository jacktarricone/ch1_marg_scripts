climateR::params
remotes::install_github("mikejohnson51/AOI") # suggested!
remotes::install_github("mikejohnson51/climateR")
library(daymetr)
library(terra)
library(raster)

setwd("~/ch1_margulis")

# load in snsr shape
snsr <-vect("./vectors/snsr_shp.gpkg")
ext(snsr)

# test download
download_daymet_ncss(location = c(41.8758638835292,-123.065041220838,35.4219961410279,-117.651990878793),
                     start = 1985,
                     end = 2016,
                     param = "srad",
                     path = "./rasters/daymet/")

srad_raster <-raster("./rasters/daymet/srad_daily_1990_ncss.nc")
srad_repoj <-projectRaster(srad_raster, crs=crs(snsr))
writeRaster(srad_v1, "./rasters/daymet/srad_daily_1990_ncss.tif")

srad <-rast("./rasters/daymet/srad_daily_1990_ncss.tif")
srad_v1 <-project(srad, crs(snsr))
ext(srad_v1) <-ext(snsr)
plot(srad_v1)
srad_v2 <-mask(srad_v1, snsr)
plot(srad_v2)
writeRaster(srad_v2, "./rasters/daymet/daymet_srad_1990.tif")
