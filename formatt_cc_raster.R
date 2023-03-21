### plot average snow metric rasters
# jack tarricone

library(terra)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)

#set working directory
setwd("~/ch1_margulis")

## bring in SNSR shape file
# with terra
snsr <-vect("./vectors/snsr_shp.gpkg")

# with sf
snsr_v1 <-st_read("./vectors/snsr_shp.gpkg")
snsr_sf <-st_geometry(snsr_v1)

## dem
dem <-rast("./rasters/static/SNSR_DEM.tif")

## bring in raw cc
cc_v1 <-rast("./rasters/nlcd_cc/nlcd_full.tif")
cc_v1
plot(cc_v1)
hist(cc_v1)

# set rough crop
rough_crop <-ext(-2493045, -1893045, 177285, 2410005)

## crop
cc_v2 <-crop(cc_v1, rough_crop)
plot(cc_v2)

### reproj
cc_v3 <-project(cc_v2, crs(dem), method = 'bilinear')
cc_v3
plot(cc_v3)

# resample
cc_v4 <-resample(cc_v3, dem)
cc_v4
plot(cc_v4)

# mask
cc_v5 <-mask(cc_v4, snsr)
plot(cc_v5)
# riteRaster(cc_v5, "./rasters/nlcd_cc/cc_w0.tif")

# 0 to NaN
cc_v6 <-subst(cc_v5, 0, NA)
cc_v6
# writeRaster(cc_v6, "./rasters/nlcd_cc/cc_wNA.tif")