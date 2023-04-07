# comparing melt rate dynamics to SNOTELS

library(terra)
library(dplyr)
library(ggplot2)
library(snotelr)

setwd('~/ch1_margulis')

# bring in real snotel locations
snotel_locs <-read.csv("./csvs/SNOTEL_MASTER (1).csv")
head(snotel_locs)

# bring in mwa 2016
mwa16 <-rast('./rasters/snow_metrics/mwa/mwa_WY2016.tif')

# filter for CA
snotel_ca <-filter(snotel_locs, State == "CA")
ca_points <-vect(snotel_ca, geom = c("Longitude","Latitude"), crs = crs(mwa16))

# crop down to extent of points test
crop_ext <-ext(-120.79192, -119, 38, 39.8)
mwa_snotel_ext <-crop(mwa16, crop_ext)
ca_points_snsr <-crop(ca_points, crop_ext)

# crop 
plot(mwa_snotel_ext)
plot(ca_points_snsr, add =TRUE)

#

