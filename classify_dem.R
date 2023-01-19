# classify dem raster
# jack tarricone
# january 18th, 2023

library(terra)

setwd("/Users/jacktarricone/ch1_margulis/")

# bring in dem raster
dem_v1 <-rast("./static/rasters/SNSR_DEM.tif")
dem <-project(dem_v1, 'EPSG:4326')
plot(dem)
hist(dem, breaks = 100)

# bring in american shape file
american_v1 <-vect("./american_test/american.shp")
american <-project(american_v1, crs(scf_stack))

# crop and mask
dem_am_v1 <-crop(dem, american)
dem_am <-mask(dem_am_v1, american)
plot(dem_am)

hist(dem_am)
min(dem_am)

# bin into 4 categories north, south, east, west
dem_classes_3 <-matrix(c(1500,2000,1, # 1 = north
                         2000,2500,2,          # 2 = south
                         2500,3060,3),           # 3 = east 
                         ncol=3, byrow=TRUE)
dem_classes_3

# classify using matrix
dem_am_cat <-classify(dem_am, rcl = dem_classes_3)
plot(dem_am_cat)
dem_am_cat
hist(dem_am_cat)
# writeRaster(dem_am_cat, "./static/dem_am_cat.tif")

# calculate percentage of land area


