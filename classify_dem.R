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
american <-project(american_v1, crs(dem))

# crop and mask
dem_am_v1 <-crop(dem, american)
dem_am <-mask(dem_am_v1, american)
plot(dem_am)

hist(dem_am)
min(dem_am)
max(dem)

# bin into 3 categories
dem_classes_3 <-matrix(c(1500,2000,1, # 1 = north
                         2000,2500,2,          # 2 = south
                         2500,3060,3),           # 3 = east 
                         ncol=3, byrow=TRUE)
dem_classes_3


# bin into 11 categories north, south, east, west for
dem_classes_11 <-matrix(c(1500,1750,1, # 1 = north
                          1750,2000,2,          # 2 = south
                          2000,2250,3,
                          2250,2500,4,
                          2500,2750,5,
                          2750,3000,6,
                          3000,3250,7,
                          3250,3500,8,
                          3500,3750,9,
                          3750,4000,10,
                          4000,4360,11),           # 3 = east 
                          ncol=3, byrow=TRUE)
dem_classes_11




# classify using matrix
dem_cat11 <-classify(dem, rcl = dem_classes_11)
plot(dem_cat11)
dem_cat11
hist(dem_cat11)
writeRaster(dem_cat11, "./static/dem_cat11.tif")









# calculate percentage of land area
total_area <-expanse(dem_am_cat, unit = 'km') #km^2

### mask for each individual layer
# first
first_bin <-subst(dem_am_cat,c(2:3),NA)
plot(first_bin, col = 'darkblue')

# second
second_bin <-subst(dem_am_cat,c(1,3),NA)
plot(second_bin, col = "darkred", add = TRUE)

# second
third_bin <-subst(dem_am_cat,c(1,2),NA)
plot(third_bin, col = "darkgreen", add = TRUE)

# calc areas
first_area <-expanse(first_bin, unit = 'km') # km^2
first_percent <-round((first_area/total_area)*100, digits = 1)
# 56.2 of land area

second_area <-expanse(second_bin, unit = 'km') # km^2
second_percent <-round((second_area/total_area)*100, digits = 1)
# 37 of land area

third_area <-expanse(third_bin, unit = 'km') # km^2
third_percent <-round((third_area/total_area)*100, digits = 2)
# 6.8 of land area



