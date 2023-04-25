# classify dem raster
# jack tarricone
# january 18th, 2023

library(terra)

setwd("~/ch1_margulis")

# bring in dem raster
dem <-rast("./rasters/static/SNSR_DEM.tif")
plot(dem)
hist(dem, breaks = 100)

# pull out dem max
dem_max <-as.numeric(global(dem,max,na.rm=T))

# bin into 11 elevation categories by 250m
dem_classes <-matrix(c(1500,2250,1, # 1 = north
                       2250,3000,2,
                       3000,dem_max,3), # 2 = south), # 3 = east 
                       ncol=3, byrow=TRUE)

# classify using matrix
dem_3z <-classify(dem, rcl = dem_classes)
plot(dem_3z)
hist(dem_3z)
writeRaster(dem_3z, "./rasters/categorized/dem_3z.tif")

# compute metricsf
aspect <-terrain(dem, v = "aspect", neighbors = 8, unit = "degrees")
slope <-terrain(dem, v = "slope", neighbors = 8, unit = "degrees")

# thres hold aspect to nan under two degrees
slope_thres <-slope
values(slope_thres)[values(slope_thres) < 4] = NA
# writeRaster(slope_thres, "./rasters/categorized/slope_4_na.tif", overwrite = TRUE)

# 
aspect_thres <-mask(aspect, slope_thres, maskvalue = NA)
plot(aspect_thres)
# writeRaster(aspect_thres, "./rasters/categorized/aspect_thres_4.tif", overwrite = TRUE)

# bin into 45 degree segments
aspect_classes_8 <-matrix(c(0,45,1, # 1 = north
                          45,90,2,
                          90,135,3, # 2 = south
                          135,180,4,
                          180,225,5,
                          225,270,6,
                          270,316,7,
                          315,360,8), # 3 = east 
                          ncol=3, byrow=TRUE)
aspect_classes_8

# classify using matrix
aspect_cat <-classify(aspect_thres, rcl = aspect_classes_8)
plot(aspect_cat)
hist(aspect_cat)
# writeRaster(aspect_cat, "./rasters/categorized/aspect_thres_8_classes.tif", overwrite = TRUE)

# bin into 11 elevation categories by 250m
aspect_classes_4 <-matrix(c(0,45,1, # 1  north
                            315,360,1,
                            45,135,2, # 2 = south
                            135,225,3,
                            225,315,4),
                            ncol=3, byrow=TRUE)
aspect_classes_4

# classify using matrix
aspect_cat_4 <-classify(aspect_thres, rcl = aspect_classes_4)
plot(aspect_cat_4)
hist(aspect_cat_4)
# writeRaster(aspect_cat_4, "./rasters/categorized/aspect_thres_4_classes.tif", overwrite = TRUE)

# test plot
plot(aspect_cat_4)
plot(dem_3z)

##### add north and south classification
#### ez1_n = 1
ez1_n <-dem_3z == 1 &  # ez1
       aspect_cat_4 == 1  # north
ez1_n <-subst(ez1_n,TRUE,1)
ez1_n <-subst(ez1_n,0,NA)
plot(ez1_n, col = 'red')
# writeRaster(ez1_n, "./rasters/categorized/ez1_n.tif")

##### ez1_s = 2
ez1_s <-dem_3z == 1 & # ez1
          aspect_cat_4 == 3 # south
ez1_s <-subst(ez1_s,TRUE,2)
ez1_s <-subst(ez1_s,0,NA)
plot(ez1_s, col = 'darkgreen', add = TRUE)
# writeRaster(ez1_s, "./rasters/categorized/ez1_s.tif")

##### ez2_n = 3
ez2_n <-dem_3z == 2 &  # ez2
          aspect_cat_4 == 1  # north
ez2_n <-subst(ez2_n,TRUE,3)
ez2_n <-subst(ez2_n,0,NA)
plot(ez2_n, col = 'purple', add = TRUE)
# writeRaster(ez2_n, "./rasters/categorized/ez2_n.tif")

##### ez2_s = 3
ez2_s <-dem_3z == 2 &  # ez2
        aspect_cat_4 == 3  # south
ez2_s <-subst(ez2_s,TRUE,4)
ez2_s <-subst(ez2_s,0,NA)
plot(ez2_s, col = 'firebrick', add = TRUE) 
# writeRaster(ez2_s, "./rasters/categorized/ez2_s.tif")

##### ez3_n = 5
ez3_n <-dem_3z == 3 &  # ez3
          aspect_cat_4 == 1  # north
ez3_n <-subst(ez3_n,TRUE,5)
ez3_n <-subst(ez3_n,0,NA)
plot(ez3_n, col = 'grey50', add = TRUE)
# writeRaster(ez3_n, "./rasters/categorized/ez3_n.tif")

##### ez3_s = 5
ez3_s <-dem_3z == 3 &  # ez3
  aspect_cat_4 == 3  # south
ez3_s <-subst(ez3_s,TRUE,6)
ez3_s <-subst(ez3_s,0,NA)
plot(ez3_s, col = 'yellow', add = TRUE)
# writeRaster(ez3_s, "./rasters/categorized/ez3_s.tif")

# mosaic back together
stitch <-mosaic(ez1_n, ez1_s, ez2_n, ez2_s, ez3_n, ez3_s)
plot(stitch)

writeRaster(stitch, "./rasters/categorized/dem_ez3_ns.tif")


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



