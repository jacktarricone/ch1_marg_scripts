# classify dem raster
# jack tarricone
# january 18th, 2023

library(terra)

setwd("/Users/jacktarricone/ch1_margulis/")

# bring in dem raster
dem <-rast("./rasters/static/SNSR_DEM.tif")
plot(dem)
hist(dem, breaks = 100)

# compute metrics
aspect <-terrain(dem, v = "aspect", neighbors = 8, unit = "degrees")
slope <-terrain(dem, v = "slope", neighbors = 8, unit = "degrees")

# thres hold aspect to nan under two degrees
slope_thres <-slope
values(slope_thres)[values(slope_thres) < 4] = NA
writeRaster(slope_thres, "./rasters/categorized/slope_4_na.tif", overwrite = TRUE)

# 
aspect_thres <-mask(aspect, slope_thres, maskvalue = NA)
plot(aspect_thres)
writeRaster(aspect_thres, "./rasters/categorized/aspect_thres_4.tif", overwrite = TRUE)

# bin into 11 elevation categories by 250m
aspect_classes <-matrix(c(0,45,1, # 1 = north
                          45,90,2,
                          90,135,3, # 2 = south
                          135,180,4,
                          180,225,5,
                          225,270,6,
                          270,316,7,
                          315,360,8), # 3 = east 
                          ncol=3, byrow=TRUE)
aspect_classes

# classify using matrix
aspect_cat <-classify(aspect_thres, rcl = aspect_classes)
plot(aspect_cat)
hist(aspect_cat)
writeRaster(aspect_cat, "./rasters/categorized/aspect_thres_8_classes.tif", overwrite = TRUE)

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
writeRaster(aspect_cat_4, "./rasters/categorized/aspect_thres_4_classes.tif", overwrite = TRUE)











# bring in aspect ns
aspect_ns <-rast("./static/aspect_cat_ns.tif")
aspect_ns_am_v1 <-crop(aspect_ns, american)
aspect_ns_am <-mask(aspect_ns_am_v1, american)
# writeRaster(aspect_ns_am, "./static/aspect_ns_am.tif")

# test plot
plot(aspect_ns_am)
plot(dem_am_cat)

##### add north and south classification
# b1_n = 1
test <-dem_am_cat == 1 &  # b1
       aspect_ns_am == 1  # n
test <-subst(test,TRUE,1)
test <-subst(test,0,NA)
plot(test, col = 'red')

# b1_s = 2
test_v2 <-dem_am_cat == 1 & # b1
          aspect_ns_am == 2 # s
test_v2 <-subst(test_v2,TRUE,2)
test_v2 <-subst(test_v2,0,NA)
plot(test_v2, col = 'darkgreen', add = TRUE)

# b2_n = 3
test_v3 <-dem_am_cat == 2 &  # b2
          aspect_ns_am == 1  # n
test_v3 <-subst(test_v3,TRUE,3)
test_v3 <-subst(test_v3,0,NA)
plot(test_v3, col = 'purple', add = TRUE)

# b2_s = 4
test_v4 <-dem_am_cat == 2 &  # b2
          aspect_ns_am == 2  # s
test_v4 <-subst(test_v4,TRUE,4)
test_v4 <-subst(test_v4,0,NA)
plot(test_v4, col = 'darkorange', add = TRUE)

# b3_n = 5
test_v5 <-dem_am_cat == 3 &  # b3
          aspect_ns_am == 1  # n
test_v5 <-subst(test_v5,TRUE,5)
test_v5 <-subst(test_v5,0,NA)
plot(test_v5, col = 'grey50', add = TRUE)

# b3_s = 6
test_v6 <-dem_am_cat == 3 &  # b3
          aspect_ns_am == 2  # s
test_v6 <-subst(test_v6,TRUE,6)
test_v6 <-subst(test_v6,0,NA)
plot(test_v6, col = 'yellow', add = TRUE)

# mosaic back together
stitch <-mosaic(test,test_v2,test_v3,test_v4,test_v5,test_v6)
plot(stitch)

writeRaster(stitch, "./static/american_dem_aspect_classified.tif")


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



