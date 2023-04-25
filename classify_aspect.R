# classify SNSR aspect raster
# jack tarricone
# january 18th, 2023

library(terra)

# bring in aspect raster
aspect_v1 <-rast("/Users/jacktarricone/ch1_margulis/static/rasters/SNSR_aspect.tif")
aspect <-project(aspect_v1, crs(max_stack))
plot(aspect)
hist(aspect, breaks = 100)

# bin into 4 categories north, south, east, west
aspect_classes_4 <-matrix(c(315,360,1, 00,45,1, # 1 = north
                            135,225,2,          # 2 = south
                            45,135,3,           # 3 = east 
                            225,315,4),         # 4 = west
                          ncol=3, byrow=TRUE)

aspect_classes_2 <-matrix(c(270,360,1, 0,90,1, # 1 = north
                            90,270,2),          # 2 = south
                          ncol=3, byrow=TRUE)

aspect_classes_2

# classify using matrix
aspect_cat <-classify(aspect, rcl = aspect_classes_4)
plot(aspect_cat)
# writeRaster(aspect_cat, "./static/aspect_cat.tif")

aspect_cat_ns <-classify(aspect, rcl = aspect_classes_2)
plot(aspect_cat_ns)
hist(aspect_cat_ns)
# writeRaster(aspect_cat_ns, "./static/aspect_cat_ns.tif")


##### add north and south classification
# b1_n = 1
test <-dem_3z == 1 &  # ez1
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
