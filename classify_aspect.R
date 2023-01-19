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
