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

# bin into 11 elevation categories by 250 m
dem_classes_11 <-matrix(c(1500,1750,1, 
                          1750,2000,2,         
                          2000,2250,3,
                          2250,2500,4,
                          2500,2750,5,
                          2750,3000,6,
                          3000,3250,7,
                          3250,3500,8,
                          3500,3750,9,
                          3750,4000,10,
                          4000,4360,11),          
                          ncol=3, byrow=TRUE)
dem_classes_11

# classify using matrix
dem_cat11 <-classify(dem, rcl = dem_classes_11)
plot(dem_cat11)
dem_cat11
hist(dem_cat11)
# writeRaster(dem_am_cat, "./static/dem_am_cat.tif")

# bring in aspect ns
aspect_ns <-rast("./static/aspect_cat_ns.tif")

##### add north and south classification
# b1_n = 1

# create function
create_ele_aspect <-function(dem,aspect,bin_dem,bin_aspect){
    tf <-dem == bin_dem &  # b1
         aspect == bin_aspect  # n
    tf_v1 <-subst(tf,TRUE,bin_dem)
    rast <-subst(tf_v1,0,NA)
    # rast
    # plot(rast, col = 'red')
    return(rast)
}

# north facing
test <-create_ele_aspect(dem = dem_cat11,
                         aspect = aspect_ns,
                         bin_dem = 1,
                         bin_aspect = 1)

# vector of bin numbers
ele_bin_list <-seq(1,11,1)

# lapp for north to make a list of rasters
north_list <-lapply(ele_bin_list, function(x) create_ele_aspect(dem = dem_cat11,
                                             aspect = aspect_ns,
                                             bin_dem = x, 
                                             bin_aspect = 1)) # 1 = north facing

# mosaic back together
north_stitch <-mosaic(north_list[[1]],north_list[[2]],north_list[[3]],north_list[[4]],north_list[[5]],
                north_list[[6]],north_list[[7]], north_list[[8]],north_list[[9]],north_list[[10]],north_list[[11]])

plot(north_stitch)



# lapp
south_list <-lapply(ele_bin_list, function(x) create_ele_aspect(dem = dem_cat11,
                                                               aspect = aspect_ns,
                                                               bin_dem = x, 
                                                               bin_aspect = 2)) # 2 = south facing

# mosaic back together
south_stitch <-mosaic(test_list[[1]],test_list[[2]],test_list[[3]],test_list[[4]],test_list[[5]],
                test_list[[6]],test_list[[7]], test_list[[8]],test_list[[9]],test_list[[10]],test_list[[11]])

plot(south_stitch)


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



