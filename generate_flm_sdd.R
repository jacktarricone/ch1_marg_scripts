# testing

library(terra)

setwd("~/ch1_margulis/")

# load in stack
flm_list <-list.files("~/ch3_fusion/rasters/flm/raw", full.names = T)
tail(flm_list)

# stack em
flm_stack <-rast(flm_list)
# flm_stack <-ifel(flm_stack1 == 0, NA, flm_stack1)
# writeRaster(flm_stck, "./~/ch3_fusion/rasters/flm/flm_stack_formatted.tif")
# plot(flm_stack[[180]])
# flm_stack

# crop mask yuba
# tuo_shp1 <-vect("./sierra_basin_shapefiles/upper_tuolumne.gpkg")
# tuo_shp <-project(tuo_shp1, crs(flm_stack))
# tuo_stack1 <-crop(flm_stack, ext(tuo_shp))
# tuo_stack <-mask(tuo_stack1, tuo_shp)
# # tuo_stack_form <-ifel(tuo_stack == 0,NA,tuo_stack)
# # writeRaster(tuo_stack, "./rasters/tuo_stack.tif")


# test <-c(100,100,100,16,100,100,100,100,199,100,100,15,100,100,100,100,100,100,15,68,17,16,15,14,0)
# dowy <-as.integer(max(which(test >= 15)))
# # yaht <-sdd(test)
# 
# # read in rast
# tuo_stack1 <-rast("./rasters/tuo_stack.tif")

# agg to 300
flm_stack2 <-terra::aggregate(flm_stack, fact = 3, fun = near, cores = 14)
flm_stack2
plot(flm_stack2[[220]])

# reproj
flm_stack3 <-project(flm_stack2, "EPSG:4326", method = "near")
plot(flm_stack3[[220]])

# define and calc sdd
sdd <-function(x, sca_thres = 30){
  
  # Remove NAs, or handle them safely in the comparison
  if (all(is.na(x))) {
    return(NA)  # Entire pixel time series is NA
  }
  
  # 10 mm (1 cm)
  if (max(x) < sca_thres){
    return(NA)
  } 
  else{
    dowy <-as.integer(max(which(x >= sca_thres)))
    return(dowy)
  }
}

# run and plot
plot(tuo_stack[[245]])
flm90_sdd <- terra::app(x = flm_stack3, fun = sdd, cores = 14)
plot(flm90_sdd)
writeRaster(flm90_sdd, "./rasters/flm_wy2020_90m_30_sdd.tif")


# full thing test
flm_wy20_sdd <- terra::app(x = flm_stack, fun = sdd, cores = 14)
plot(flm_wy20_sdd)
writeRaster(flm_wy20_sdd, "./rasters/flm_wy2020_30m_30_sdd.tif")
hist(flm_wy20_sdd)
