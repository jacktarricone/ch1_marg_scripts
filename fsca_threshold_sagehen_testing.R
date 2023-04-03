# testing making metric for
# date at which fsca goes below certain (15%?) threshold

library(rhdf5)
library(terra)
library(parallel)
library(pbmcapply)

#######################################################
##### code for bringing block to test functions on
#######################################################

# reset wd
setwd("/Volumes/jack_t/projects/ch1_margulis/") 

# metric creating with this script
snow_metric <-"fsca_thres"

# list sca files
sca_list <-list.files("./sca", full.names = TRUE)
print(swe_list) # static and wy2015 SCA

# set pth to biggest year and smallest year
path_1993 <-swe_list[9]
path_2015 <-swe_list[31]

# read in a block around sagehen for water year 1993
# pulled from QGIS using the cell number rasters generated
sagehen_wy93 <-h5read(path_1993, "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent
class(sagehen_wy93) #inspect 
dim(sagehen_wy93) #dimensions

# same for 2015
sagehen_wy15 <- h5read(path_2015, "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent

# pull out one pixel time series
z <-c(sagehen_wy15[50,60,1:365])
plot(z)
x <-z


##### mid winter ablation function function
mwa <-function(x){
  
  # define max_swe_dowy
  max_swe_dowy <-function(x){
    if (max(x) < 5.1){
      return(NA)
    } 
    else{
      max_swe <-as.numeric(max(x))
      dowy <-as.numeric(max(which(x == max_swe)))
      return(dowy)} 
  }
  
  # calc ms_dowy
  # return 0 for values that never reach the 5.1 mm threshold
  if (is.na(max_swe_dowy(x))){
    return(NA)
  } else {
    ms_dowy <-max_swe_dowy(x)
    
    # trim vector to that date
    before_max_swe <-x[1:ms_dowy]
    
    # find difference between values
    val_diff <-diff(before_max_swe)
    
    # sum all negative values
    mwa_mm <-abs(sum(val_diff[val_diff<0]))
    return(mwa_mm)
  }
}

# test
mid_winter_abal(z)

# test for wy2015
mat_wy15 <-apply(sagehen_wy15, c(1,2), mid_winter_abal)
rast_wy15 <-rast(mat_wy15)
plot(rast_wy15)
# writeRaster(rast_wy15, "./snow_metric_rasters/terra_rasters/mwa/tests/sh_wy15.tif")

# test for wy1993
mat_wy93 <-as.matrix(apply(sagehen_wy93, c(1,2), mid_winter_abal))
rast_wy93 <-rast(mat_wy93)
plot(rast_wy93)
# writeRaster(rast_wy93, "./snow_metric_rasters/terra_rasters/mwa/tests/sh_wy93.tif")

# differene
diff <-rast_wy93-rast_wy15
plot(diff)

max_swe_dowy_raster <-function(x){
  
#### top half
top <- h5read(hdf_file, "/SWE", index = list(1:3300,1:5701,1:365))
top_max_dowy_mat <-as.matrix(apply(top, c(1,2), max_swe_dowy))
rm(top)

#### bottomhalf half
bottom <- h5read(hdf_file, "/SWE", index = list(3301:6601,1:5701,1:365))
bottom_max_dowy_mat <-as.matrix(apply(bottom, c(1,2), max_dowy))
rm(bottom)

#bind chunks together
full <-rbind(top_max_dowy_mat, bottom_max_dowy_mat)
rast <-raster(full, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66"))
plot(rast)
hist(rast)

name <- gsub(".h5", "", hdf_name)
good_name <- gsub("SN_SWE_", "max_dowy_", name)

setwd("/Volumes/jt/projects/margulis/snow_metric_rasters/max_dowy/rasters")
writeRaster(rast, paste0(good_name, ".tif"))
return(rast)
}


