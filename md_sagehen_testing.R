# max_dowy function testing on sagehen block

library(rhdf5)
library(terra)
library(parallel)
library(pbmcapply)

#######################################################
##### code for bringing block to test functions on
#######################################################

#set working directory
setwd("/Users/jacktarricone/ch1_margulis/")

# metric creating with this script
snow_metric <-"md"

# list hdf swe files
swe_list <-list.files("./swe/hdf", pattern = ".h5", full.names = TRUE)
print(swe_list) # static and wy2015 SCA

# set pth to biggest year and smallest year
path_1993 <-swe_list[9]
path_2015 <-swe_list[31]

# read in a block around sagehen for water year 1993
# pulled from QGIS using the cell number rasters generated
sagehen_wy93 <- h5read(path_1993, "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent
class(sagehen_wy93) #inspect 
dim(sagehen_wy93) #dimensions

# same for 2015
sagehen_wy15 <- h5read(path_2015, "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent
x <-sagehen_wy93[70,90,1:365]
x
plot(x)

##### melt_day function

md <-function(x){
  
  max_swe_dowy <-function(x){
    
    # set threshold so that pixel is NA if never hiss 5 mm
    if (max(x) < 5){
      return(NA)
    } 
    else{
      # pull out max value
      max_swe<-as.numeric(max(x))
      
      # use which() funciton for position tracking
      # nested with max() to have last day of max swe
      dowy <-as.numeric(max(which(x == max_swe)))
      return(dowy)
    }
  }
    # calc ms_dowy
    # return NA for values that never reach the 5 mm threshold
    if (is.na(max_swe_dowy(x))){
      return(NA)
    } else {
      
      # calc mox_swe dowy
      ms_dowy <-max_swe_dowy(x)
      
      # create dowy_vec
      dowy_vect <-seq(1,length(x),1)
      
      # find spot on vector where SWE is less first 5 mm
      # and date is greater than max_swe doy
      melt_out_dowy_vect <-which(x < 5 & dowy_vect > ms_dowy)
      
      # pull out fist element of vect aka first day snow is gone
      melt_out_dowy <-melt_out_dowy_vect[1]
    }
    if (length(melt_out_dowy_vect) == 0){
      # if condition is never met, or snow never goes below 5 mm after max_dowy
      # return last dowy (365 or 366)
      return(length(dowy_vect))
    } else {
    return(melt_out_dowy)
  }
}

vect <-sagehen_wy15[70,100,1:365]
plot(vect)
md(vect)

# test for wy2015
mat_wy15 <-as.matrix(apply(sagehen_wy15, c(1,2), md))
rast_wy15 <-rast(mat_wy15)
plot(rast_wy15)

# test for wy1993
mat_wy93 <-as.matrix(apply(sagehen_wy93, c(1,2), md))
rast_wy93 <-rast(mat_wy93)
plot(rast_wy93)

# differene
diff <-rast_wy93-rast_wy15
plot(diff)

max_swe_dowy_raster <-function(x){
  
#### top half
top <- h5read(hdf_file, "/SWE", index = list(1:3300,1:5701,1:365))
top_max_dowy_mat <-as.matrix(apply(top, c(1,2), md))
rm(top)

#### bottomhalf half
bottom <- h5read(hdf_file, "/SWE", index = list(3301:6601,1:5701,1:365))
bottom_max_dowy_mat <-as.matrix(apply(bottom, c(1,2), max_dowy))
rm(bottom)

#bind chunks together
full <-rbind(top_max_dowy_mat, bottom_max_dowy_mat)
rast <-raster(full, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, crs(dem))
plot(rast)
hist(rast)

name <- gsub(".h5", "", hdf_name)
good_name <- gsub("SN_SWE_", "max_dowy_", name)

setwd("/Volumes/jt/projects/margulis/snow_metric_rasters/max_dowy/rasters")
writeRaster(rast, paste0(good_name, ".tif"))
return(rast)
}
