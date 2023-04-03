# testing making metric for
# date at which fsca goes below certain (15%?) threshold

library(rhdf5)
library(terra)
library(ggplot2)

#######################################################
##### code for bringing block to test functions on
#######################################################

# reset wd
setwd("/Volumes/jack_t/projects/ch1_margulis/") 

# metric creating with this script
snow_metric <-"fsca_thres"

# list sca files
sca_list <-list.files("./sca", full.names = TRUE)
print(sca_list) # static and wy2015 SCA

# set pth to biggest year and smallest year
path_1993 <-sca_list[9]
path_2015 <-sca_list[31]

# read in a block around sagehen for water year 1993
# pulled from QGIS using the cell number rasters generated
sagehen_fsca_wy93 <-h5read(path_1993, "/SCA", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent
sagehen_fsca_wy15 <-h5read(path_2015, "/SCA", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent
sagehen_fsca_wy16 <-h5read(sca_list[32], "/SCA", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent

# get swe data
# list sca files
swe_list <-list.files("~/ch1_margulis/swe/hdf", full.names = TRUE)
sagehen_swe_wy93 <-h5read(swe_list[9], "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent
sagehen_swe_wy15 <- h5read(swe_list[31], "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent
sagehen_swe_wy16 <- h5read(swe_list[32], "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent

# 93 swe vs. fsca
swe15 <-c(sagehen_swe_wy15[50,60,1:365]/10)
swe93 <-c(sagehen_swe_wy93[50,60,1:365]/10)
swe16 <-c(sagehen_swe_wy16[50,60,1:365]/10)

# pull out one pixel time series
sca15 <-c(sagehen_fsca_wy15[50,60,1:365])
sca93 <-c(sagehen_fsca_wy93[50,60,1:365])
sca16 <-c(sagehen_fsca_wy16[50,60,1:365])

# dowy
dowy <-seq(1,length(swe15),1)

# make df
plot_df <-as.data.frame(cbind(dowy,swe15,sca15,swe93,sca93,swe16,sca16))
plot_df

# test plots
ggplot(plot_df) +
  geom_point(aes(y = swe16/10, x = dowy), col = "darkred") +
  geom_point(aes(y = sca16, x = dowy), col = "darkblue") +
  ylim(c(0,70)) +
  xlim(c(230,260))

ggplot(plot_df) +
  geom_point(aes(y = swe15/10, x = dowy), col = "darkred") +
  geom_point(aes(y = sca15, x = dowy), col = "darkblue") 

ggplot(plot_df) +
  geom_point(aes(y = swe93/10, x = dowy), col = "darkred") +
  geom_point(aes(y = sca93, x = dowy), col = "darkblue") +
  ylim(c(0,70)) +
  xlim(c(280,320))

##### mid winter ablation function function
fsca_thres <-function(x, percent){
  
    if (max(x) < percent){
      return(NA)
    } 
    else{
      dowy <-as.numeric(max(which(x > percent)))
      return(dowy)
    }
  }

# test
fsca_thres(z, 25)

# test for wy2015
mat_wy15 <-apply(sagehen_wy15, c(1,2), function(x) fsca_thres(x, 15))
rast_wy15 <-rast(mat_wy15)
plot(rast_wy15)
# writeRaster(rast_wy15, "./snow_metric_rasters/terra_rasters/mwa/tests/sh_wy15.tif")

# test for wy1993
mat_wy93 <-apply(sagehen_wy93, c(1,2), function(x) fsca_thres(x, 15))
rast_wy93 <-rast(mat_wy93)
plot(rast_wy93)
# writeRaster(rast_wy93, "./snow_metric_rasters/terra_rasters/mwa/tests/sh_wy93.tif")

mat_wy16 <-apply(sagehen_fsca_wy16, c(1,2), function(x) fsca_thres(x, 15))
rast_wy16 <-rast(mat_wy16)
plot(rast_wy16)
hist(rast_wy16, breaks = 100)

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


