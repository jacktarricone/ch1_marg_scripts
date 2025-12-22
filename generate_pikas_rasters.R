### creating pixel-wise annual rasters for:
#### wa

# april 25, 2023
# jack tarricone
# install.packages("BiocManager")
# BiocManager::install("rhdf5")

library(rhdf5)
library(terra)
library(parallel)
library(pbmcapply)

#set working directory
setwd("~/ch1_margulis")

# list hdf swe files
swe_list <-list.files("./swe/hdf", pattern = ".h5", full.names = TRUE)
print(swe_list) # static and wy2015 SCA

# read in dem for georeferencing
static_list <-list.files("./rasters/static", pattern = ".tif", full.names = TRUE)
print(static_list) # static and wy2015 SCA
dem <-rast(static_list[3])
dem

# load in functions
# this file has all the snow metric functions and the raster creation one

url <-"https://raw.githubusercontent.com/jacktarricone/ch1_marg_scripts/main/snow_metric_functions.R"
devtools::source_url(url)

###########################
#####  make rasters #######
###########################

# metric creating with this script
snow_metric_name <-"pikas_0.5m"
snow_metric_name

# mcapply function 
# set number of cores to use, this is dependent on your machine
ncores <-5

# check list, looks good
swe_list

#######################################
############      pikas     ###########
#######################################

###### create function for SWE threshold for given swe value for pikas work
###### 

pikas_50mm <-function(x, swe_thres = 25.4){
  
}

length(swe_list)
# run function using progress bar (pb) multi-core lapply
system.time(raster_list <-pbmclapply(1:length(swe_list),
                                     function(x)
                                     generate_snow_metric_rasters(x,
                                                                  snow_metric_function = "pikas_50mm",
                                                                  snow_metric_name = "pika_50mm"),
                                     mc.cores = ncores,
                                     mc.cleanup = TRUE))
