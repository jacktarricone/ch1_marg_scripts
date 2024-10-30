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

## list attributes for swe file
h5ls(swe_list[1], datasetinfo = TRUE) # contains 3 groups: lat, long, and SCA
h5readAttributes(swe_list[1], "SWE") # SWE units = mm

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
snow_metric_name <-"wa"
snow_metric_name

# mcapply function 
# set number of cores to use
ncores <-5

# check list, looks good
swe_list

#######################################
############      wa       ###########
#######################################

###### winter ablation (mm)
###### calculated from dom

wa <-function(x, swe_thres = 25.4){
  
  max_swe_dowy <-function(x){
    
    # set threshold
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      # pull out max value
      max_swe <-as.numeric(max(x))
      
      # use which() funciton for position tracking
      # nested with max() to have last day of max swe
      dowy <-as.numeric(max(which(x == max_swe)))
      return(dowy)
    }
  }
  
  # calc ms_dowy
  # return NA for values that never reach the 5.1 mm threshold
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

# swe_list[31]
# 
# generate_snow_metric_rasters(swe_list[31], 
#                              snow_metric_function = "wa", 
#                              snow_metric_name = "wa")


# run function using progress bar (pb) multi-core lapply
system.time(raster_list <-pbmclapply(swe_list,
                                     function(x)
                                     generate_snow_metric_rasters(x,
                                                                  snow_metric_function = "wa",
                                                                  snow_metric_name = "wa"),
                                     mc.cores = ncores,
                                     mc.cleanup = TRUE))
