### creating pixel-wise annual rasters for:
#### fm_apr1

# may 2, 2023
# jack tarricone

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
snow_metric_name <-"fm_apr1"
snow_metric_name

# mcapply function 
# set number of cores to use
ncores <-3

# check list, looks good
swe_list
fm_apr1_v1 <-function(x, swe_thres = 25.4){
  
  # set threshold
  if (max(x) < swe_thres){
    return(NA)
  } else {x}
  if (length(x) == 365){ # non leap year
    
    # calc cumulative annual melt
    full_year_val_diff <-diff(x)
    total_melt_mm <-abs(sum(full_year_val_diff[full_year_val_diff<0]))
    
    # trim vector to dec 1 - march 31
    ondjfm <-x[1:181]
    
    # find difference between values
    apr1_val_diff <-diff(ondjfm)
    mwa_mm <-abs(sum(apr1_val_diff[apr1_val_diff<0]))
    
    # caculate fraction of melt in percent and round
    fm_frac <-round((mwa_mm/total_melt_mm),2)
    return(fm_frac)
  }
  else{ # leap year
    # calc cumulative annual melt
    full_year_val_diff <-diff(x)
    total_melt_mm <-abs(sum(full_year_val_diff[full_year_val_diff<0]))
    
    # trim vector to dec 1 - march 31
    ondjfm <-x[1:182]
    
    # find difference between values
    apr1_val_diff <-diff(ondjfm)
    mwa_mm <-abs(sum(apr1_val_diff[apr1_val_diff<0]))
    
    # caculate fraction of melt in percent and round
    fm_frac <-round((mwa_mm/total_melt_mm),2)
    return(fm_frac)
  }
}

# run function using progress bar (pb) multi-core lapply
system.time(raster_list <-pbmclapply(swe_list[3:32], 
                                     function(x)
                                     generate_snow_metric_rasters(x, 
                                                                  snow_metric_function = fm_apr1_v1, 
                                                                  snow_metric_name = snow_metric_name),
                                     mc.cores = ncores, 
                                     mc.cleanup = TRUE))
