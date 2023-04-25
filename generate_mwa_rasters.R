### creating pixel-wise annual rasters for:
#### mwa

# april 25, 2023
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
snow_metric_name <-"mwa_djfm_total"
snow_metric_name

# mcapply function 
# set number of cores to use
ncores <-2

# check list, looks good
swe_list

mwa_djfm_total_v1 <-function(x, swe_thres = 25.4){
  
  # set threshold
  if (max(x) < swe_thres){
    return(NA)
  } else {x}
  if (length(x) == 365){ # non leap year
    
    # trim vector to dec 1 - march 31
    djfm <-x[61:181]
    
    # find difference between values
    val_diff <-diff(djfm)
    val_diff
    
    # sum all negative values
    mwa_mm <-abs(sum(val_diff[val_diff<0]))
    mwa_mm
    return(mwa_mm)
  }
  else{ # leap year
    # trim vector to dec 1 - march 31
    djfm <-x[61:182]
    
    # find difference between values
    val_diff <-diff(djfm)
    val_diff
    
    # sum all negative values
    mwa_mm <-abs(sum(val_diff[val_diff<0]))
    mwa_mm
    return(mwa_mm)
  }
}


# run function using progress bar (pb) multi-core lapply
system.time(raster_list <-pbmclapply(swe_list[3:32], 
                                     function(x)
                                     generate_snow_metric_rasters(x, 
                                                                  snow_metric_function = mwa_djfm_total_v1, 
                                                                  snow_metric_name = snow_metric_name),
                                     mc.cores = ncores, 
                                     mc.cleanup = TRUE))
