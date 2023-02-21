### creating pixel-wise annual rasters for:
#### md
#### for eli, set threshold to 50 mm (5 cm)

# february 20th 2022
# jack tarricone

library(rhdf5)
library(terra)
library(parallel)
library(pbmcapply)

#set working directory
setwd("/Users/jacktarricone/ch1_margulis/")

# list hdf swe files
swe_list <-list.files("./swe/hdf", pattern = ".h5", full.names = TRUE)
print(swe_list) # static and wy2015 SCA

## list attributes for swe file
h5ls(swe_list[1], datasetinfo = TRUE) # contains 3 groups: lat, long, and SCA
h5readAttributes(swe_list[1], "SWE") # SWE units = mm

# read in dem for georeferencing
static_list <-list.files("./static/rasters", pattern = ".tif", full.names = TRUE)
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
snow_metric_name <-function_names[5]
snow_metric_name

# mcapply function 
# set number of cores to use
ncores <-2

# check list, looks good
swe_list

# subsest list to 2011-206
eli_swe_years <-swe_list[27:32]
eli_swe_years

# change melt date function so one variable and threshold is 50 mm
eli_md <-function(x){
  
  # x = swe data time series
  # threshold = number in (mm) that should be considered
  
  max_swe_dowy <-function(x){
    
    # set threshold so that pixel is NA if never hiss 5 mm
    if (max(x) < 50){
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
    melt_out_dowy_vect <-which(x < 50 & dowy_vect > ms_dowy)
    
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

# run function using progress bar (pb) multi-core lapply
system.time(raster_list <-pbmclapply(eli_swe_years, 
                                     function(x)
                                     generate_snow_metric_rasters(x, 
                                                                  snow_metric_function = eli_md, 
                                                                  snow_metric_name = snow_metric_name),
                                     mc.cores = ncores, 
                                     mc.cleanup = TRUE))



