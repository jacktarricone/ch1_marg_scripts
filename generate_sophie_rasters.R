### creating pixel-wise annual rasters for:
#### max_swe

# november 3th 2022
# jack tarricone

library(rhdf5)
library(terra)
library(parallel)
library(pbmcapply)

#set working directory
setwd("/Users/jtarrico/ch1_margulis/")

# list hdf swe files
swe_list <-list.files("./swe/hdf", pattern = ".h5", full.names = TRUE)
print(swe_list) # static and wy2015 SCA

## list attributes for swe file
h5ls(swe_list[1], datasetinfo = TRUE) # contains 3 groups: lat, long, and SCA
h5readAttributes(swe_list[1], "SWE") # SWE units = mm

# read in dem for georeferencing
static_list <-list.files("./rasters/static/", pattern = ".tif", full.names = TRUE)
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
snow_metric_name <-"avg_swe_d_mm"

# mcapply function 
# set number of cores to use
ncores <-4

# check list, looks good
swe_list

# run function using progress bar (pb) multi-core lapply
# make sure to give it proper metric name and function
system.time(raster_list <-pbmclapply(swe_list[2:32], 
                                     function(x)
                                     generate_snow_metric_rasters(x, 
                                                                  snow_metric_function = avg_swe_d_mm, 
                                                                  snow_metric_name = snow_metric_name),
                                     mc.cores = ncores, 
                                     mc.cleanup = TRUE))

# list new rasters
list <-list.files("./rasters/snow_metrics/avg_swe_d_mm", pattern = ".tif", full.names = T)
stack <-rast(list)
stack

# define fun
metric_mean <-function(x){terra::mean(x, na.rm = TRUE)}
mean <-app(stack, fun = metric_mean, cores=14)
plot(mean)
writeRaster(mean,"./rasters/snow_metric_averages/avg_swe_d_mm.tif")
