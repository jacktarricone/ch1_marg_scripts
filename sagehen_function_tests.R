# testing making metric for
# date at which fsca goes below certain (15%?) threshold

library(rhdf5)
library(terra)
library(ggplot2)

#######################################################
##### code for bringing block to test functions on
#######################################################

# reset wd
setwd("~/ch1_margulis") 

# metric creating with this script
snow_metric <-"fsca_thres"

# list sca files
swe_list <-list.files("./swe/hdf", full.names = TRUE)
print(swe_list) # static and wy2015 SCA

# set pth to biggest year and smallest year
path_1993 <-swe_list[9]
path_2015 <-swe_list[31]
path_2016 <-swe_list[32]

# get swe data
sagehen_swe_wy93 <-h5read(path_1993, "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent
sagehen_swe_wy15 <- h5read(path_2015, "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent
sagehen_swe_wy16 <- h5read(path_2016, "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent

# load in functions
# this file has all the snow metric functions and the raster creation one
url <-"https://raw.githubusercontent.com/jacktarricone/ch1_marg_scripts/main/snow_metric_functions.R"
devtools::source_url(url)

######################
###### melt_rate #####
######################

# 1993
melt_rate_wy93 <-apply(sagehen_swe_wy93, c(1,2), function(x) melt_rate(x, 25.4))
plot(melt_rate_wy93)

# 2015
melt_rate_wy15 <-rast(apply(sagehen_swe_wy15, c(1,2), melt_rate))
plot(melt_rate_wy15)

# 2016
melt_rate_wy16 <-rast(apply(sagehen_swe_wy16, c(1,2), melt_rate))
plot(melt_rate_wy16)

######################
######    sdd    #####
######################

# 1993
sdd_wy93 <-rast(apply(sagehen_swe_wy93, c(1,2), function(x) sdd(x, 25.4)))
plot(sdd_wy93)

# 2015
sdd_wy15 <-rast(apply(sagehen_swe_wy15, c(1,2), function(x) sdd(x, 25.4)))
plot(sdd_wy15)

# 2016
sdd_wy16 <-rast(apply(sagehen_swe_wy16, c(1,2), function(x) sdd(x, 25.4)))
plot(sdd_wy16)

######################
######    msl    #####
######################

# 1993
msl_wy93 <-rast(apply(sagehen_swe_wy93, c(1,2), msl))
plot(msl_wy93)

# 2015
msl_wy15 <-rast(apply(sagehen_swe_wy15, c(1,2), msl))
plot(msl_wy15)

# 2016
msl_wy16 <-rast(apply(sagehen_swe_wy16, c(1,2), msl))
plot(msl_wy16)

#########################
###### max_swe_dowy #####
#########################

# 1993
max_swe_dowy_wy93 <-rast(apply(sagehen_swe_wy93, c(1,2), max_swe_dowy))
plot(max_swe_dowy_wy93)

# 2015
max_swe_dowy_wy15 <-rast(apply(sagehen_swe_wy15, c(1,2), max_swe_dowy))
plot(max_swe_dowy_wy15)

# 2016
max_swe_dowy_wy16 <-rast(apply(sagehen_swe_wy16, c(1,2), max_swe_dowy))
plot(max_swe_dowy_wy16)

#####################
###### max_swe ######
#####################

# 1993
max_swe_wy93 <-rast(apply(sagehen_swe_wy93, c(1,2), max_swe))
plot(max_swe_wy93)

# 2015
max_swe_wy15 <-rast(apply(sagehen_swe_wy15, c(1,2), max_swe))
plot(max_swe_wy15)

# 2016
max_swe_wy16 <-rast(apply(sagehen_swe_wy16, c(1,2), max_swe))
plot(max_swe_wy16)



