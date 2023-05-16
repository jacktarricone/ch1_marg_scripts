# prism datadownload
# jack t
# may 10th, 2023

library(prism)
library(raster)
library(terra)

# set paths
setwd("~/ch1_margulis")
prism_set_dl_dir("./rasters/prism")
?prism
# download normals
get_prism_normals(type="tmean", resolution = "800m", mon = 1:12, keepZip = FALSE)
get_prism_normals(type="tmax", resolution = "800m", mon = 1:12, keepZip = FALSE)
get_prism_normals(type="tmean", resolution = "800m", mon = 1:12, keepZip = FALSE)
pd_get_name(prism_archive_ls())

# get annuals
get_prism_monthlys(type = "tmean", year = 1985:2014, mon = 1, keepZip = FALSE)
?get_prism_monthlys

# pull out months
oct_mean <- prism_archive_subset("tmean", 
                                 "monthly normals", 
                                 mon = 10, resolution = "800m")

nov_mean <- prism_archive_subset("tmean", 
                                 "monthly normals", 
                                 mon = 11, resolution = "800m")

dec_mean <- prism_archive_subset("tmean", 
                                 "monthly normals", 
                                 mon = 12, resolution = "800m")

jan_mean <- prism_archive_subset("tmean", 
                                 "monthly normals", 
                                 mon = 1, resolution = "800m")

feb_mean <- prism_archive_subset("tmean", 
                                 "monthly normals", 
                                 mon = 2, resolution = "800m")

mar_mean <- prism_archive_subset("tmean", 
                                 "monthly normals", 
                                 mon = 2, resolution = "800m")

# bind files
files <-pd_to_file(c(oct_mean,nov_mean,dec_mean,jan_mean,feb_mean,mar_mean))
stack <-rast(files)
plot(stack)

# calc mean
mean_ondjfm <-app(stack, mean)
writeRaster(mean_ondjfm, "./rasters/prism/conus_test.tif")

# load in dem for formatting
dem <-rast("./rasters/static/SNSR_DEM.tif")

# reproject and crop
rast_v1 <-project(mean_ondjfm, crs(dem))
rast_v2 <-crop(rast_v1, ext(dem))
plot(rast_v2)

# resample and mask
rast_v3 <-resample(rast_v2, dem, method = 'bilinear')
rast_v4 <-mask(rast_v3, dem, maskvalue = NA)
plot(rast_v4)
hist(rast_v4, breaks = 100)

3300*2700

# save
writeRaster(rast_v4, "./rasters/prism/prism_tmean_snsr_ondjfm.tif")
