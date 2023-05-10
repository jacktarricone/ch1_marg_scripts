# prism datadownload
# jack t
# may 10th, 2023

library(prism)
library(raster)
library(terra)

# set paths
setwd("~/ch1_margulis")
prism_set_dl_dir("./rasters/prism")

# download normals
get_prism_normals(type="tmean", resolution = "800m", mon = 1:12, keepZip = FALSE)
get_prism_normals(type="tmax", resolution = "800m", mon = 1:12, keepZip = FALSE)
get_prism_normals(type="tmean", resolution = "800m", mon = 1:12, keepZip = FALSE)
pd_get_name(prism_archive_ls())

jan_mean <- prism_archive_subset(
  "tmean", "monthly normals", mon = 1, resolution = "800m"
)


file <-pd_to_file(jmean)
rast <-rast(file)
plot(rast)

# load in dem for formatting
dem <-rast("./rasters/static/SNSR_DEM.tif")

# reproject and crop
rast_v1 <-project(rast, crs(dem))
rast_v2 <-crop(rast_v1, ext(dem))
plot(rast_v2)

# resample and mask
rast_v3 <-resample(rast_v2, dem, method = 'bilinear')
rast_v4 <-mask(rast_v3, dem, maskvalue = NA)
plot(rast_v4)
writeRaster(rast_v4, "./rasters/prism/prism_tmean_test.tif")
hist(rast_v4, breaks = 100)
