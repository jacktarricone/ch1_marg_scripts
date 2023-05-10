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
get_prism_normals(type="tmean", resolution = "800m", mon = 1:6, keepZip = FALSE)
pd_get_name(prism_archive_ls())

jmean <- prism_archive_subset(
  "tmean", "monthly normals", mon = 1, resolution = "800m"
)
file <-pd_to_file(jmean)
jmean_rast <-rast(file)
plot(jmean_rast)

dem <-rast("./rasters/static/SNSR_DEM.tif")

jmean_rast_c <-crop(jmean_rast, ext(dem))
plot(jmean_rast_c)
