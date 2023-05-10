library(terra)
library(ncdf4)

muss_test <-nc_open("~/Downloads/meltTrends_analysisNCC_scripts 2/data/snowPillowSWE_westernNA_level2_ncc.nc")
muss_test

names <-ncvar_get(muss_test, "agency")
siteID <-ncvar_get(muss_test, "siteID")
swe_array <-ncvar_get(muss_test, "swe_level2")
lat_array <-ncvar_get(muss_test, "latitude")
lon_array <-ncvar_get(muss_test, "longitude")
ele_array <-ncvar_get(muss_test, "elevation")
date_matlab <-ncvar_get(muss_test, "date_matlab")

head(swe_array)
dim(swe_array)
plot(swe_array[,1,52])
