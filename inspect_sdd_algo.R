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


## same for south half of data
end_of_93 <-h5read(swe_list[9], "/SWE", index = list(1:6601,1:5701,355:365))
r <-rast(end_of_93) # convert from matrix to raster
values(r)[values(r) == -32768] <- NA # change no data to NA
ext(r) <-c(-123.3,-117.6,35.4,42) # set extent
crs(r) <-crs(dem) # set crs from DEM raster
plot(r)

