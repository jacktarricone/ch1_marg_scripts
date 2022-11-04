### creating pixel-wise annual max swe raster ###
# november 3th 2022
# jack tarricone

library(rhdf5)
library(terra)
library(parallel)

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

### function for creating max raster
max_raster <- function(swe_list) {
  
  # pull out number of days in given year
  test <-h5ls(swe_list) # contains 3 groups: lat, long, and SCA
  dims <-test$dim[1]
  nday <-as.integer(sub("6601 x 5701 x ","",dims))
  
  # load in borth half of the data cube for RAM purposes
  c1 <-h5read(swe_list, "/SWE", index = list(1:3300,1:5701,1:nday))
  print("c1 read into memory")
  
  ## calculate pixel-wise max
  # returns max value in mm per pixel in the given year
  max_c1 <-as.matrix(apply(c1, c(1,2), max)) 
  print("c1 max calculated")
  rm(c1) # clean up
  
  ## same for south half of data
  c2 <-h5read(swe_list, "/SWE", index = list(3301:6601,1:5701,1:nday))
  print("c2 read into memory")
  max_c2 <-as.matrix(apply(c2, c(1,2), max))
  print("c2 max calculated")
  rm(c2)
  h5closeAll()
  
  #bind chunks together
  full_max <-rbind(max_c1,max_c2)
  r <-rast(full_max) # convert from matrix to raster
  rm(full_max) # trash array
  values(r)[values(r) == -32768] <- NA # change no data to NA
  print("-32768 converted to NA")
  
  # georeference
  ext(r) <-c(-123.3,-117.6,35.4,42) # set extent
  crs(r) <-crs(dem) # set crs from DEM raster

  # name formatting
  name <- gsub(".h5", "", basename(swe_list))
  good_name <- gsub("SN_SWE_", "max_swe_", name)
  
  # save
  setwd("./snow_metric_rasters/max_swe/terra_rasters")
  writeRaster(r, paste0(good_name, ".tif"))
  
  # thank you!
  print(paste0(good_name," has been generated!"))
}

# mcapply function 
# set number of cores to use
ncores <-4

# check list, looks good
swe_list[15:31] 

# run function
system.time(raster_list <-mclapply(swe_list[15:31], 
                                   function(x) max_raster(x),
                                   mc.cores = ncores, 
                                   mc.cleanup = FALSE))

