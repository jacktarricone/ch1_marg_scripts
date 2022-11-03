### creating pixel-wise annual max swe raster ###
# november 3th 2022
# jack tarricone

library(rhdf5)
library(terra)
library(stars)
library(parallel)

# tempdir <- function() { "/Volumes/jt/projects/margulis/temp" }
# tempdir()

#set working directory
setwd("/Users/jacktarricone/ch1_margulis/")

# list hdf swe files
swe_list <-list.files("./swe/hdf", pattern = ".h5", full.names = TRUE)
print(swe_list) # static and wy2015 SCA

## list attributes for swe file
h5ls(swe_list[1]) # contains 3 groups: lat, long, and SCA
h5readAttributes(swe_list[1], name = "SWE") # SWE units = mm

# test reading with raster instead
hmmt <-rast(swe_list[1], subds = "//SWE")
hmmt
plot(t(hmmt[[1]])) # looks okay

# test running matx with app and 8 cores
# system.time(max_test <-app(hmmt, fun=function(i) max(i), cores=10)) # calc max

# No data value to NaN
# system.time(max_test[max_test == -32768 ] <- NA) #remove NA
# plot(t(max_test))

?read_stars

#tif <- system.file("tif/L7_ETMs.tif", package = "stars")
x <-read_stars(swe_list[1])
x$`//SWE`

##
no_cores <- detectCores() - 3
cluster <- makeCluster(no_cores)
result <- parLapply(cluster, docs$text, preProcessChunk)


st_apply(x$`//SWE`, 
         MARGIN = 1:2, 
         FUN = max,
         CLUSTER = cluster,
         PROGRESS = TRUE)# mean band value for each pixel

stopCluster(cluster)

# st_apply(
#   X,
#   MARGIN,
#   FUN,
#   ...,
#   CLUSTER = NULL,
#   PROGRESS = FALSE,
#   FUTURE = FALSE,
#   rename = TRUE,
#   .fname,
#   single_arg = has_single_arg(FUN, list(...)) || can_single_arg(FUN),
#   keep = FALSE
# )


st_apply(x, c("x", "y"), mean) # equivalent to the above
st_apply(x, 3, mean) # mean of all pixels for each band


### list attributes for swe file
h5ls(swe_list[1]) # contains 3 groups: lat, long, and SCA
h5readAttributes(swe_wy16_path, name = "SWE") # SWE units = mm
dims(swe_list[2])
test <-h5ls(swe_list[1]) # contains 3 groups: lat, long, and SCA
test$dim[1]

### function for creating max raster
max_raster <- function( swe_list ) {
  
  
  c1 <-h5read(swe_list[1], "/SWE", index = list(1:10,1:5701,1:365)) #load in 
  c1[ c1[] == -32768 ] <- NA #remove NA
  max_c1 <-as.matrix(apply(c1, c(1,2), max)) #creat matrix with max value on z axis
  rm(c1) 
  
  c2 <-h5read(path, "/SWE", index = list(1001:2000,1:5701,1:365))
  c2[ c2[] == -32768 ] <- NA
  max_c2 <-as.matrix(apply(c2, c(1,2), max))
  rm(c2)
  h5closeAll()
  
  c3 <-h5read(path, "/SWE", index = list(2001:3000,1:5701,1:365))
  c3[ c3[] == -32768 ] <- NA
  max_c3 <-as.matrix(apply(c3, c(1,2), max))
  rm(c3)
  h5closeAll()
  
  c4 <-h5read(path, "/SWE", index = list(3001:4000,1:5701,1:365))
  c4[ c4[] == -32768 ] <- NA
  max_c4 <-as.matrix(apply(c4, c(1,2), max))
  rm(c4)
  h5closeAll()
  
  c5 <-h5read(path, "/SWE", index = list(4001:5000,1:5701,1:365))
  c5[ c5[] == -32768 ] <- NA
  max_c5 <-as.matrix(apply(c5, c(1,2), max))
  rm(c5)
  h5closeAll()
  
  c6 <-h5read(path, "/SWE", index = list(5001:6000,1:5701,1:365))
  c6[ c6[] == -32768 ] <- NA
  max_c6 <-as.matrix(apply(c6, c(1,2), max))
  rm(c6)
  h5closeAll()
  
  c7 <-h5read(path, "/SWE", index = list(6001:6601,1:5701,1:365))
  c7[ c7[] == -32768 ] <- NA
  max_c7 <-as.matrix(apply(c7, c(1,2), max))
  rm(c7)
  h5closeAll()
  
  #bind chunks together
  full_max <-rbind(max_c1,max_c2,max_c3,max_c4,max_c5,max_c6,max_c7)
  rast <-raster(full_max, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66"))
  plot(rast)
  hist(rast)
  
  name <- gsub(".h5", "", hdf_name)
  good_name <- gsub("SN_SWE_", "max_swe_", name)
  
  setwd("/Volumes/jt/projects/margulis/max_rasters/")
  writeRaster(rast, paste0(good_name, ".tif"))
  return(rast)
}


#### apply to hdf list

setwd("/Volumes/jt/projects/margulis/swe/hdf")
files <- list.files(pattern = ".h5")
hdf_list <-mixedsort(sort(files)) #sort in correct order
print(hdf_list)

system.time(raster_list <-lapply(hdf_list, function(x) max_raster(x)))


###### oct 13th 
# packages for doing pixelwise sen's slope

# read in rasters to stack
setwd("/Volumes/jt/projects/margulis/max_rasters/")
list <-list.files()
max_list <-lapply(list, function(x) raster(x))
max_stack <-stack(max_list) 
crs(max_stack)<-"+proj=leac +ellps=clrk66"
plot(max_stack[[9]])
max_stack

# test running on top 1000 rows
top1deg <-crop(max_stack, extent(-123.3, -117.6, 41, 42)) #convert max stack to array
top1deg_stack <-stack(top1deg)
top1deg
plot(top1deg[[9]])


writeRaster(top_5th, "/Volumes/jt/projects/margulis/mk_results/top_5th.tif")
top5 <-stack("/Volumes/jt/projects/margulis/mk_results/top_5th.tif")
crs(top5)<-"+proj=leac +ellps=clrk66"


max_results <-stack("/Volumes/jt/projects/margulis/mk_results/max_results.tif")
#crs(max_results)<-"+proj=leac +ellps=clrk66"
#plot(max_results[[1]])
#max_results

###### mk test
trend.slope <- function(y, p.value.pass = TRUE, z.pass = TRUE, 
                        tau.pass = TRUE, confidence.pass = TRUE, intercept.pass = TRUE) {
  options(warn = -1)
  fit <- EnvStats::kendallTrendTest(y ~ 1)
  fit.results <- fit$estimate[2]
  if (tau.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[1])
  }
  if (intercept.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[3])
  }
  if (p.value.pass == TRUE) {
    fit.results <- c(fit.results, fit$p.value)
  }
  if (z.pass == TRUE) {
    fit.results <- c(fit.results, fit$statistic)
  }
  if (confidence.pass == TRUE) {
    ci <- unlist(fit$interval["limits"])
    if (length(ci) == 2) {
      fit.results <- c(fit.results, ci)
    }
    else {
      fit.results <- c(fit.results, c(NA, NA))
    }
  }
  options(warn = 0)
  return(fit.results)
}

trend.slope2 <- function(y, p.value.pass = TRUE, z.pass = FALSE, 
                        tau.pass = FALSE, confidence.pass = FALSE, intercept.pass = FALSE) {
  options(warn = -1)
  fit <- EnvStats::kendallTrendTest(y ~ 1)
  fit.results <- fit$estimate[2]
  if (tau.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[1])
  }
  if (intercept.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[3])
  }
  if (p.value.pass == TRUE) {
    fit.results <- c(fit.results, fit$p.value)
  }
  if (z.pass == TRUE) {
    fit.results <- c(fit.results, fit$statistic)
  }
  if (confidence.pass == TRUE) {
    ci <- unlist(fit$interval["limits"])
    if (length(ci) == 2) {
      fit.results <- c(fit.results, ci)
    }
    else {
      fit.results <- c(fit.results, c(NA, NA))
    }
  }
  options(warn = 0)
  return(fit.results)
}


# whotle thing
beginCluster(n=3)

system.time(max_results <- clusterR(max_stack, overlay, args=list(fun=trend.slope)))

endCluster()

########### top 5th





?raster.kendall

top1deg_results2
plot(top1deg_results2[[1]])
########

plot(max_results)
max_results_stack <-stack(max_results)
crs(max_results_stack)<-"+proj=leac +ellps=clrk66"

p_value <-max_results[[2]]
slope <-max_results[[1]]
writeRaster(p_value,"/Volumes/jt/projects/margulis/mk_results/p_value.tif")
writeRaster(slope, "/Volumes/jt/projects/margulis/mk_results/slope.tif")

#?cluster
#?system.time

##### testinng with smaller size

#set path and file name for hdf5 SWE file
hdf_path <- "/Volumes/jt/projects/margulis/swe/hdf" #create path
hdf_name <- "SN_SWE_WY2016.h5"
path <-file.path( hdf_path , hdf_name ) 
h5closeAll()
h5readAttributes(path, name = "SWE") #$units = mm

test_chunk <-h5read(path, "/SWE", index = list(1:100, 1:5701, 150:186)) #load in 
test_rast <-stack(brick(test_chunk))

k <- raster.kendall(test_rast, p.value=TRUE, z.value=FALSE,
                    intercept=FALSE, confidence=FALSE,
                    tau=FALSE)

beginCluster(n=3)

system.time(max_results <- clusterR(test_rast, overlay, args=list(fun=trend.slope2)))

endCluster()




#fun_kendall <-function(x){ return(unlist(MannKendall(x)))}
#kendall_result <-calc(test_rast, fun_kendall)
#kendall_result
#plot(kendall_result)
#beginCluster(n=3)
?MannKendall





test_results <- clusterR(test_rast, calc, args=list(fun=trend.slope2))

endCluster()

plot(test_results)
test_results
