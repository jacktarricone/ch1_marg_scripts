library(daymetr)
library(terra)
library(raster)
library(ncdf4)
library(raster)

setwd("~/ch1_margulis")

# load in snsr shape
snsr <-vect("./vectors/snsr_shp.gpkg")
ext(snsr)
?download_daymet_ncss
# test download
download_daymet_ncss(location = c(41.8758638835292,-123.065041220838,35.4219961410279,-117.651990878793),
                     start = 1995,
                     end = 2016,
                     param = "srad",
                     path = "./rasters/daymet/")

# test download
download_daymet_ncss(location = c(41.8758638835292,-123.065041220838,35.4219961410279,-117.651990878793),
                     start = 1995,
                     end = 2016,
                     param = "tmax",
                     frequency = "monthly",
                     path = "./rasters/daymet/")

# format data, so dumb
tmax_rast <-raster::stack("./rasters/daymet/tmax/tmax_monavg_1995_ncss.nc")
crs(tmax_rast) <-"+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=km +no_defs" # this works!!
tmax_repoj <- raster::projectRaster(tmax_rast, crs = "+init=epsg:4326")
tmax_rast <-rast(tmax_repoj) 
plot(test_rast[[9]])
plot(snsr, add = TRUE)
writeRaster(test_rast[[9]], "./rasters/daymet/tmax_test.tif", overwrite = TRUE)

srad_v1 <-project(srad, crs(snsr))
ext(srad_v1) <-ext(snsr)
plot(srad_v1)
srad_v2 <-mask(srad_v1, snsr)
plot(srad_v2)
writeRaster(srad_v2, "./rasters/daymet/daymet_srad_1990.tif")

srad_list <-list.files("./rasters/daymet", pattern = "srad", full.names = TRUE)
srad_list

test <-raster(srad_list[1])
test

library(ncdf4)

test <-nc_open(srad_list[1])
test

srad_nc <-ncvar_get(test, "srad")
srad_nc_rot_v1 <-nat::flip(srad_nc, direction = "y")
srad_nc_rot <-rotate(srad_nc_rot_v1, -90)
srad_mean <-apply(srad_nc_rot, c(1,2), mean)
srad_nc_rast <-rast(srad_mean)
ext(srad_nc_rast) <-ext(-125.33127456073, -116.028778808886, 34.3884392364541, 42.8389355269087)
crs(srad_nc_rast) <-crs("+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs")
plot(srad_nc_rast)
srad_nc_rast
srad_nc_rast_v1 <-project(srad_nc_rast, crs(snsr))
snsr_r <-project(snsr, crs(srad_nc_rast_v1))
plot(srad_nc_rast)
plot(snsr_r, add = TRUE)

tif_convert <-function(x){
  srad_raster <-raster(x)
  srad_repoj <-projectRaster(srad_raster, crs=crs(snsr))
  name_v1 <-basename(x)
  name <-gsub("*s.nc","s.tif",name_v1)
  writeRaster(srad_repoj, paste0("./rasters/daymet/",name))
}
lapply(srad_list,tif_convert)



rotate <- function(A, grad=90){
  
  # ========================================================================
  #
  # Description:
  # 
  # With this funtion is it possible to rotate a vector, matrix or an array.
  # A      Has to be a vector, matrix or an array.
  # grad   Can set to 0, 90, 180, 270, -90, -180 or -270.
  #
  # Examples:
  #
  # x <- c(1:5)
  # rotate(x,270)
  #
  # A <- matrix(1:12,ncol=4,byrow=TRUE)
  # rotate(A)
  #
  # A <- array(1:20, c(2,5,2))
  # rotate(A, 180)
  # ========================================================================
  
  gradLogical = TRUE
  if (grad != 0 && grad != 90 && grad != 180 && grad != 270 && 
      grad != -90 && grad != -180 && grad != -270 ){
    
    cat("WARNING: grad=", grad, " is not supported. The surrender value is 'NULL'.\n", sep="")
    gradLogical=FALSE
    rotA <- NULL
  }
  
  if (grad<0)
    grad <- 360+grad
  
  dimA  <- dim(A)
  ldimA <- length(dimA)
  
  if ( (!(is.null(dimA)) && ldimA != 2 && ldimA != 3) || is.null(A) ){
    cat("WARNING: 'A' has to be a vector, a matrix or an array.\n") 
    cat("The surrender value is 'NULL'.\n")
    gradLogical=FALSE
    rotA <- NULL
  }
  
  #########################################################################
  # Case 1: A is a vector
  if(gradLogical){
    if (is.null(dimA)){
      dimA  <- length(A)
      if(dimA>1){
        if(grad==90){
          cat("NOTE: The type of the surrender data is changed from vector to matrix. \n")
          rotA=matrix(A, nrow=dimA, ncol=1)
          rotA=matrix(rotA[dimA:1,],nrow=dimA)
        } else if (grad==180) {
          rotA=A[dimA:1]
        } else if(grad==270) {
          cat("NOTE: The type of the surrender data is changed from vector to matrix. \n")
          rotA=matrix(A, nrow=dimA, 1)
        } else
          rotA=A
      } else if(dimA==1) rotA=A
      else stop("Unsupported data typ.")
      
    } else if (ldimA==2){
      #########################################################################
      # Case 2: A is a matrix
      nrowA <- dimA[1]
      ncolA <- dimA[2]
      if(grad==90){
        rotA <- aperm(A,c(2,1))
        rotA[1:ncolA,] <- rotA[ncolA:1,]
      } else if (grad==180) {
        if(nrowA==1 || ncolA==1)
          rotA <- matrix(A[,ncolA:1],nrow=nrowA ,ncol=ncolA)
        else
          rotA <- A[,ncolA:1]
        rotA[1:nrowA,] <- rotA[nrowA:1,]
      } else if(grad==270) {
        rotA <- aperm(A,c(2,1))
        rotA[,1:nrowA] <- rotA[,nrowA:1]
      } else
        rotA <- A
      
    } else if (ldimA==3){
      #########################################################################
      # Case 3: A is an 3-dimensional array
      nrowA <- dimA[1]
      ncolA <- dimA[2]
      permVec <- c(2,1, 3:length(dimA))
      if(grad==90){
        rotA <- aperm(A,permVec)
        rotA <- rotA[ncolA:1,,]
      } else if (grad==180) {
        rotA <- A[,ncolA:1,]
        rotA <- rotA[nrowA:1,,]
      } else if(grad==270) {
        rotA <- aperm(A,permVec)
        rotA <- rotA[,nrowA:1,]
      } else
        rotA=A
    }
  }
  
  return(rotA)
}
