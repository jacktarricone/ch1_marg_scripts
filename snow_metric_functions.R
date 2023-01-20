# jack tarricone
# november 3, 2022

#### snow metric functions

# functions for calculating different snowpack metrics
# applied on a pixel-wise basis to the margulis et al. 2016 dataset
# swe values are in mm

# list names

function_names <-c("scf","sdd","max_swe_dowy","wa")

#######################################
############   max_swe   ##############
#######################################

# maximum annual of SWE
# abbr: max_swe

# just max...

#######################################
############     scf      #############
#######################################

# create snow cover frequency function
# number of days there is snow on the ground vs total days in a given year
# set threshold to 5.1 mm or .2 inches because thats the accuracy a SNTOEL site has
# this will also reduce the long tails seen on the data and unrealistically large scf number
# abbr: scf

scf <-function(x){
  
  length(which(x > 5.1))/length(x)
  
} 

#######################################
############     sdd      #############
#######################################

# snow disappearance date: dowy snow is gone
# need 14 days of continuous snow cover to be considered
# abbr: sdd

sdd <-function(x){
  
  # return 0 for values that never reach the 5.1 mm threshold
  if (max(x) < 5.1){
    return(0)
  } else {
    # use which function to index the days that meet condition
    # reverse the vector
    days_vector <-as.numeric(rev(which(x > 5.1)))
    
    # take the difference of each vector point and make positive
    # we do this bc we are looking for the first point that meets condition
    # and does this for 14 consecutive days
    diff_vect <-diff(days_vector*-1)
    
    # use rle function to calc the lengths of continuous number sequences
    # if there isnt a 14 days with continuous snow cover, make pixel 0
    # this fixes the code bc accounts for pixels which may have had 10 days of sc
    
    # which therefore was out of the previous condition set and caused the error
    # found this about by testing different areas, and saw was more in east half
    result <- rle(diff_vect)
    if (max(result$lengths) < 14){
      return(0)
    } else{
      #represents place on rle vector where condition is met
      meets <-as.numeric(min(which(result$lengths >= 14)))
      
      #if its the first rle value, this means day 365 so max of days_vecto0r
      if (meets == 1) {
        sdd1 <- max(days_vector)
        return(sdd1)
      } else {
        #calculate legnth needed to be cut off days vector from rle
        values_to_cut <-result$lengths[1:(meets-1)]
        #sum the lengths
        index_values_until_start <-as.numeric(sum(values_to_cut))
        #subtract that many values off to get your SDD
        sdd2 <-max(days_vector[-c(1:index_values_until_start)])
        return(sdd2)
      }
    }
  }
}

#######################################
############   max_dowy     ###########
#######################################

# day of water year that max swe occurs on
# abbr: max_swe_dowy


max_swe_dowy <-function(x){
  
  # set threshold
  if (max(x) < 5.1){
    return(NA)
    
  } 
  else{
    # pull out max value
    max_swe<-as.numeric(max(x))
    
    # use which() funciton for position tracking
    # nested with max() to have last day of max swe
    dowy <-as.numeric(max(which(x == max_swe)))
    return(dowy)
  }
}


#######################################
############      mwa       ###########
#######################################

mwa <-function(x){
  
  # define max_swe_dowy
  max_swe_dowy <-function(x){
    if (max(x) < 5.1){
      return(NA)
    } 
    else{
      max_swe <-as.numeric(max(x))
      dowy <-as.numeric(max(which(x == max_swe)))
      return(dowy)} 
  }
  
  # calc ms_dowy
  # return 0 for values that never reach the 5.1 mm threshold
  if (is.na(max_swe_dowy(x))){
    return(NA)
  } else {
    ms_dowy <-max_swe_dowy(x)
    
    # trim vector to that date
    before_max_swe <-x[1:ms_dowy]
    
    # find difference between values
    val_diff <-diff(before_max_swe)
    
    # sum all negative values
    mwa_mm <-abs(sum(val_diff[val_diff<0]))
    return(mwa_mm)
  }
}


#########################################
### function for creating rasters ####
#########################################

generate_snow_metric_rasters <- function(swe_list, snow_metric_function, snow_metric_name) {
  
  # reset wd
  setwd("/Users/jacktarricone/ch1_margulis/") 
  
  # pull out number of days in given year
  test <-h5ls(swe_list) # contains 3 groups: lat, long, and SCA
  dims <-test$dim[1]
  nday <-as.integer(sub("6601 x 5701 x ","",dims))
  
  # load in borth half of the data cube for RAM purposes
  c1 <-h5read(swe_list, "/SWE", index = list(1:3300,1:5701,1:nday))
  print("c1 read into memory")
  
  ## calculate pixel-wise max
  # returns max value in mm per pixel in the given year
  max_c1 <-as.matrix(apply(c1, c(1,2), snow_metric_function)) 
  print("c1 max calculated")
  rm(c1) # clean up
  
  ## same for south half of data
  c2 <-h5read(swe_list, "/SWE", index = list(3301:6601,1:5701,1:nday))
  print("c2 read into memory")
  max_c2 <-as.matrix(apply(c2, c(1,2), snow_metric_function))
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
  good_name <- gsub("SN_SWE", snow_metric_name, name)
  
  # set saving director to correct folder
  # doesn't need to change for each metric bc include at top of script
  saving_location <-list.files("./snow_metric_rasters/terra_rasters",
                               pattern = paste0("*",snow_metric_name,"$"), 
                               full.names = TRUE)
  # save
  setwd(saving_location)
  writeRaster(r, paste0(good_name, ".tif"))
  
  # thank you!
  print(paste0(good_name," has been generated!"))
}
