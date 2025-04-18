# jack tarricone
# november 3, 2022

#### snow metric functions

# functions for calculating different snowpack metrics
# applied on a pixel-wise basis to the margulis et al. 2016 dataset
# swe values are in mm

# list names

function_names <-c("max_swe", "max_swe_dowy","wa","msl","melt_rate","sdd","wa")

#######################################
############   max_swe   ##############
#######################################

##### maximum annual of SWE (mm)

avg_swe_d_mm <-function(x){
  
    swe_per_day <-as.numeric(sum(x)/length(x))
    return(swe_per_day) 
    
}

#######################################
############   max_swe   ##############
#######################################

##### maximum annual of SWE (mm)

max_swe <-function(x, swe_thres){

  # 10 mm (1 cm threshold)  
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      max_swe_mm <-as.numeric(max(x))
      return(max_swe_mm) }
  }

#######################################
############     sdd      #############
#######################################

# snow disappearance date: dowy snow is goes below certain threshold (1 inch (25.4 mm))

# define and calc sdd
sdd <-function(x, swe_thres){
  
  # 10 mm (1 cm)
  if (max(x) < swe_thres){
    return(NA)
  } 
  else{
    dowy <-as.numeric(max(which(x > swe_thres)))
    return(dowy)
  }
}

#######################################
############   max_dowy     ###########
#######################################

###### day of water year that max swe occurs on
###### abbr: max_swe_dowy

max_swe_dowy <-function(x, swe_thres){
  
  # set threshold of 10 mm
  if (max(x) < swe_thres){
    return(NA)
  } 
  else{
    # pull out max value
    max_swe <-as.numeric(max(x))
    
    # use which() funciton for position tracking
    # nested with max() to have last day of max swe
    dowy <-as.numeric(max(which(x == max_swe)))
    return(dowy)
  }
}


#######################################
############      wa       ###########
#######################################

###### winter ablation (mm)
###### calculated from dom

wa <-function(x, swe_thres = 25.4){
  
  max_swe_dowy <-function(x){
    
    # set threshold
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      # pull out max value
      max_swe <-as.numeric(max(x))
      
      # use which() funciton for position tracking
      # nested with max() to have last day of max swe
      dowy <-as.numeric(max(which(x == max_swe)))
      return(dowy)
    }
  }
  
  # calc ms_dowy
  # return NA for values that never reach the 5.1 mm threshold
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

#######################################
############ mwa_djfm_total  ###########
#######################################

###### total swe loss (mm) between dec 1 and march 31

mwa_djfm_total <-function(x, swe_thres){
  
  # set threshold
  if (max(x) < swe_thres){
    return(NA)
  } else {x}
  if (length(x) == 365){ # non leap year
    
    # trim vector to dec 1 - march 31
    djfm <-x[61:181]
    
    # find difference between values
    val_diff <-diff(djfm)
    val_diff
    
    # sum all negative values
    mwa_mm <-abs(sum(val_diff[val_diff<0]))
    mwa_mm
    return(mwa_mm)
  }
  else{ # leap year
    # trim vector to dec 1 - march 31
    djfm <-x[61:182]
    
    # find difference between values
    val_diff <-diff(djfm)
    val_diff
    
    # sum all negative values
    mwa_mm <-abs(sum(val_diff[val_diff<0]))
    mwa_mm
    return(mwa_mm)
  }
}

#######################################
############ mwa_djfm_days  ###########
#######################################

###### number of swe loss days between dec 1 - march 31

mwa_djfm_days <-function(x, swe_thres){
  
  # set threshold
  if (max(x) < swe_thres){
    return(NA)
  } else {x}
  if (length(x) == 365){ # non leap year
    
    # trim vector to dec 1 - march 31
    djfm <-x[61:181]
    
    # find difference between values
    val_diff <-diff(djfm)
    val_diff
    
    # sum all negative values
    mwa_mm <-abs(sum(val_diff[val_diff<0]))
    mwa_days <-sum(val_diff <0)
    return(mwa_days)
  }
  else{ # leap year
    # trim vector to dec 1 - march 31
    djfm <-x[61:182]
    
    # find difference between values
    val_diff <-diff(djfm)
    val_diff
    
    # sum all negative values
    mwa_mm <-abs(sum(val_diff[val_diff<0]))
    mwa_days <-as.integer(sum(val_diff <0))
    return(mwa_days)
  }
}

#######################################
############ mwa_djfm_rate  ###########
#######################################

###### average rate of swe loss (mm/day) per swe loss day between dec 1 - march 31

mwa_djfm_rate <-function(x, swe_thres){
  
  # set threshold
  if (max(x) < swe_thres){
    return(NA)
  } else {x}
  if (length(x) == 365){ # non leap year
    
    # trim vector to dec 1 - march 31
    djfm <-x[61:181]
    
    # find difference between values
    val_diff <-diff(djfm)
    val_diff
    
    # sum all negative values
    mwa_mm <-abs(sum(val_diff[val_diff<0]))
    mwa_days <-sum(val_diff <0)
    mwa_mm_day <-mwa_mm/mwa_days
    return(mwa_mm_day)
  }
  else{ # leap year
    # trim vector to dec 1 - march 31
    djfm <-x[61:182]
    
    # find difference between values
    val_diff <-diff(djfm)
    val_diff
    
    # sum all negative values
    mwa_mm <-abs(sum(val_diff[val_diff<0]))
    mwa_days <-sum(val_diff <0)
    mwa_mm_day <-mwa_mm/mwa_days
    return(mwa_mm_day)
  }
}

#######################################
######      melt_rate_50    ###########
#######################################

## melt rate halfway through melt szn

melt_rate_50 <-function(x, swe_thres){
  
  # define and calc max
  max_swe <-function(x){
    
    # 10 mm (1 cm threshold)  
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      max_swe_mm <-as.numeric(max(x))
      return(max_swe_mm) }
  }
  
  # calc max
  max <-max_swe(x)
  
  # calc half
  half_max <-max/2
  
  # define and calc max_dowy
  max_swe_dowy <-function(x){
    
    # set threshold 10 mm
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      # pull out max value
      max_swe <-as.numeric(max(x))
      
      # use which() funciton for position tracking
      # nested with max() to have last day of max swe
      dowy <-as.numeric(max(which(x == max_swe)))
      return(dowy)
    }
  }
  
  max_dowy <-max_swe_dowy(x)
  
  # define and calc when half of max swe is gone
  max_swe_50 <-function(x){
    
    # set threshold 10 mm
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      # pull out max value
      max <-as.numeric(max(x))
      
      # half of max
      half_max <-max/2
      
      # use which() funciton for position tracking
      # nested with max() to have last day of max swe
      half_dowy <-as.numeric(max(which(x > half_max)))
      return(half_dowy)
    }
  }
  
  half_max_date <-max_swe_50(x)
  
  # subtract for melt date
  half_msl <-half_max_date-max_dowy
  
  # calc melt rate
  melt_rate_mm <-half_max/half_msl
  return(melt_rate_mm)
}


#######################################
######      melt_rate       ###########
#######################################

####### (mm/day)

melt_rate <-function(x, swe_thres){
  
  # define and calc max
  max_swe <-function(x){
    
    # 10 mm (1 cm threshold)  
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      max_swe_mm <-as.numeric(max(x))
      return(max_swe_mm) }
  }
  max <-max_swe(x)
  
  # sub tract the threhold
  max_w_thres <-max-swe_thres
  
  # define and calc max_dowy
  max_swe_dowy <-function(x){
    
    # set threshold 10 mm
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      # pull out max value
      max_swe <-as.numeric(max(x))
      
      # use which() funciton for position tracking
      # nested with max() to have last day of max swe
      dowy <-as.numeric(max(which(x == max_swe)))
      return(dowy)
    }
  }
  dowy <-max_swe_dowy(x)
  
  # define and calc sdd
  sdd <-function(x){
    
    # 10 mm (1 cm)
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      dowy <-as.numeric(max(which(x > swe_thres)))
      return(dowy)
    }
  }
  melt_date <-sdd(x)
  
  # subtract for melt date
  msl <-melt_date-dowy
  
  # calc melt rate
  melt_rate_mm <-max_w_thres/msl
  return(melt_rate_mm)
}


#######################################
######      melt_rate_33    ###########
#######################################

melt_rate_33 <-function(x, swe_thres){
  
  # define and calc max
  max_swe <-function(x){
    
    # 10 mm (1 cm threshold)  
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      max_swe_mm <-as.numeric(max(x))
      return(max_swe_mm) }
  }
  
  # calc max
  max <-max_swe(x)
  
  # calc half
  two_thirds_max <-max-max*(1/3)
  one_third_max <-max*(1/3)
  
  # define and calc max_dowy
  max_swe_dowy <-function(x){
    
    # set threshold 10 mm
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      # pull out max value
      max_swe <-as.numeric(max(x))
      
      # use which() funciton for position tracking
      # nested with max() to have last day of max swe
      dowy <-as.numeric(max(which(x == max_swe)))
      return(dowy)
    }
  }
  
  max_dowy <-max_swe_dowy(x)
  
  # define and calc when half of max swe is gone
  max_swe_33 <-function(x){
    
    # set threshold 10 mm
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      # pull out max value
      max <-as.numeric(max(x))
      
      # use which() funciton for position tracking
      # nested with max() to have last day of max swe
      third_dowy <-as.numeric(max(which(x > two_thirds_max)))
      return(third_dowy)
    }
  }
  
  third_max_date <-max_swe_33(x)
  
  # subtract for melt date
  third_msl <-third_max_date-max_dowy
  
  # calc melt rate
  melt_rate_mm <-one_third_max/third_msl
  return(melt_rate_mm)
}


#######################################
######      melt_rate_25    ###########
#######################################


melt_rate_25 <-function(x, swe_thres){
  
  # define and calc max
  max_swe <-function(x){
    
    # 10 mm (1 cm threshold)  
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      max_swe_mm <-as.numeric(max(x))
      return(max_swe_mm) }
  }
  
  # calc max
  max <-max_swe(x)
  
  # calc half
  three_quarter_max <-max-max*(1/4)
  one_quarter_max <-max*(1/4)
  
  # define and calc max_dowy
  max_swe_dowy <-function(x){
    
    # set threshold 10 mm
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      # pull out max value
      max_swe <-as.numeric(max(x))
      
      # use which() funciton for position tracking
      # nested with max() to have last day of max swe
      dowy <-as.numeric(max(which(x == max_swe)))
      return(dowy)
    }
  }
  
  max_dowy <-max_swe_dowy(x)
  
  # define and calc when half of max swe is gone
  max_swe_25 <-function(x){
    
    # set threshold 10 mm
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      # pull out max value
      max <-as.numeric(max(x))
      
      # use which() funciton for position tracking
      # nested with max() to have last day of max swe
      third_dowy <-as.numeric(max(which(x > three_quarter_max)))
      return(third_dowy)
    }
  }
  
  one_quarter_max_date <-max_swe_25(x)
  
  # subtract for melt date
  quarter_msl <-one_quarter_max_date-max_dowy
  
  # calc melt rate
  melt_rate_mm <-one_quarter_max/quarter_msl
  return(melt_rate_mm)
}



#######################################
############      msl       ###########
#######################################

##### melt season length (days)

msl <-function(x, swe_thres){
  
  # define and calc max
  max_swe <-function(x){
    
    # 10 mm (1 cm threshold)  
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      max_swe_mm <-as.numeric(max(x))
      return(max_swe_mm) }
  }
  max <-max_swe(x)
  
  # sub tract the threhold
  max_w_thres <-max-swe_thres
  
  # define and calc max_dowy
  max_swe_dowy <-function(x){
    
    # set threshold 10 mm
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      # pull out max value
      max_swe <-as.numeric(max(x))
      
      # use which() funciton for position tracking
      # nested with max() to have last day of max swe
      dowy <-as.numeric(max(which(x == max_swe)))
      return(dowy)
    }
  }
  dowy <-max_swe_dowy(x)
  
  # define and calc sdd
  sdd <-function(x){
    
    # 10 mm (1 cm)
    if (max(x) < swe_thres){
      return(NA)
    } 
    else{
      dowy <-as.numeric(max(which(x > swe_thres)))
      return(dowy)
    }
  }
  melt_date <-sdd(x)
  
  # subtract for melt date
  msl_days <-melt_date-dowy
  return(msl_days)
}

#######################################
############      fm_apr1   ###########
#######################################

fm_apr1 <-function(x, swe_thres){
  
  # set threshold
  if (max(x) < swe_thres){
    return(NA)
  } else {x}
  if (length(x) == 365){ # non leap year
    
    # calc cumulative annual melt
    full_year_val_diff <-diff(x)
    total_melt_mm <-abs(sum(full_year_val_diff[full_year_val_diff<0]))
    
    # trim vector to dec 1 - march 31
    ondjfm <-x[1:181]
    
    # find difference between values
    apr1_val_diff <-diff(ondjfm)
    mwa_mm <-abs(sum(apr1_val_diff[apr1_val_diff<0]))
    
    # caculate fraction of melt in percent and round
    fm_frac <-round((mwa_mm/total_melt_mm),2)
    return(fm_frac)
  }
  else{ # leap year
    # calc cumulative annual melt
    full_year_val_diff <-diff(x)
    total_melt_mm <-abs(sum(full_year_val_diff[full_year_val_diff<0]))
    
    # trim vector to dec 1 - march 31
    ondjfm <-x[1:182]
    
    # find difference between values
    apr1_val_diff <-diff(ondjfm)
    mwa_mm <-abs(sum(apr1_val_diff[apr1_val_diff<0]))
    
    # caculate fraction of melt in percent and round
    fm_frac <-round((mwa_mm/total_melt_mm),2)
    return(fm_frac)
  }
}

#########################################
### function for creating rasters ####
#########################################

generate_snow_metric_rasters <-function(swe_list, snow_metric_function, snow_metric_name) {
  
  # reset wd
  setwd("~/ch1_margulis") 
  
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
  saving_location <-list.files("./rasters/snow_metrics/",
                               pattern = paste0("*",snow_metric_name,"$"), 
                               full.names = TRUE)
  # save
  setwd(saving_location)
  writeRaster(r, paste0(good_name, ".tif"))
  
  # thank you!
  print(paste0(good_name," has been generated!"))
}
