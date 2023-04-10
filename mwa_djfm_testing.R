swe_dat <-read.csv("./csvs/rename_snsr_snotel_data/BLUE_LAKES_swe_2016.csv")
x <-swe_dat$snsr_swe_mm

###### mid winter ablation (mm)

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

mwa_djfm_total(x, 25.4)
mwa_djfm_days(x, 25.4)
mwa_djfm_rate(x, 25.4)

