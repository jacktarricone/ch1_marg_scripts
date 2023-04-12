library(ggplot2)
setwd("~/ch1_margulis/")

#########################
#########################
## read in snotel data ##
#########################
#########################

snotel_df <-read.csv("./csvs/snotel_df_v2.csv")
snotel_df <-subset(snotel_df, select=-c(X, network)) # move bad one
head(snotel_df)

#########################
#########################
###  read in SNSR data ##
#########################
#########################

# read in csvs
snsr_snotel_list <-sort(list.files("./csvs/snsr_snotel_data", full.names = TRUE))
snsr_snotel_data <-lapply(snsr_snotel_list, read.csv)

# swe_dat <-read.csv("./csvs/snsr_snotel_data/ECHO_PEAK_swe_2003.csv")
# snotel_df <-read.csv("./csvs/snotel_df_v2.csv")
# ep_03 <-filter(snotel_df, site_name == "echo peak " & waterYear == 2003)
# x <-ep_03$snotel_swe_mm
# x <-swe_dat$snsr_swe_mm
# plot(x)

ggplot() +
  geom_point(data = ep_03, aes(y = snotel_swe_mm, x = seq(1,365,1))) +
  geom_point(data = swe_dat, aes(y = snsr_swe_mm, x = seq(1,365,1)), color = 'red') +
  theme_classic(12)

swe_thres <-25.4

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


melt_rate_50(ep_03$snotel_swe_mm, 25.4)
#melt_rate(x, 25.4)

melt_rate_50(swe_dat$snsr_swe_mm, 25.4)
#melt_rate(css_03$snotel_swe_mm, 25.4)



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

swe_thres <-25.4

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

head(snotel_df)

library(dplyr)
# calc metric 
melt_rate_df <-as.data.frame(snotel_df %>%
                               group_by(site_name, waterYear) %>%
                               summarise(melt_rate_snotel_0 = melt_rate_50(snotel_swe_mm, swe_thres = 0),
                                         melt_rate_snsr_0   = melt_rate_50(snsr_swe_mm, swe_thres = 0),
                                         melt_rate_snotel_25.4 = melt_rate_50(snotel_swe_mm, swe_thres = 25.4),
                                         melt_rate_snsr_25.4   = melt_rate_50(snsr_swe_mm, swe_thres = 25.4),
                                         melt_rate_snotel_50.8 = melt_rate_50(snotel_swe_mm, swe_thres = 50.8),
                                         melt_rate_snsr_50.8   = melt_rate_50(snsr_swe_mm, swe_thres = 50.8)))

melt_rate_df[sapply(melt_rate_df, is.infinite)] <- NA


melt_rate_50(x, 25.4)
melt_rate(x, 25.4)

plot(x)

mwa_djfm_total(x, 25.4)
mwa_djfm_days(x, 25.4)
mwa_djfm_rate(x, 25.4)

