library(ggplot2)
library(dplyr)

setwd("~/ch1_margulis")

#########################
#########################
## read in snotel data ##
#########################
#########################

snotel_df <-read.csv("./csvs/snotel_df_v3.csv")
snotel_df <-subset(snotel_df, select=-c(X)) # move bad one
head(snotel_df)

#########################
#########################
###  read in SNSR data ##
#########################
#########################

# read in csvs
snsr_snotel_list <-sort(list.files("./csvs/snsr_snotel_data", full.names = TRUE))
snsr_snotel_data <-lapply(snsr_snotel_list, read.csv)

swe_dat <-read.csv("./csvs/snsr_snotel_data/FALLEN_LEAF_swe_1993.csv")
# snotel_df <-read.csv("./csvs/snotel_df_v2.csv")
# ep_03 <-filter(snotel_df, site_name == "echo peak " & waterYear == 2003)
x <-swe_dat$snsr_swe_mm
plot(x)

# ggplot() +
#   geom_point(data = ep_03, aes(y = snotel_swe_mm, x = seq(1,365,1))) +
#   geom_point(data = swe_dat, aes(y = snsr_swe_mm, x = seq(1,365,1)), color = 'red') +
#   theme_classic(12)

swe_thres <-25.4

###### mid winter ablation (mm)
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
    fm_percent <-round((mwa_mm/total_melt_mm),2)
    return(fm_percent)
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
    fm_percent <-round((mwa_mm/total_melt_mm),2)
    return(fm_percent)
  }
}

# calc metric 
fm_df_v2 <-as.data.frame(snotel_df %>%
                      group_by(site_name, waterYear) %>%
                               summarise(fm_apr1_snotel = fm_apr1(snotel_swe_mm, swe_thres = 25.4)))

hist(fm_df_v2$fm_apr1_snotel, breaks = 50)

fallen_2013 <-filter(snotel_df, site_name == "fallen leaf " & waterYear == 2013)
plot(fallen_2013$snotel_swe_mm)
