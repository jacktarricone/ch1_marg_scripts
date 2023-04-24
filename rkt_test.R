### rkt test with rando sample
# april 21nd 2023
# jack tarricone

library(terra)
library(dplyr)
library(data.table)
library(rkt)

# set working directory
setwd("~/ch1_margulis")

# bring in shape files
snsr <-vect("./vectors/snsr_shp.gpkg")
snsr_basins <-vect("./vectors/ca_basins/snsr_all_basins.shp")
american <-vect("./vectors/ca_basins/american.gpkg")

##############
##### max ####
##############

# # # load in stack
# max_stack <-rast("./rasters/snow_metrics/max_swe/max_stack_f_25mm_27obs.tif")
# american_stack <-crop(mask(max_stack, american), american)
# american_stack
# 
# # make df
# american_stack_df <-as.data.table(american_stack, na.rm = TRUE, xy = TRUE, cell = TRUE)
# 
# # change col names
# years <-1985:2016
# colnames(american_stack_df)[4:ncol(american_stack_df)] <-years
# 
# # meeeeelt
# american_df_long <-melt(american_stack_df, id.vars=1:3, measure.vars=4:35)
# colnames(american_df_long)[4:5] <-c("wy","max_swe_mm")
# 
# # quick format, save
# american_df_long$max_swe_m <-american_df_long$max_swe_mm/1000
# head(american_df_long)
# fwrite(american_df_long, "./csvs/american_stack_df_long.csv")

# max_stack_df <-as.data.table(max_stack, na.rm = TRUE, xy = TRUE, cell = TRUE)
# 
# # change col names
# years <-1985:2016
# colnames(max_stack_df)[4:ncol(max_stack_df)] <-years
# 
# #
# max_df_long <-melt(max_stack_df, id.vars=1:3, measure.vars=4:35)
# colnames(max_df_long)[4:5] <-c("wy","max_swe_mm")
# max_df_long$max_swe_m <-max_df_long$max_swe_mm/1000
# head(max_df_long)
# fwrite(max_df_long, "./csvs/max_stack_df_long.csv")


# read max df 
american_df <-fread("./csvs/american_stack_df_long.csv")
american_df$cat <-rep(1,nrow(american_df))
hist(american_df$max_swe_m, breaks = 100)
head(american_df)

# take a random sample
american_sample <-american_df[sample(.N, 100000)]
hist(american_sample$max_swe_m, breaks = 100)

# rkt test
rkt_results <-rkt(american_sample$wy,        # time vector of years
                       american_sample$max_swe_mm, # max_percent data 
                       american_sample$cat,       # 'block' aka class variable
                       correct = TRUE,
                       rep = "m") 

# this runs!!
print(rkt_results)
