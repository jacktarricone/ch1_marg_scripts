# comparing max swe to SNOTELS

library(dplyr)
library(snotelr)
library(lubridate)

setwd('~/ch1_margulis')

#########################
#########################
## read in snotel data ##
#########################
#########################

# bring in real snotel locations
snotel_locs <-read.csv("./csvs/snsr_snotels.csv")
head(snotel_locs)

# create list to loop the dataframes into
snotel_df_list <-as.list(rep(NA, nrow(snotel_locs)))
head(snotel_df_list)

# read in SNOTEL CSVs using ID number and name from meta data
for (i in seq_along(snotel_locs$Site_Name)){
  snotel_df_list[[i]] <-as.data.frame(snotel_download(snotel_locs$Site_ID[i], path = tempdir(), internal = TRUE))
}

# convert from list of dfs to one big one
snotel_df <-bind_rows(snotel_df_list, .id = "network")
snotel_df$date <-ymd(snotel_df$date)

# add water year
colnames(snotel_df)[12] <-"Date"
colnames(snotel_df)[13] <-"snotel_swe_mm"
snotel_df <-dataRetrieval::addWaterYear(snotel_df)
head(snotel_df)

# filter to start at 85
snotel_df <-filter(snotel_df, start <= as.Date("1984-10-01"))
head(snotel_df)

# one ones that don't have the full time series
snotel_df <-filter(snotel_df, waterYear >= 1985 & waterYear <= 2016)
head(snotel_df)

write.csv(snotel_df, "./csvs/snotel_df.csv")
