# create snotel dataframe

library(dplyr)
library(snotelr)
library(lubridate)

setwd('~/ch1_margulis')

#########################
#########################
## read in snotel data ##
#########################
#########################

# read in snsr shp
snsr_shp <-vect("./vectors/snsr_shp.gpkg")
plot(snsr_shp)

# bring in real snotel locations
snotel_keith_df <-read.csv("./csvs/SNOTEL_MASTER_pt_update.csv")
head(snotel_keith_df)

# create shape file of locations
snotel_locs <-vect(snotel_keith_df, geom = c("Longitude","Latitude"), crs = crs(snsr_shp))

# mask points within snsr shp to get all points in shp
snsr_snotels <-mask(snotel_locs, snsr_shp)
plot(snsr_shp)
plot(snsr_snotels[18], add = TRUE)

### back to df after filtering
stations_df <-as.data.frame(snsr_snotels)
stations_df

#########################
## read in snotel data ##
#########################

# create list to loop the dataframes into
snotel_df_list <-as.list(rep(NA, nrow(stations_df)))
head(snotel_df_list)

# read in SNOTEL CSVs using ID number and name from meta data
for (i in seq_along(stations_df$Site_Name)){
  snotel_df_list[[i]] <-as.data.frame(snotel_download(stations_df$Site_ID[i], path = tempdir(), internal = TRUE))
}

# convert from list of dfs to one big one
snotel_df <-bind_rows(snotel_df_list, .id = "network")
snotel_df$date <-ymd(snotel_df$date)

# add water year
colnames(snotel_df)[12] <-"Date"
colnames(snotel_df)[13] <-"snotel_swe_mm"
snotel_df <-dataRetrieval::addWaterYear(snotel_df)
head(snotel_df)

# one ones that don't have the full time series
snotel_df <-filter(snotel_df, waterYear >= 1985 & waterYear <= 2016)
head(snotel_df)

write.csv(snotel_df, "./csvs/snotel_df_v2.csv", row.names = FALSE)
