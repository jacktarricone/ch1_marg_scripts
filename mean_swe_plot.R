library(dplyr)

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

# bind together
snsr_df <-bind_rows(snsr_snotel_data) # make df
snsr_df <-subset(snsr_df, select=-c(station_id)) # move bad one
colnames(snsr_df)[c(1,4,5)] <-c('site_name_v2','lat','lon')
tail(snsr_df)
tail(snotel_df)

# print names
unique(snsr_df$site_name_v2)
unique(snotel_df$site_name)

snsr_df[119431:119440,]
snotel_df[119331:119440,]

snsr_ndays <-as.data.frame(snsr_df %>%
                             group_by(site_name_v2, wy) %>%
                             summarise(n_days = length(wy)))

horse <-filter(snsr_ndays, site_name_v2 == "HORSE_MEADOW")
horse

snotel_ndays <-as.data.frame(snotel_df %>%
                             group_by(site_name, waterYear) %>%
                             summarise(n_days = length(waterYear)))

horse_v2 <-filter(snotel_ndays, site_name == "horse meadow ")
horse_v2

max(snsr_ndays$n_days, )

snotel_df

# remove 9 extra values...
snsr_df_v2 <-snsr_df %>% filter(row_number() <= n()-9)

# test to see if wy match
year_match_test <-ifelse(snsr_df$wy == snotel_df$waterYear, TRUE, FALSE)
year_match_test
which(year_match_test == FALSE)

# bind together
swe_df <-cbind(snotel_df, snsr_df_v2)
head(swe_df)