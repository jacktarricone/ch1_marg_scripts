# comparing max swe to SNOTELS

library(dplyr)
library(snotelr)
library(lubridate)

setwd('~/ch1_margulis')

# load in functions
# this file has all the snow metric functions and the raster creation one
url <-"https://raw.githubusercontent.com/jacktarricone/ch1_marg_scripts/main/snow_metric_functions.R"
devtools::source_url(url)

# set custom plot theme
theme_classic <-function(base_size = 11, base_family = "",
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22) {
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # no background and no grid
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # show axes
      # axis.line      = element_line(colour = "black", linewidth = rel(1)),
      
      # match legend key to panel.background
      legend.key       = element_blank(),
      
      # simple, black and white strips
      strip.background = element_rect(fill = "white", colour = "black", linewidth = rel(2)),
      # NB: size is 1 but clipped, it looks like the 0.5 of the axes
      
      complete = TRUE
    )
}

theme_set(theme_classic(14))

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
snotel_df <-filter(snotel_df, start <= as.Date("1984-10-10"))
head(snotel_df)

# one ones that don't have the full time series
snotel_df <-filter(snotel_df, waterYear >= 1985 & waterYear <= 2016)
head(snotel_df)

write.csv(snotel_df, "./csvs/snotel_df.csv")
