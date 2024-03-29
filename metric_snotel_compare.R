# comparing melt rate dynamics to SNOTELS

library(terra)
library(dplyr)
library(ggplot2)
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

# bring in real snotel locations
snotel_locs <-read.csv("./csvs/SNOTEL_MASTER (1).csv")
head(snotel_locs)

# read in stack
mwa_list <-list.files('./rasters/snow_metrics/mwa/', full.names = TRUE)
mwa_stack <-rast(mwa_list)

# stations in CA with 32 years of record in the SNSR
good_stations <-as.integer(c(356, 428, 462, 463, 473, 508, 518, 539, 
                  540, 541, 575, 697, 724, 771, 778, 784, 809, 834, 846, 848))

# filter for CA
snotel_ca <-filter(snotel_locs, Site_ID %in% good_stations)

# convert to vect
ca_points <-vect(snotel_ca, geom = c("Longitude","Latitude"), crs = crs(mwa_stack))

# crop down to extent of points test
crop_ext <-ext(-120.79192, -119, 38, 39.8)
mwa_snotel_ext <-crop(mwa_stack, crop_ext)
ca_points_snsr <-crop(ca_points, crop_ext)

# crop 
plot(mwa_snotel_ext[[8]])
plot(ca_points_snsr, add =TRUE)

# create list to loop the dataframes into
df_list <-as.list(rep(NA, nrow(snotel_ca)))
head(snotel_ca)

# read in SNOTEL CSVs using ID number and name from meta data
for (i in seq_along(snotel_ca$Site_Name)){
  df_list[[i]] <-as.data.frame(snotel_download(snotel_ca$Site_ID[i], path = tempdir(), internal = TRUE))
}

# bind into one big df
big_df <-bind_rows(df_list, .id = "network")
big_df$date <-ymd(big_df$date)

# add water year
colnames(big_df)[12] <-"Date"
big_df <-dataRetrieval::addWaterYear(big_df)
head(big_df)

# filter to start at 85
snotel_df_v1 <-filter(big_df, start <= as.Date("1984-10-10"))
head(snotel_df_v1)

# one ones that don't have the full time series
snotel_df <-filter(snotel_df_v1, waterYear >= 1985 & waterYear <= 2016)
head(snotel_df)

# calc snotel mwa for 2016
snotel_mwa_df <-as.data.frame(snotel_df %>%
  group_by(site_name, waterYear) %>%
  summarise(mwa = as.integer(mwa(snow_water_equivalent, swe_thres = 25.4))))

colnames(snotel_mwa_df)[3] <-"snotel_mwa_mm"

# extract snotel locations
snsr_mwa_snotel <-terra::extract(mwa_stack, ca_points_snsr, 
                                 names = TRUE, 
                                 cells = TRUE, 
                                 xy = TRUE, 
                                 ID = TRUE,
                                 method = 'simple')
head(snsr_mwa_snotel)

# rename cols
years <-seq(1985,2016,1)
colnames(snsr_mwa_snotel)[2:33] <-years

# -c(cell,x,y,SNSR_aspect), 
snsr_mwa_df <-as.data.frame(pivot_longer(snsr_mwa_snotel, cols = 2:33, 
                                         names_to = "year",
                                         values_to = "snsr_mwa_mm"))

# make df for plotting
compare_df <-cbind(snsr_mwa_df, snotel_mwa_df)
head(compare_df)

ggplot(compare_df) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(aes(x = snsr_mwa_mm, y = snotel_mwa_mm), size = .9)+
  scale_y_continuous(limits = c(0,200),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,200),expand = (c(0,0))) +
  ylab("SNSR MWA (mm)") + xlab("SNOTEL MWA (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

cor(compare_df$snotel_mwa_mm, compare_df$snsr_mwa_mm, use = "complete.obs")









# filter for 2016
big_df16 <-filter(big_df, date >= "2015-10-01" & date <= "2016-09-30")

ggplot(big_df16)+
  geom_line(aes(y = snow_water_equivalent, x = date, group = site_name), alpha = .4)

# filter for palisades
pt16 <-filter(big_df16, site_name == "palisades tahoe ")

#### calc metrics for pt to make sure they're working correctly
# mwa
pt16_mwa <-mwa(pt16$snow_water_equivalent, 25.4)
pt16_mwa

# max swe
pt16_max_swe <-max_swe(pt16$snow_water_equivalent, 25.4)
pt16_max_swe

# max_dowy
pt16_max_swe_dowy <-max_swe_dowy(pt16$snow_water_equivalent, 25.4)
pt16_max_swe_dowy

# melt_rate
pt16_melt_rate <-melt_rate(pt16$snow_water_equivalent, 25.4)
pt16_melt_rate

# msl
pt16_msl <-msl(pt16$snow_water_equivalent, 25.4)
pt16_msl

# sdd
pt16_sdd <-sdd(pt16$snow_water_equivalent, 25.4)
pt16_sdd

ggplot(pt)+
  geom_line(aes(y = snow_water_equivalent, x = date))
