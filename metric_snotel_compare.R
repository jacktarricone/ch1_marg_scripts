# comparing melt rate dynamics to SNOTELS

library(terra)
library(dplyr)
library(ggplot2)
library(snotelr)
library(lubridate)

setwd('~/ch1_margulis')

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

# bring in mwa 2016
mwa16 <-rast('./rasters/snow_metrics/mwa/mwa_WY2016.tif')

# read in stack
mwa_list <-list.files('./rasters/snow_metrics/mwa/', full.names = TRUE)
mwa_stack <-rast(mwa_list)

# filter for CA
snotel_ca_v1 <-filter(snotel_locs, State == "CA")
snotel_ca <-filter(snotel_ca_v1 , Latitude < 40)

# convert to vect
ca_points <-vect(snotel_ca, geom = c("Longitude","Latitude"), crs = crs(mwa16))

# crop down to extent of points test
crop_ext <-ext(-120.79192, -119, 38, 39.8)
mwa_snotel_ext <-crop(mwa16, crop_ext)
# ca_points_snsr <-crop(ca_points, crop_ext)

# crop 
plot(mwa_snotel_ext)
plot(ca_points, add =TRUE)

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
head(big_df)

# filter for 2016
big_df16 <-filter(big_df, date >= "2015-10-01" & date <= "2016-09-30")

ggplot(big_df16)+
  geom_line(aes(y = snow_water_equivalent, x = date, group = site_name), alpha = .4)

# load in functions
# this file has all the snow metric functions and the raster creation one
url <-"https://raw.githubusercontent.com/jacktarricone/ch1_marg_scripts/main/snow_metric_functions.R"
devtools::source_url(url)

# calc snotel mwa for 2016
snotel_mwa_16 <-as.data.frame(big_df16 %>%
  group_by(site_name) %>% 
  summarise(mwa = mwa(snow_water_equivalent, swe_thres = 25.4)))


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


# extract snotel locations
snsr_mwa_snotel <-terra::extract(mwa_stack, ca_points_snsr,  cells = TRUE, xy = TRUE, method = 'bilinear')
head(snsr_mwa_snotel)
head(snotel_mwa_16)

# rename cols
years <-seq(1985,2016,1)
colnames(snsr_mwa_snotel)[2:33] <-years

# -c(cell,x,y,SNSR_aspect), 
snsr_mwa_df <-as.data.frame(pivot_longer(snsr_mwa_snotel, cols = 2:33, 
                                         names_to = "year",
                                         values_to = "snsr_mwa_mm"))

head(snsr_mwa_df)

compare_df <-cbind(snsr_mwa_snotel, snotel_mwa_16)
head(compare_df)

ggplot(compare_df)+
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(aes(x = lyr.1, y = mwa), size = .9)+
  scale_y_continuous(limits = c(0,200),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,200),expand = (c(0,0))) +
  ylab("SNSR MWA (mm)") + xlab("SNOTEL MWA (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))
