# comparing max swe to SNOTELS

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
snotel_locs <-read.csv("./csvs/SNOTEL_MASTER_pt_update.csv")
head(snotel_locs)

# read in stack
max_list <-list.files('./rasters/snow_metrics/max_swe_dowy/', full.names = TRUE)
max_stack <-rast(max_list)

# stations in CA with 32 years of record in the SNSR
good_stations <-as.integer(c(356, 428, 462, 463, 473, 508, 518, 539, 
                  540, 541, 575, 697, 724, 771, 778, 784, 809, 834, 846, 848))

# filter for CA
snotel_ca <-filter(snotel_locs, Site_ID %in% good_stations)
snotel_ca

# convert to vect
ca_points <-vect(snotel_ca, geom = c("Longitude","Latitude"), crs = crs(max_stack))

# crop down to extent of points test
crop_ext <-ext(-120.79192, -119, 38, 39.8)
max_snotel_ext <-crop(max_stack, crop_ext)
ca_points_snsr <-crop(ca_points, crop_ext)

# crop 
plot(max_snotel_ext[[9]])
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

# calc snotel max for 2016
snotel_max_df <-as.data.frame(snotel_df %>%
  group_by(site_name, waterYear) %>%
  summarise(snotel_max_dowy = as.integer(max_swe_dowy(snow_water_equivalent, swe_thres = 25.4))))

# colnames(snotel_max_df)[3] <-"snotel_max_mm"
head(snotel_max_df)

# check to see if names are the same
unique(snotel_max_df$site_name)
unique(snotel_ca$Site_Name)

# # extract cell number from pit lat/lon point
# snotel_cell_numbers <-as.data.frame(cells(max_stack, ca_points_snsr))
# snotel_cell_numbers 
# 
# ## loop to pull create string of snotel cell and 8 surrounding 
# 
# dummy_list <-list()
# for (i in seq_along(snotel_cell_numbers$cell)){
#   neighbor_cells <-c(adjacent(max_stack, cells = snotel_cell_numbers$cell[i], directions ="8"))
#   dummy_list[[i]] <-c(snotel_cell_numbers$cell[i], neighbor_cells)
# }
# 
# # extract using that vector
# nine_cells <-terra::extract(max_stack, dummy_list[[20]], xy = TRUE)
# nine_cells

snsr_max_snotel <-terra::extract(max_stack, ca_points_snsr, 
                                 names = TRUE, 
                                 cells = TRUE, 
                                 xy = TRUE, 
                                 ID = TRUE,
                                 method = 'bilinear')
# rename cols
years <-seq(1985,2016,1)
snsr_max_snotel <-cbind(unique(snotel_df$site_name), snsr_max_snotel)
colnames(snsr_max_snotel)[3:34] <-years
colnames(snsr_max_snotel)[1] <-"site_name_v2"
snsr_max_snotel

# -c(cell,x,y,SNSR_aspect), 
snsr_max_df <-as.data.frame(pivot_longer(snsr_max_snotel, cols = 3:34, 
                                         names_to = "year",
                                         values_to = "snsr_max_dowy"))
# round to int
snsr_max_df$snsr_max_dowy <-as.integer(snsr_max_df$snsr_max_dowy)
head(snsr_max_df)
head(snotel_max_df)

# make df for plotting
compare_df <-cbind(snsr_max_df, snotel_max_df)
head(compare_df)

ggplot(compare_df) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(aes(y = snsr_max_dowy, x = snotel_max_dowy), size = .9, color = "goldenrod") +
  scale_y_continuous(limits = c(50, 300),expand = (c(0,0))) +
  scale_x_continuous(limits = c(50, 300),expand = (c(0,0))) +
  ylab("SNSR Max SWE DOWY") + xlab("SNOTEL Max DOWY") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

cor(compare_df$snsr_max_dowy, compare_df$snotel_max_dowy, use = "complete.obs")

# filter for 2016
big_df16 <-filter(snotel, date >= "2015-10-01" & date <= "2016-09-30")

ggplot(big_df16)+
  geom_line(aes(y = snow_water_equivalent, x = date, group = site_name), alpha = .4)

# filter for palisades
pt <-filter(snotel_df, site_name == "palisades tahoe ")
pt95 <-filter(pt, waterYear == 1995)


ggplot(pt95)+
  geom_line(aes(y = snow_water_equivalent, x = Date))

