# starting making mean table
# april 6, 2023

library(terra)
library(tidyr)
library(ggplot2)

setwd("~/ch1_margulis/")

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

# bring in elevation
dem <-rast("./rasters/static/SNSR_DEM.tif")
dem

# bring in max dowy mean
max_dowy_mean <-rast("./rasters/snow_metric_averages/max_dowy_mean_v2.tif")
max_dowy_mean
plot(max_dowy_mean)
hist(max_dowy_mean)

# max
max <-rast("./rasters/snow_metric_averages/max_mean.tif")
max_cm <-max/10

# mwa 
mwa <-rast("./rasters/snow_metric_averages/mwa_mean_v2.tif")
plot(mwa)

# mask dem for same pixels
dem_v2 <-mask(dem, mwa)
max_v2 <-mask(max_cm, mwa)
max_dowy <-mask(max_dowy_mean, mwa)

# convert to df
dem_df <-as.data.frame(dem_v2, xy = TRUE)
dowy_df <-as.data.frame(max_dowy, xy = TRUE)
max_df <-as.data.frame(max_v2, xy = TRUE)
mwa_df <-as.data.frame(mwa, xy = TRUE)

# bind and rename
df <-cbind(dem_df, as.integer(dowy_df$lyr.1), max_df$mean, mwa_df$lyr.1)
colnames(df)[3:6] <- c("dem","max_dowy","max_swe_cm","mwa_mm")
head(df)

# dowy vs elevation
ggplot(df, aes(mwa_mm, dem)) +
  geom_hex(bins = 30) +
  scale_fill_gradient(low = "gray99", high = "black") +
  scale_y_continuous(limits = c(1500,4200), breaks = c(seq(1500,4000,500)), expand = (c(0,.2))) +
  scale_x_continuous(limits = c(0, 200),breaks = c(seq(0,200,50)), expand = (c(0,0))) +
  labs(x = "MWA (mm)", y = "Elevation (m)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1), 
        legend.position = c(.13,.75))

# dowy vs elevation
ggplot(df, aes(max_dowy, dem)) +
  geom_hex(bins = 40) +
  scale_fill_gradient(low = "gray99", high = "black") +
  scale_y_continuous(limits = c(1500,4200), breaks = c(seq(1500,4000,500)), expand = (c(0,.2))) +
  scale_x_continuous(limits = c(79, 225),breaks = c(seq(100,225,25)), expand = (c(0,0))) +
  labs(x = "Max SWE DOWY", y = "Elevation (m)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1), 
        legend.position = c(.13,.75))

# dowy vs max
ggplot(df, aes(max_dowy, max_swe_cm)) +
  geom_hex(bins = 40) +
  scale_fill_gradient(low = "gray99", high = "black") +
  scale_y_continuous(limits = c(0,200), breaks = c(seq(0,200,50)), expand = (c(0,.2))) +
  scale_x_continuous(limits = c(79, 225),breaks = c(seq(100,225,25)), expand = (c(0,0))) +
  labs(x = "Max SWE DOWY", y = "Max SWE (cm)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1), 
        legend.position = c(.13,.75))
