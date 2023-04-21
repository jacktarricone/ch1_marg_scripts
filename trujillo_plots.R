# starting making mean table
# april 6, 2023

library(terra)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(cowplot)
library(viridis)
library(ggpointdensity)

setwd("~/ch1_margulis")

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

# max
max_mean <-rast("./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif")
max_mean

# bring in max dowy mean
dom_mean <-rast("./rasters/snow_metric_averages/dom_mean_f_25mm_27obs.tif")
dom_mean

# bring in elevation
dem_v1 <-rast("./rasters/static/SNSR_DEM.tif")
dem <-mask(dem_v1, dom_mean)

#### convert to df
dem_df <-as.data.frame(dem, xy = TRUE, cells = TRUE)
colnames(dem_df)[4] <-'ele'

dom_df <-as.data.frame(dom_mean, xy = TRUE, cells = TRUE)
colnames(dom_df)[4] <-'dom'

max_df <-as.data.frame(max_mean, xy = TRUE, cells = TRUE)
colnames(max_df)[4] <-'max_swe_mm'
max_df$max_swe_m <-max_df$max_swe_mm/1000 # create meter col
head(max_df)
hist(max_df$max_swe_m)

# create plotting df
plotting_df_v1 <-dplyr::full_join(dem_df, dom_df)
plotting_df <-dplyr::full_join(plotting_df_v1, max_df)
head(plotting_df)

scale <-brewer.pal(9, 'YlGnBu')

# max swe vs elevation
ggplot(plotting_df, aes(y = ele, x = max_swe_m)) +
  geom_hex(bins = 30) +
  scale_fill_gradientn(colors = scale, oob = squish) + 
  scale_y_continuous(limits = c(1500,4200), breaks = c(seq(1500,4000,500)), expand = (c(0,.2))) +
  scale_x_continuous(limits = c(0, 2),breaks = c(seq(0,2,.5)), expand = (c(0,0))) +
  labs(x = "Max SWE (m)", y = "Elevation (m)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1), 
        legend.position = c(.75,.75))

test <-ggplot(plotting_df, aes(y = ele, x = max_swe_m)) +
  geom_pointdensity(adjust = 5, size = 1) +
  scale_color_viridis(option = "H") +
  scale_y_continuous(limits = c(1500,4200), breaks = c(seq(1500,4000,500)), expand = (c(0,.2))) +
  scale_x_continuous(limits = c(0, 2),breaks = c(seq(0,2,.5)), expand = (c(0,0))) +
  labs(x = "Max SWE (m)", y = "Elevation (m)")+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1), 
        legend.position = c(.75,.75))

# save
ggsave(test,
       file = "./plots/test.png",
       width = 4.5, 
       height = 4.5,
       dpi = 600)

system("open ./plots/test.png")




# aspect north vs south
aspect <-rast("./rasters/categorized/aspect_4deg_ns.tif")


# filter trend data by slope
max_trend_s_v1 <-mask(max_trend, aspect, maskvalue = 1)
max_trend_s <-mask(max_trend_s_v1, aspect, maskvalue = NA)
plot(max_trend_s)
writeRaster(max_trend_s, "./rasters/mk_results/max_trend_s.tif", overwrite = TRUE)

max_trend_n_v1 <-mask(max_trend, aspect, maskvalue = 3)
max_trend_n <-mask(max_trend_n_v1, aspect, maskvalue = NA)
plot(max_trend_n)
writeRaster(max_trend_n, "./rasters/mk_results/max_trend_n.tif", overwrite = TRUE)

# mean stats
mean_n <-round(as.numeric(global(max_trend_n, mean, na.rm = TRUE)), digits = 2)
mean_s <-round(as.numeric(global(max_trend_s, mean, na.rm = TRUE)), digits = 2)
sd_n <-round(as.numeric(global(max_trend_n, sd, na.rm = TRUE)), digits = 2)
sd_s <-round(as.numeric(global(max_trend_s, sd, na.rm = TRUE)), digits = 2)
cv_n <-(sd_n/mean_n)*100
cv_s <-(sd_n/mean_s)*100

iqr_n <-round(as.numeric(global(max_trend_n, IQR, na.rm = TRUE)), digits = 2)
?global


hist(max_trend_n, breaks = 100)
hist(max_trend_s, breaks = 100, col = "red", add = TRUE)

# mask dem for same pixels
dem_v2 <-mask(dem, mwa)
max_v2 <-mask(max_cm, mwa)
max_dowy <-mask(max_dowy_mean, mwa)

# just for ns
dem_ns <-mask(dem_v2, aspect)
max_ns <-mask(max_v2, aspect)
dowy_ns <-mask(max_dowy, aspect)

# convert to df
dem_df <-as.data.frame(dem_v2, xy = TRUE)
dowy_df <-as.data.frame(max_dowy, xy = TRUE)
max_df <-as.data.frame(max_v2, xy = TRUE)
# mwa_df <-as.data.frame(mwa, xy = TRUE)

# convert to df
dem_ns_df <-as.data.frame(dem_ns, xy = TRUE)
dowy_ns_df <-as.data.frame(dowy_ns, xy = TRUE)
max_ns_df <-as.data.frame(max_ns, xy = TRUE)
aspect_df <-as.data.frame(aspect, xy = TRUE)
# mwa_df <-as.data.frame(mwa, xy = TRUE)

# bind and rename
df <-cbind(dem_df, as.integer(dowy_df$lyr.1), max_df$mean, aspect_df$aspect)
colnames(df)[3:6] <- c("dem","max_dowy","max_swe_cm","aspect")
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
