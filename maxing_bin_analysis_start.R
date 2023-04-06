# binning analysis test

library(terra)
library(dplyr)
library(ggplot2)
library(Kendall)

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

# bring in american shp
american <-vect("./vectors/ca_basins/american.gpkg")
plot(american)

# max swe 93, 16, 16
max93 <-rast("./rasters/snow_metrics/max_swe/years/max_swe_WY1993.tif")
max15 <-rast("./rasters/snow_metrics/max_swe/years/max_swe_WY2015.tif")
max16 <-rast("./rasters/snow_metrics/max_swe/years/max_swe_WY2016.tif")
# max_stack <-rast("./rasters/snow_metrics/max_swe/max_american.tif")

# test on max_dowy
max_list <-list.files("./rasters/snow_metrics/mwa", pattern = ".tif", full.names = TRUE)
max_stack_raw <-rast(max_list)

# bring in aspect
bins <-rast("./rasters/categorized/american_dem_aspect_classified.tif")

# crop and mask
stack_v2 <-crop(max_stack_raw, ext(bins))
max_stack <-mask(stack_v2, bins)
plot(max_stack[[7]])

# # pull out bins
# ez1_n <-mask(max93_v3, bins, maskvalue = 1, inverse = TRUE)
# ez1_s <-mask(max93_v3, bins, maskvalue = 2, inverse = TRUE)
# ez2_n <-mask(max93_v3, bins, maskvalue = 3, inverse = TRUE)
# ez2_s <-mask(max93_v3, bins, maskvalue = 4, inverse = TRUE)
# ez3_n <-mask(max93_v3, bins, maskvalue = 5, inverse = TRUE)
# ez3_s <-mask(max93_v3, bins, maskvalue = 6, inverse = TRUE)
# 
# # stack bings for analysis
# bin_stack <-c(ez1_n, ez1_s, ez2_s, ez2_n, ez3_s, ez3_n)

# pull out bins
ez1_n <-mask(max_stack, bins, maskvalue = 1, inverse = TRUE)
ez1_s <-mask(max_stack, bins, maskvalue = 2, inverse = TRUE)
ez2_n <-mask(max_stack, bins, maskvalue = 3, inverse = TRUE)
ez2_s <-mask(max_stack, bins, maskvalue = 4, inverse = TRUE)
ez3_n <-mask(max_stack, bins, maskvalue = 5, inverse = TRUE)
ez3_s <-mask(max_stack, bins, maskvalue = 6, inverse = TRUE)

# stack bings for analysis
bin_stack <-c(ez1_n, ez1_s, ez2_s, ez2_n, ez3_s, ez3_n)
zone <-rep(c("ez1_n", "ez1_s", "ez2_s", "ez2_n", "ez3_s", "ez3_n"), each = 32)
year <-rep(1985:2016, 6)

# calc?
bin_means <-global(bin_stack, "mean", na.rm = TRUE)
bin_means

# bind
df <-as.data.frame(cbind(bin_means, zone, year))
df

# calc
total_max <-global(max_stack, "mean", na.rm = TRUE)
total_max

### test plot
# set colors
color_scale <-viridis::viridis(6)
names(color_scale) <-levels(df$zone)

# plot
ggplot(df, aes(x = year, y = mean, color = zone)) +
  geom_point() +
  scale_colour_manual(name = "zone", values = color_scale) +
  labs(y = "MWA (mm/yr)", x = "WY") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))
  

# filter
ez1_n <-filter(df, zone == "ez1_n")
ez1_s <-filter(df, zone == "ez1_s")
ez2_n <-filter(df, zone == "ez2_n")
ez2_s <-filter(df, zone == "ez2_s")
ez3_n <-filter(df, zone == "ez3_n")
ez3_s <-filter(df, zone == "ez3_s")


# kendall test
library(trend)
sens.slope(ez1_n$mean, conf.level = .95)
sens.slope(ez1_s$mean, conf.level = .95)
sens.slope(ez2_n$mean, conf.level = .95)
sens.slope(ez2_s$mean, conf.level = .95)
sens.slope(ez3_n$mean, conf.level = .95)
sens.slope(ez3_s$mean, conf.level = .95)




