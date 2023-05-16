# annual temp vs fm and short wave
# jack tarricone
# may 15th, 2023

library(terra)
library(tidyr)
library(ggplot2)
library(scales)
library(cowplot)
library(viridis)
library(data.table)

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

# bring vectors
usj <-vect("./vectors/ca_basins/usj.gpkg")

# single rasters: insol, temp normal, dem
insol_v1 <-rast("./rasters/insolation/snsr_dem_insol_watts_masked_v1.tif")
insol_usj <-mask(crop(insol_v1,ext(usj)), usj)
plot(insol_usj)

# temp
temp_v1 <-rast("./rasters/daymet/tmean_normal_1985_2016.tif")
temp_usj <-mask(crop(temp_v1,ext(usj)), usj)
plot(temp_usj)

# temp
dem_v1 <-rast("./rasters/static/SNSR_DEM.tif")
dem_usj <-mask(crop(dem_v1,ext(usj)), usj)
plot(dem_usj)

# fm load and format
fm_list <-list.files("./rasters/snow_metrics/fm_apr1", full.names = TRUE)
fm_stack_v1 <-rast(fm_list[1:32])
fm_stack_usj <-mask(crop(fm_stack_v1,ext(usj)), usj)
plot(fm_stack_usj[[31]])

# fm load and format
tmean_list <-list.files("./rasters/daymet/tmean", full.names = TRUE)
tmean_stack_v1 <-rast(tmean_list[1:32])
fm_stack_usj <-mask(crop(tmean_stack_v1,ext(usj)), usj)
plot(fm_stack_usj[[2]])


dem_v1 <-rast("./rasters/static/SNSR_DEM.tif")
