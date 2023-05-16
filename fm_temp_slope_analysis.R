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
insol_usj <-mask(crop(rast("./rasters/insolation/snsr_dem_insol_watts_masked_v1.tif"),ext(usj)), usj)
names(insol_usj) <-"insol_watts"
plot(insol_usj)

# temp
temp_usj <-mask(crop(rast("./rasters/daymet/tmean_normal_1985_2016.tif"),ext(usj)), usj)
names(temp_usj) <-"mean_temp_c"
plot(temp_usj)

# dem
dem_usj <-mask(crop(rast("./rasters/static/SNSR_DEM.tif"),ext(usj)), usj)
names(dem_usj) <-"elevation"
plot(dem_usj)

### stacks
# fm load and format
fm_list <-list.files("./rasters/snow_metrics/fm_apr1", full.names = TRUE)
fm_stack_usj <-mask(crop(rast(fm_list[1:32]),ext(usj)), usj)
plot(fm_stack_usj[[31]])

# temp load and format
tmean_list <-list.files("./rasters/daymet/tmean", full.names = TRUE)
tmean_stack_usj <-mask(crop(rast(tmean_list[1:32]),ext(usj)), usj)
plot(tmean_stack_usj[[2]])

# rename layers
wy_names <-seq(1985,2016,1)
names(tmean_stack_usj) <-wy_names
names(fm_stack_usj) <-wy_names
tmean_stack_usj

# stack with other layers
tmean_full <-c(insol_usj, temp_usj, dem_usj, tmean_stack_usj)
fm_full <-c(insol_usj, temp_usj, dem_usj, fm_stack_usj)

# convert to df
tmean_usj_df_v1 <-as.data.frame(tmean_full, xy=TRUE, cells=TRUE)
tmean_usj_df <-as.data.frame(pivot_longer(tmean_usj_df_v1, 7:38, names_to = "wy"))
colnames(tmean_usj_df)[8] <-'annual_tmean_c'
head(tmean_usj_df)

# convert to df
fm_usj_df_v1 <-as.data.frame(fm_full, xy=TRUE, cells=TRUE)
fm_usj_df <-as.data.frame(pivot_longer(fm_usj_df_v1, 7:38, names_to = "wy"))
colnames(fm_usj_df)[8] <-'frac_melt'
head(fm_usj_df)

# join
full_df <-full_join(fm_usj_df, tmean_usj_df)
head(full_df)
fwrite(full_df, "./csvs/usj_temp_fm_slope_analysis.csv", row.names = FALSE)
full_df <-fread("./csvs/usj_temp_fm_slope_analysis.csv")
