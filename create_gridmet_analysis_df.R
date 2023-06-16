# make full analysis df with new gridmet data
# jack tarricone
# may june 16th, 2023

library(terra)
library(tidyverse)
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

# list of paths to shape files
basin_paths <-list.files("./vectors/ca_basins", full.names = TRUE, pattern = "\\.gpkg$")

generate_gridmet_df <-function(basin_paths_list){
  
  # read in shape
  shp <-vect(basin_paths[1])
  basin_name_v1 <-basename(basin_paths[1])
  basin_name <-gsub(".gpkg","",basin_name_v1)
  print(basin_name)
  
  # insol
  insol_shp <-mask(crop(rast("./rasters/insolation/snsr_dem_insol_watts_masked_v1.tif"), ext(shp)), shp)
  names(insol_shp) <-"insol_watts"
  plot(insol_shp)
  
  # temp_mean
  temp_mean_shp <-mask(crop(rast("./rasters/gridmet/var_means/tmean_mean.tif"),ext(shp)), shp)
  names(temp_mean_shp) <-"mean_temp_c"
  plot(temp_mean_shp)
  
  # rmean
  rmean_shp <-mask(crop(rast("./rasters/gridmet/var_means/rmean_mean.tif"),ext(shp)), shp)
  names(rmean_shp) <-"mean_rh_%"
  plot(rmean_shp)
  
  # rmean
  rmean_shp <-mask(crop(rast("./rasters/gridmet/var_means/rmean_mean.tif"),ext(shp)), shp)
  names(rmean_shp) <-"mean_rh_%"
  plot(rmean_shp)
  
  # fm_mean
  fm_mean_shp <-mask(crop(rast("./rasters/snow_metric_averages/fm_mean_f_25mm_27obs.tif"),ext(shp)), shp)
  names(fm_mean_shp) <-"mean_fm"
  plot(fm_mean_shp)
  
  # dem
  dem_shp <-mask(crop(rast("./rasters/static/SNSR_DEM.tif"),ext(shp)), shp)
  names(dem_shp) <-"elevation"
  plot(dem_shp)
  
  # elevation zone
  ez_shp <-mask(crop(rast("./rasters/categorized/dem_6zb.tif"),ext(shp)), shp)
  names(ez_shp) <-"ez"
  plot(ez_shp)
  
  # aspect
  aspect_shp <-mask(crop(rast("./rasters/categorized/aspect_thres_4_classes.tif"),ext(shp)), shp)
  names(aspect_shp) <-"aspect"
  plot(aspect_shp)
  
  ### stacks
  # fm load and format
  fm_list <- list.files("./rasters/snow_metrics/fm_apr1", full.names = TRUE)
  fm_stack_shp <-mask(crop(rast(fm_list[1:32]),ext(shp)), shp)
  # plot(fm_stack_shp[[1]])
  
  # temp load and format
  tmean_list <-list.files("./rasters/daymet/tmean", full.names = TRUE)
  tmean_stack_shp_v1 <-mask(crop(rast(tmean_list[1:32]),ext(shp)), shp)
  tmean_stack_shp <-mask(tmean_stack_shp_v1, fm_stack_shp, maskvalue = NA)
  # plot(tmean_stack_shp[[1]])
  
  # rename layers
  wy_names <-seq(1985,2016,1)
  names(tmean_stack_shp) <-wy_names
  names(fm_stack_shp) <-wy_names
  
  # calculate number of non na obs per pixel
  fm_n_obs <-app(fm_stack_shp, function(x) sum(!is.na(x)))
  n_obs_v2 <-subst(fm_n_obs, 0, NA)
  # plot(n_obs_v2)
  
  # convert all values below 10 to NA
  masking_value <-subst(n_obs_v2, 0:27, NA)
  # plot(masking_value)
  
  # if there are less than 10 observations per pixel, make NA
  fm_v3 <-mask(fm_stack_shp, masking_value, maskvalues = NA)
  tmean_v3 <-mask(tmean_stack_shp, masking_value, maskvalues = NA)
  
  # stack and join
  # fm
  fm_stack <-c(dem_shp, temp_mean_shp, insol_shp, ez_shp, aspect_shp, fm_mean_shp, fm_v3)
  fm_df_v1 <-as.data.frame(fm_stack, xy = TRUE, cell = TRUE)
  fm_df <-as.data.frame(tidyr::pivot_longer(fm_df_v1 ,10:41, 
                                            names_to = 'wy', values_to = 'frac_melt'))
  # head(fm_df)
  
  # tmean
  tmean_stack <-c(dem_shp, temp_mean_shp, insol_shp, ez_shp, aspect_shp, fm_mean_shp, tmean_v3)
  tmean_df_v1 <-as.data.frame(tmean_stack, xy = TRUE, cell = TRUE)
  tmean_df <-as.data.frame(tidyr::pivot_longer(tmean_df_v1 ,10:41, 
                                               names_to = 'wy', values_to = 'ondjfm_temp_c'))
  # head(tmean_df)
  
  # full join
  analysis_df <-full_join(fm_df, tmean_df)
  analysis_df <-analysis_df %>% drop_na()
  
}