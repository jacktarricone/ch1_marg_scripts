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
  
  #### static vars
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
  
  # insol
  insol_shp <-mask(crop(rast("./rasters/insolation/snsr_dem_insol_watts_masked_v1.tif"), ext(shp)), shp)
  names(insol_shp) <-"insol_watts"
  plot(insol_shp)
  
  
  ###### gridmet
  # temp_mean
  temp_mean_shp <-mask(crop(rast("./rasters/gridmet/var_means/tmean_mean.tif"),ext(shp)), shp)
  names(temp_mean_shp) <-"mean_temp_c"
  
  # rmean
  rmean_mean_shp <-mask(crop(rast("./rasters/gridmet/var_means/rmean_mean.tif"),ext(shp)), shp)
  names(rmean_mean_shp) <-"mean_rh_%"
  
  # srad
  srad_mean_shp <-mask(crop(rast("./rasters/gridmet/var_means/srad_mean.tif"),ext(shp)), shp)
  names(srad_mean_shp) <-"mean_srad"=
  
  # sph
  sph_mean_shp <-mask(crop(rast("./rasters/gridmet/var_means/sph_mean.tif"),ext(shp)), shp)
  names(sph_mean_shp) <-"mean_sph"
  plot(sph_mean_shp)
  
  
  #### snow metircs
  # fm_mean
  fm_mean_shp <-mask(crop(rast("./rasters/snow_metric_averages/fm_mean_f_25mm_27obs.tif"),ext(shp)), shp)
  names(fm_mean_shp) <-"mean_fm"
  plot(fm_mean_shp)
  
  # max swe
  max_mean_shp <-mask(crop(rast("./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif"),ext(shp)), shp)
  names(max_mean_shp) <-"mswe_mean_mm"
  plot(max_mean_shp)
  
  #dom
  dom_mean_shp <-mask(crop(rast("./rasters/snow_metric_averages/dom_mean_f_25mm_27obs.tif"),ext(shp)), shp)
  names(dom_mean_shp) <-"dom_mean_dowy"
  plot(dom_mean_shp)
  
  ################### 
  ##### stacks ######
  ################### 
  
  ########### snow metrics
  # fm load and format
  fm_list <- list.files("./rasters/snow_metrics/fm_apr1", full.names = TRUE)
  fm_stack_shp <-mask(crop(rast(fm_list[1:32]),ext(shp)), shp)
  
  # max_swe
  mswe_stack <- rast("./rasters/snow_metrics/max_swe/max_stack_f_25mm_27obs.tif")
  mswe_stack_shp <-mask(crop(mswe_stack,ext(shp)), shp)
  
  # dom
  dom_stack <- rast("./rasters/snow_metrics/max_swe_dowy/dom_stack_f_25mm_27obs.tif")
  dom_stack_shp <-mask(crop(dom_stack,ext(shp)), shp)
  
  
  ########## gridmet data
  # tmean
  tmean_stack_v1 <-rast("./rasters/gridmet/tmean/tmean_stack.tif")
  tmean_stack_shp_v1 <-mask(crop(tmean_stack_v1,ext(shp)), shp)
  tmean_stack_shp <-mask(tmean_stack_shp_v1, mswe_stack_shp, maskvalue = NA)
  
  # rmean  
  rmean_stack_v1 <- rast("./rasters/gridmet/rmean/rmean_stack.tif")
  rmean_stack_shp <-mask(crop(rmean_stack_v1,ext(shp)), shp)
  

  # srad  
  srad_list <- list.files("./rasters/gridmet/srad/", full.names = TRUE)
  srad_stack_shp <-mask(crop(rast(srad_list[1:32]),ext(shp)), shp)
  # plot(srad_stack_shp[[10]])
  
  # sph  
  sph_list <- list.files("./rasters/gridmet/sph/", full.names = TRUE)
  sph_stack_shp <-mask(crop(rast(sph_list[1:32]),ext(shp)), shp)
  # plot(sph_stack_shp[[10]])
  
  # rename layers
  wy_names <-seq(1985,2016,1)
  
  # snow
  names(fm_stack_shp) <-wy_names
  names(mswe_stack_shp) <-wy_names
  names(dom_stack_shp) <-wy_names
  
  # gridmet
  names(tmean_stack_shp) <-wy_names
  names(rmean_stack_shp) <-wy_names
  names(srad_stack_shp) <-wy_names
  names(sph_stack_shp) <-wy_names

  # # calculate number of non na obs per pixel
  # fm_n_obs <-app(fm_stack_shp, function(x) sum(!is.na(x)))
  # n_obs_v2 <-subst(fm_n_obs, 0, NA)
  # # plot(n_obs_v2)
  # 
  # # convert all values below 10 to NA
  # masking_value <-subst(n_obs_v2, 0:27, NA)
  # # plot(masking_value)
  # 
  # # if there are less than 10 observations per pixel, make NA
  # fm_v3 <-mask(fm_stack_shp, masking_value, maskvalues = NA)
  # tmean_v3 <-mask(tmean_stack_shp, masking_value, maskvalues = NA)
  
  # stack and join
  #### snow
  # fm
  fm_stack <-c(dem_shp, ez_shp, aspect_shp, insol_shp, # dem, elevaiton zone, aspect, insol
               temp_mean_shp, rmean_shp, srad_mean_shp, sph_mean_shp, # 4 gridmet vars
               fm_mean_shp, max_mean_shp, dom_mean_shp, # 3 snow metric means
               fm_stack_shp) # fm
  fm_df_v1 <-as.data.frame(fm_stack, xy = TRUE, cell = TRUE)
  
  fm_df <- tidyr::pivot_longer(fm_df_v1, cols = 15:46, 
                               names_to = 'wy', values_to = 'frac_melt')
  
  # mswe
  mswe_stack <-c(dem_shp, ez_shp, aspect_shp, insol_shp, # dem, elevaiton zone, aspect, insol
               temp_mean_shp, rmean_shp, srad_mean_shp, sph_mean_shp, # 4 gridmet vars
               fm_mean_shp, max_mean_shp, dom_mean_shp, # 3 snow metric means
               mswe_stack_shp) # fm
  mswe_df_v1 <-as.data.frame(mswe_stack, xy = TRUE, cell = TRUE)
  
  mswe_df <- tidyr::pivot_longer(mswe_df_v1, cols = 15:46, 
                               names_to = 'wy', values_to = 'mswe_mm')
  
  # dom
  dom_stack <-c(dem_shp, ez_shp, aspect_shp, insol_shp, # dem, elevaiton zone, aspect, insol
                 temp_mean_shp, rmean_shp, srad_mean_shp, sph_mean_shp, # 4 gridmet vars
                 fm_mean_shp, max_mean_shp, dom_mean_shp, # 3 snow metric means
                 dom_stack_shp) # fm
  dom_df_v1 <-as.data.frame(dom_stack, xy = TRUE, cell = TRUE)
  
  dom_df <- tidyr::pivot_longer(dom_df_v1, cols = 15:46, 
                                 names_to = 'wy', values_to = 'dom_dowy')
  
  
  # gridmet
  # tmean
  tmean_stack <-c(dem_shp, ez_shp, aspect_shp, insol_shp, # dem, elevaiton zone, aspect, insol
                temp_mean_shp, rmean_shp, srad_mean_shp, sph_mean_shp, # 4 gridmet vars
                fm_mean_shp, max_mean_shp, dom_mean_shp, # 3 snow metric means
                tmean_stack_shp) # fm
  tmean_df_v1 <-as.data.frame(tmean_stack, xy = TRUE, cell = TRUE)
  
  tmean_df <- tidyr::pivot_longer(tmean_df_v1, cols = 15:46, 
                                names_to = 'wy', values_to = 'temp_mean_c')
  
  
  # rmean
  rmean_stack <-c(dem_shp, ez_shp, aspect_shp, insol_shp, # dem, elevaiton zone, aspect, insol
                  temp_mean_shp, rmean_shp, srad_mean_shp, sph_mean_shp, # 4 gridmet vars
                  fm_mean_shp, max_mean_shp, dom_mean_shp, # 3 snow metric means
                  rmean_stack_shp) # fm
  rmean_df_v1 <-as.data.frame(rmean_stack, xy = TRUE, cell = TRUE)
  
  rmean_df <- tidyr::pivot_longer(rmean_df_v1, cols = 15:46, 
                                  names_to = 'wy', values_to = 'rh_mean_%')
  
  # srad
  srad_stack <-c(dem_shp, ez_shp, aspect_shp, insol_shp, # dem, elevaiton zone, aspect, insol
                  temp_mean_shp, rmean_shp, srad_mean_shp, sph_mean_shp, # 4 gridmet vars
                  fm_mean_shp, max_mean_shp, dom_mean_shp, # 3 snow metric means
                  srad_stack_shp) # fm
  srad_df_v1 <-as.data.frame(srad_stack, xy = TRUE, cell = TRUE)
  
  srad_df <- tidyr::pivot_longer(srad_df_v1, cols = 15:46, 
                                  names_to = 'wy', values_to = 'srad_wm2')
  
  # srad
  sph_stack <-c(dem_shp, ez_shp, aspect_shp, insol_shp, # dem, elevaiton zone, aspect, insol
                 temp_mean_shp, rmean_shp, srad_mean_shp, sph_mean_shp, # 4 gridmet vars
                 fm_mean_shp, max_mean_shp, dom_mean_shp, # 3 snow metric means
                 sph_stack_shp) # fm
  sph_df_v1 <-as.data.frame(sph_stack, xy = TRUE, cell = TRUE)
  
  sph_df <- tidyr::pivot_longer(sph_df_v1, cols = 15:46, 
                                 names_to = 'wy', values_to = 'sph_kgkg')
  
  # full join
  # srad_df, sph_df)
  a_df1 <-full_join(fm_df, mswe_df)
  a_df2 <-full_join(a_df1, dom_df)
  a_df3 <-full_join(a_df2, tmean_df)
  a_df4 <-full_join(a_df3, rmean_df)
  a_df5 <-full_join(a_df4, srad_df)
  a_df6 <-full_join(a_df5, sph_df)
  
  # save
  saving_name <-paste0(basin_name,"_gridmet_snsr_ts.csv")
  fwrite(a_df6, paste0("./csvs/gridmet_dfs/",saving_name))
  print(paste0(basin_name, " is done!"))
}
