# make full analysis df with new gridmet data
# jack tarricone
# june 22nd, 2023

library(terra)
library(tidyverse)
library(scales)
library(cowplot)
library(viridis)
library(data.table)

setwd("~/ch1_margulis")

# list of paths to shape files
basin_paths_v1 <-list.files("./vectors/ca_basins", full.names = TRUE, pattern = "\\.gpkg$")
basin_paths <-basin_paths_v1[c(6,19,21)]

basin_paths_list <-basin_paths[1]

generate_gridmet_df <-function(basin_paths_list,
                               cd_rast,
                               cw_rast,
                               hd_rast,
                               hw_rast){
  
  # read in shape
  shp <-vect(basin_paths_list)
  basin_name_v1 <-basename(basin_paths_list)
  basin_name <-gsub(".gpkg","",basin_name_v1)
  print(basin_name)
  
  #### static vars
  # dem
  dem <-rast("./rasters/static/SNSR_DEM.tif")
  dem_shp <-mask(crop(dem,ext(shp)), shp)
  names(dem_shp) <-"elevation"
  plot(dem_shp)
  
  # elevation zone
  ez_shp <-mask(crop(rast("./rasters/categorized/dem_6zb.tif"),ext(shp)), shp)
  ext(ez_shp) <-ext(dem_shp)
  names(ez_shp) <-"ez"
  plot(ez_shp)
  
  # aspect
  aspect_shp <-mask(crop(rast("./rasters/categorized/aspect_thres_4_classes.tif"),ext(shp)), shp)
  ext(aspect_shp) <-ext(dem_shp)
  names(aspect_shp) <-"aspect"
  plot(aspect_shp)
  
  # insol
  insol_shp <-mask(crop(rast("./rasters/insolation/snsr_dem_insol_watts_masked_v1.tif"), ext(shp)), shp)
  ext(insol_shp) <-ext(dem_shp)
  names(insol_shp) <-"insol_watts"
  plot(insol_shp)
  
  ###### gridmet
  # temp_mean
  tmean_mean <-rast("./rasters/gridmet/var_means/tmean_mean.tif")
  ext(tmean_mean) <-ext(dem)
  temp_mean_shp <-mask(crop(tmean_mean,ext(shp)), shp)
  names(temp_mean_shp) <-"mean_temp_c"
  
  # rmean
  rmean_mean <-rast("./rasters/gridmet/var_means/rmean_mean.tif")
  ext(rmean_mean) <-ext(dem)
  rmean_mean_shp <-mask(crop(rmean_mean,ext(shp)), shp)
  names(rmean_mean_shp) <-"mean_rh_%"
  
  # ah
  ah_mean <-rast("./rasters/gridmet/var_means/ah_mean_f_25mm_27obs.tif")
  ext(ah_mean) <-ext(dem)
  ah_mean_shp <-mask(crop(ah_mean,ext(shp)), shp)
  names(ah_mean_shp) <-"mean_ah"
  plot(ah_mean_shp)
  
  # srad
  srad_mean <-rast("./rasters/gridmet/var_means/srad_mean.tif")
  ext(srad_mean) <-ext(dem)
  srad_mean_shp <-mask(crop(srad_mean,ext(shp)), shp)
  names(srad_mean_shp) <-"mean_srad"
  
  # sph
  sph_mean <-rast("./rasters/gridmet/var_means/sph_mean.tif")
  ext(sph_mean) <-ext(dem)
  sph_mean_shp <-mask(crop(sph_mean,ext(shp)), shp)
  names(sph_mean_shp) <-"mean_sph"
  plot(sph_mean_shp)
  
  #### snow metircs
  # fm_mean
  fm_mean_shp <-mask(crop(rast("./rasters/snow_metric_averages/fm_mean_f_25mm_27obs.tif"),ext(shp)), shp)
  ext(fm_mean_shp) <-ext(dem_shp)
  names(fm_mean_shp) <-"mean_fm"
  plot(fm_mean_shp)
  
  mwa_mean_shp <-mask(crop(rast("./rasters/snow_metric_averages/mwa_mean_f_25mm_27obs.tif"),ext(shp)), shp)
  ext(mwa_mean_shp) <-ext(dem_shp)
  names(mwa_mean_shp) <-"mean_mwa"
  plot(mwa_mean_shp)
  
  # max swe
  max_mean_shp <-mask(crop(rast("./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif"),ext(shp)), shp)
  ext(max_mean_shp) <-ext(dem_shp)
  names(max_mean_shp) <-"mswe_mean_mm"
  plot(max_mean_shp)
  
  #dom
  dom_mean_shp <-mask(crop(rast("./rasters/snow_metric_averages/dom_mean_f_25mm_27obs.tif"),ext(shp)), shp)
  ext(dom_mean_shp) <-ext(dem_shp)
  names(dom_mean_shp) <-"mean_dom_dowy"
  plot(dom_mean_shp)
  
  # stack em
  gridmet_stack <-c(temp_mean_shp, rmean_mean_shp, ah_mean_shp, srad_mean_shp, sph_mean_shp)
  pz_vars_stack <-c(dem_shp, ez_shp, aspect_shp, insol_shp)
  snow_stack <-c(fm_mean_shp, max_mean_shp, dom_mean_shp, mwa_mean_shp)
  
  # static
  static_vars_stack <-c(gridmet_stack,pz_vars_stack,snow_stack)
  # plot(static_vars_stack)
  
  ################### 
  ##### stacks ######
  ################### 
  
  # rename layers
  wy_names <-seq(1985,2016,1)
  
  ########### snow metrics
  # fm load and format
  fm_list <- list.files("./rasters/snow_metrics/fm_apr1", full.names = TRUE)
  fm_stack_shp <-mask(crop(rast(fm_list[1:32]),ext(shp)), shp)
  names(fm_stack_shp) <-wy_names
  
  # mwa load and format
  mwa_list <- list.files("./rasters/snow_metrics/mwa_ondjfm_mm", full.names = TRUE)
  mwa_stack_shp <-mask(crop(rast(mwa_list[1:32]),ext(shp)), shp)
  names(mwa_stack_shp) <-wy_names
  
  # max_swe
  mswe_stack <- rast("./rasters/snow_metrics/max_swe/max_stack_f_25mm_27obs.tif")
  mswe_stack_shp <-mask(crop(mswe_stack,ext(shp)), shp)
  names(mswe_stack_shp) <-wy_names
  
  # dom
  dom_stack <- rast("./rasters/snow_metrics/max_swe_dowy/dom_stack_f_25mm_27obs.tif")
  dom_stack_shp <-mask(crop(dom_stack,ext(shp)), shp)
  names(dom_stack_shp) <-wy_names
  
  ########## gridmet data
  # tmean
  tmean_stack_v1 <-rast("./rasters/gridmet/tmean/tmean_stack.tif")
  ext(tmean_stack_v1) <-ext(dem)
  tmean_stack_shp <-mask(crop(tmean_stack_v1,ext(shp)), shp)
  names(tmean_stack_shp) <-wy_names
  
  # rmean  
  rmean_stack_v1 <- rast("./rasters/gridmet/rmean/rmean_stack.tif")
  ext(rmean_stack_v1) <-ext(dem)
  rmean_stack_shp <-mask(crop(rmean_stack_v1,ext(shp)), shp)
  names(rmean_stack_shp) <-wy_names
  
  # rmean  
  ah_stack_v1 <- rast("./rasters/gridmet/ah/ah_stack.tif")
  ext(ah_stack_v1) <-ext(dem)
  ah_stack_shp <-mask(crop(ah_stack_v1,ext(shp)), shp)
  names(ah_stack_shp) <-wy_names
  
  # srad  
  srad_list <- list.files("./rasters/gridmet/srad/", full.names = TRUE)
  srad_stack_v1 <-rast(srad_list[1:32])
  ext(srad_stack_v1) <-ext(dem)
  srad_stack_shp <-mask(crop(srad_stack_v1,ext(shp)), shp)
  names(srad_stack_shp) <-wy_names
  
  # sph  
  sph_list <- list.files("./rasters/gridmet/sph/", full.names = TRUE)
  sph_stack_v1 <-rast(sph_list[1:32])
  ext(sph_stack_v1) <-ext(dem)
  sph_stack_shp <-mask(crop(sph_stack_v1,ext(shp)), shp)
  names(sph_stack_shp) <-wy_names

  #### snow
  # fm
  fm_stack <-c(static_vars_stack,
               fm_stack_shp) # fm
  
  fm_df_v1 <-as.data.frame(fm_stack, xy = TRUE, cell = TRUE)
  
  fm_df <- tidyr::pivot_longer(fm_df_v1, cols = 17:48, 
                               names_to = 'wy', values_to = 'frac_melt')
  
  # mwa
  mwa_stack <-c(static_vars_stack,
               mwa_stack_shp) # mwa
  
  mwa_df_v1 <-as.data.frame(mwa_stack, xy = TRUE, cell = TRUE)
  
  mwa_df <- tidyr::pivot_longer(mwa_df_v1, cols = 17:48, 
                               names_to = 'wy', values_to = 'mwa_mm')
  
  # mswe
  mswe_stack <-c(static_vars_stack,
               mswe_stack_shp) # fm
  mswe_df_v1 <-as.data.frame(mswe_stack, xy = TRUE, cell = TRUE)
  
  mswe_df <- tidyr::pivot_longer(mswe_df_v1, cols = 17:48, 
                               names_to = 'wy', values_to = 'mswe_mm')
  
  # dom
  dom_stack <-c(static_vars_stack,
                 dom_stack_shp) # fm
  dom_df_v1 <-as.data.frame(dom_stack, xy = TRUE, cell = TRUE)
  
  dom_df <- tidyr::pivot_longer(dom_df_v1, cols = 17:48, 
                                 names_to = 'wy', values_to = 'dom_dowy')
  
  
  # gridmet
  # tmean
  tmean_stack <-c(static_vars_stack,
                tmean_stack_shp) # fm
  tmean_df_v1 <-as.data.frame(tmean_stack, xy = TRUE, cell = TRUE)
  
  tmean_df <- tidyr::pivot_longer(tmean_df_v1, cols = 17:48, 
                                names_to = 'wy', values_to = 'temp_mean_c')
  
  
  # rmean
  rmean_stack <-c(static_vars_stack,
                  rmean_stack_shp) # fm
  rmean_df_v1 <-as.data.frame(rmean_stack, xy = TRUE, cell = TRUE)
  
  rmean_df <- tidyr::pivot_longer(rmean_df_v1, cols = 17:48, 
                                  names_to = 'wy', values_to = 'rh_mean_%')
  
  # ah
  ah_stack <-c(static_vars_stack,
                  ah_stack_shp) # fm
  
  ah_df_v1 <-as.data.frame(ah_stack, xy = TRUE, cell = TRUE)
  
  ah_df <- tidyr::pivot_longer(ah_df_v1, cols = 17:48, 
                                  names_to = 'wy', values_to = 'abs_hum_gcm3')
  
  # srad
  srad_stack <-c(static_vars_stack,
                  srad_stack_shp) # fm
  srad_df_v1 <-as.data.frame(srad_stack, xy = TRUE, cell = TRUE)
  
  srad_df <- tidyr::pivot_longer(srad_df_v1, cols = 17:48, 
                                  names_to = 'wy', values_to = 'srad_wm2')
  
  # srad
  sph_stack <-c(static_vars_stack,
                 sph_stack_shp) # fm
  sph_df_v1 <-as.data.frame(sph_stack, xy = TRUE, cell = TRUE)
  
  sph_df <- tidyr::pivot_longer(sph_df_v1, cols = 17:48, 
                                 names_to = 'wy', values_to = 'sph_kgkg')
  
  # full join all 7 data frames
  a_df1 <-full_join(fm_df, mswe_df)
  a_df2 <-full_join(a_df1, mwa_df)
  a_df3 <-full_join(a_df2, dom_df)
  a_df4 <-full_join(a_df3, tmean_df)
  a_df5 <-full_join(a_df4, rmean_df)
  a_df6 <-full_join(a_df5, ah_df)
  a_df7 <-full_join(a_df6, srad_df)
  a_df8 <-full_join(a_df7, sph_df)
  
  # add basin name col
  a_df9 <-cbind(rep(basin_name, nrow(a_df8)), a_df8)
  colnames(a_df9)[1] <-"basin_name"
  a_df10 <-a_df9 %>% na.omit()
  head(a_df10)
  # write.csv(head(a_df10), "./csvs/gridmet_dfs/head_df_v4.csv")
  
  # save
  saving_name <-paste0(basin_name,"_full_stats_v4.csv")
  fwrite(a_df10, paste0("./csvs/gridmet_dfs/",saving_name))
  print(paste0(basin_name, " is done!"))
}

# apply to shape files list
lapply(basin_paths, generate_gridmet_df)

# df_paths <-list.files("./csvs/gridmet_dfs/", full.names = TRUE)
# df_list <-lapply(df_paths, fread)
# full <-bind_rows(df_list, .id = "column_label")
# fwrite(full, "./csvs/gridmet_dfs/full_df.csv")
