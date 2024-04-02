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

# hd
hd_list <-list.files("./rasters/snow_metric_averages/hydro_cat", pattern = "*hd", full.names = T)
hd_stack <-rast(hd_list)
hd_list

# hw
hw_list <-list.files("./rasters/snow_metric_averages/hydro_cat", pattern = "*hw", full.names = T)
hw_stack <-rast(hw_list)
hw_list

# cd
cd_list <-list.files("./rasters/snow_metric_averages/hydro_cat", pattern = "*cd", full.names = T)
cd_stack <-rast(cd_list)
cd_list

# cw
cw_list <-list.files("./rasters/snow_metric_averages/hydro_cat", pattern = "*cw", full.names = T)
cw_stack <-rast(cw_list)
cw_list

generate_gridmet_df <-function(basin_paths_list,
                               name,
                               dom_rast,
                               fm_rast,
                               max_rast,
                               mwa_rast,
                               tmean_rast){
  
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
  
 
  #### snow metircs
  #dom
  dom_mean_shp <-mask(crop(dom_rast,ext(shp)), shp)
  ext(dom_mean_shp) <-ext(dem_shp)
  names(dom_mean_shp) <-"mean_dom_dowy"
  plot(dom_mean_shp)
  
  
  # fm_mean
  fm_mean_shp <-mask(crop(fm_rast,ext(shp)), shp)
  ext(fm_mean_shp) <-ext(dem_shp)
  names(fm_mean_shp) <-"mean_fm"
  plot(fm_mean_shp)
  
  # max swe
  max_mean_shp <-mask(crop(max_rast,ext(shp)), shp)
  ext(max_mean_shp) <-ext(dem_shp)
  names(max_mean_shp) <-"mean_mswe_mm"
  plot(max_mean_shp)
  
  # mwa
  mwa_mean_shp <-mask(crop(mwa_rast,ext(shp)), shp)
  ext(mwa_mean_shp) <-ext(dem_shp)
  names(mwa_mean_shp) <-"mean_mwa"
  plot(mwa_mean_shp)

  # tmean
  tmean_mean_shp <-mask(crop(tmean_rast,ext(shp)), shp)
  ext(tmean_mean_shp) <-ext(dem_shp)
  names(tmean_mean_shp) <-"mean_tmean"
  plot(tmean_mean_shp)
  
  # stack em
  pz_vars_stack <-c(dem_shp, ez_shp, aspect_shp, insol_shp)
  snow_stack <-c(dom_mean_shp,fm_mean_shp, max_mean_shp, mwa_mean_shp, tmean_mean_shp)
  
  # static
  vars_stack <-c(pz_vars_stack,snow_stack)
  vars_stack
  
  # convert to df
  vars_df <-as.data.frame(vars_stack, xy = TRUE, cell = TRUE)
  head(vars_df)
  
  # add basin name col
  a_df9 <-cbind(rep(basin_name, nrow(vars_df)), vars_df)
  a_df10 <-cbind(rep(name, nrow(a_df9)), a_df9)
  colnames(a_df10)[1:2] <-c("hydr0_cat","basin_name")
  a_df11 <-a_df10 %>% na.omit()
  head(a_df11)
  
  # save
  saving_name <-paste0(basin_name,"_",name,"_full_stats_v1.csv")
  fwrite(a_df11, paste0("./csvs/hydro_cat/",saving_name))
  print(paste0(basin_name, " is done!"))
}

# apply to shape files list
# hd
lapply(basin_paths, function(x) generate_gridmet_df(basin_paths_list = x,
                                                    name = "hd",
                                                    dom_rast = hd_stack[[1]],
                                                    fm_rast = hd_stack[[2]],
                                                    max_rast = hd_stack[[3]],
                                                    mwa_rast = hd_stack[[4]],
                                                    tmean_rast = hd_stack[[5]]))

# hw
lapply(basin_paths, function(x) generate_gridmet_df(basin_paths_list = x,
                                                    name = "hw",
                                                    dom_rast = hw_stack[[1]],
                                                    fm_rast = hw_stack[[2]],
                                                    max_rast = hw_stack[[3]],
                                                    mwa_rast = hw_stack[[4]],
                                                    tmean_rast = hw_stack[[5]]))

# cd
lapply(basin_paths, function(x) generate_gridmet_df(basin_paths_list = x,
                                                    name = "cd",
                                                    dom_rast = cd_stack[[1]],
                                                    fm_rast = cd_stack[[2]],
                                                    max_rast = cd_stack[[3]],
                                                    mwa_rast = cd_stack[[4]],
                                                    tmean_rast = cd_stack[[5]]))

# cw
lapply(basin_paths, function(x) generate_gridmet_df(basin_paths_list = x,
                                        name = "cw",
                                        dom_rast = cw_stack[[1]],
                                        fm_rast = cw_stack[[2]],
                                        max_rast = cw_stack[[3]],
                                        mwa_rast = cw_stack[[4]],
                                        tmean_rast = cw_stack[[5]]))

# bind rows
df_paths <-list.files("./csvs/hydro_cat/", full.names = TRUE)
df_list <-lapply(df_paths, fread)
full <-bind_rows(df_list, .id = "column_label")
fwrite(full, "./csvs/hydro_cat/full_df_hydro_cat_v1.csv")
