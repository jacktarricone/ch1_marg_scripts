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

# load in stack
wa_paths <-list.files("./rasters/snow_metrics/wa/", pattern = ".tif", full.names = TRUE)
wa_stack <-rast(wa_paths)
names(wa_stack) <-c(seq(1985,2016,1))
wa_stack

# load in stack
max_paths <-list.files("./rasters/snow_metrics/max_swe/years", pattern = ".tif", full.names = TRUE)
max_stack <-rast(max_paths)
names(max_stack) <-c(seq(1985,2016,1))
max_stack

generate_basin_df <-function(paths_file,wa_rast){
  
  # read in shape
  shp <-vect(paths_file)
  basin_name_v1 <-basename(paths_file)
  basin_name <-gsub(".gpkg","",basin_name_v1)
  print(basin_name)
  
  #### static vars
  # dem
  dem <-rast("./rasters/static/SNSR_DEM.tif")
  dem_shp <-mask(crop(dem,ext(shp)), shp)
  names(dem_shp) <-"elevation"
  plot(dem_shp)
  
  # pixel area 
  pa <-rast("./rasters/static/SNSR_pixel_area.tif")
  pa_shp <-mask(crop(pa,ext(shp)), shp)
  names(pa_shp) <-"area_m3"
  plot(pa_shp)
  
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
  
  # max swe
  max_mean_shp <-mask(crop(max_stack,ext(shp)), shp)
  ext(max_mean_shp) <-ext(dem_shp)
  
  # max swe vol
  max_vol_shp <-(max_mean_shp/1000)*pa_shp
  
  # wa
  wa_mean_shp <-mask(crop(wa_stack,ext(shp)), shp)
  ext(wa_mean_shp) <-ext(dem_shp)
  
  # max swe vol
  wa_vol_shp <-(wa_mean_shp/1000)*pa_shp
  
  # stack em
  pz_vars_stack <-c(dem_shp, ez_shp, aspect_shp, insol_shp)
  
  # make dfs for max and wa
  max_vol_stack <-c(pz_vars_stack, max_vol_shp)
  max_vol_df <-as.data.frame(max_vol_stack,xy = TRUE, cell = TRUE)
  head(max_vol_df)
  
  # make dfs for max and wa
  wa_vol_stack <-c(pz_vars_stack, wa_vol_shp)
  wa_vol_df <-as.data.frame(wa_vol_stack,xy = TRUE, cell = TRUE)
  head(wa_vol_df)
  
  # pivot max
  max_long <-pivot_longer(max_vol_df,
                          cols = 8:ncol(max_vol_df),
                          names_to = "wy",
                          values_to = "max_swe_m3")
  
  # pivot max
  wa_long <-pivot_longer(wa_vol_df,
                         cols = 8:ncol(wa_vol_df),
                         names_to = "wy",
                         values_to = "wa_swe_m3")
  
  # add name
  joined_df <-full_join(wa_long,max_long)
  joined_df$basin <-rep(basin_name, nrow(joined_df))
  joined_df1 <-joined_df %>% na.omit()
  head(joined_df1)
  
  print("done!")
  joined_df1
}

# apply to shape files list
kern_df <- generate_basin_df(paths_file = basin_paths[1],  wa_rast = wa_stack)
usj_df <- generate_basin_df(paths_file= basin_paths[2],  wa_rast = wa_stack)
yuba_df <- generate_basin_df(paths_file =basin_paths[3],  wa_rast = wa_stack)


# bind rows
df_paths <-list.files("./csvs/hydro_cat/", full.names = TRUE)
df_list <-lapply(df_paths, fread)
full <-bind_rows(df_list, .id = "column_label")
fwrite(full, "./csvs/hydro_cat/full_df_hydro_cat_v4.csv")

# test <-fread("/Users/jtarrico/ch1_margulis/csvs/hydro_cat/formatted_df_v1.csv")
# head(test)
# unique(test$aspect_name)

##############################################
df <-fread("./csvs/hydro_cat/full_df_hydro_cat_v4.csv")
head(df)

# rename
df$bin_name <-ifelse(df$ez == 1, "1500-1900 m", df$ez)
df$bin_name <-ifelse(df$ez == 2, "1900-2300 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 3, "2300-2700 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 4, "2700-3100 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 5, "3100-4361 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 6, "3100-4361 m", df$bin_name)

# names
df$hydr0_cat <-ifelse(df$hydr0_cat == "cw", "CW", df$hydr0_cat)
df$hydr0_cat <-ifelse(df$hydr0_cat == "cd", "CD", df$hydr0_cat)
df$hydr0_cat <-ifelse(df$hydr0_cat == "hw", "HW", df$hydr0_cat)
df$hydr0_cat <-ifelse(df$hydr0_cat == "hd", "HD", df$hydr0_cat)

# names
df$basin_name <-ifelse(df$basin_name == "kern", "Kern", df$basin_name)
df$basin_name <-ifelse(df$basin_name == "usj", "USJ", df$basin_name)
df$basin_name <-ifelse(df$basin_name == "yuba", "Yuba", df$basin_name)
head(df)
fwrite(df, "./csvs/hydro_cat/plotting_full_df_hydro_cat_v4.csv")
