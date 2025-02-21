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

# load in shapes
kern <-vect(basin_paths[1])
usj <-vect(basin_paths[2])
yuba <-vect(basin_paths[3])

basin_paths_list <-basin_paths[1]

generate_la_df <-function(basin_paths_list, name){
  
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
 
  # static
  vars_stack <-c(dem_shp,pa_shp,ez_shp,aspect_shp)
  vars_stack
  
  # convert to df
  vars_df <-as.data.frame(vars_stack, xy = TRUE, cell = TRUE)
  head(vars_df)
  
  # add basin name col
  a_df9 <-cbind(rep(basin_name, nrow(vars_df)), vars_df)
  head(a_df9)
  colnames(a_df9)[1] <-c("basin_name")
  a_df11 <-a_df9 %>% na.omit()
  head(a_df11)
  
  # save
  saving_name <-paste0(basin_name,"_la_stats_v1.csv")
  fwrite(a_df11, paste0("./csvs/",saving_name))
  print(paste0(basin_name, " is done!"))
}

# apply to shape files list
lapply(basin_paths, function(x) generate_la_df(basin_paths_list = x))

# bind rows
df_paths <-list.files("./csvs", full.names = TRUE, pattern = "*_la_stats_v1.csv")
df_list <-lapply(df_paths, fread)
full <-bind_rows(df_list, .id = "column_label")

df <-full

# rename
df$bin_name <-ifelse(df$ez == 1, "1500-1900 m", df$ez)
df$bin_name <-ifelse(df$ez == 2, "1900-2300 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 3, "2300-2700 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 4, "2700-3100 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 5, "3100-4361 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 6, "3100-4361 m", df$bin_name)

df$aspect_name <-ifelse(df$aspect ==  1, "North", df$aspect)
df$aspect_name <-ifelse(df$aspect ==  3, "South", df$aspect_name)
df$aspect_name <-ifelse(df$aspect ==  2, "East", df$aspect_name)
df$aspect_name <-ifelse(df$aspect ==  4, "West", df$aspect_name)

# names
df$basin_name <-ifelse(df$basin_name == "kern", "Kern", df$basin_name)
df$basin_name <-ifelse(df$basin_name == "usj", "USJ", df$basin_name)
df$basin_name <-ifelse(df$basin_name == "yuba", "Yuba", df$basin_name)
head(df)
fwrite(df, "./csvs/full_la_stats_v1.csv")
