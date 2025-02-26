# made sd per hydro class
# jack tarricone

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
kern_df <-generate_basin_df(paths_file = basin_paths[1],  wa_rast = wa_stack)
usj_df <-generate_basin_df(paths_file= basin_paths[2],  wa_rast = wa_stack)
yuba_df <-generate_basin_df(paths_file =basin_paths[3],  wa_rast = wa_stack)

full <-rbind(kern_df,usj_df,yuba_df)
head(full)

hist(full$wa_swe_m3, breaks = 100)
hist(full$max_swe_m3, breaks = 100)

# fwrite(full, "./csvs/hydro_cat/full_df_hydro_cat_v4.csv")

# test <-fread("/Users/jtarrico/ch1_margulis/csvs/hydro_cat/formatted_df_v1.csv")
# head(test)
# unique(test$aspect_name)

##############################################
# full <-fread("./csvs/hydro_cat/full_df_hydro_cat_v4.csv")

df <-full
head(df)

## hydro cat 
hydro_cat <-read.csv("./csvs/hydro_cat_years.csv")
hydro_cat$lyr <-seq(1,32,1) # lyr for subsetting
hydro_cat

# filter for four quads
cd <-filter(hydro_cat, hydro_cat == "cd")
cw <-filter(hydro_cat, hydro_cat == "cw")
hd <-filter(hydro_cat, hydro_cat == "hd")
hw <-filter(hydro_cat, hydro_cat == "hw")
cd

# add hydro cat labels
df <- data.frame(df) %>%
  mutate(hydro_cat = ifelse(wy %in% cd$years, "CD", "Other"),
         hydro_cat = ifelse(wy %in% cw$years, "CW", hydro_cat),
         hydro_cat = ifelse(wy %in% hw$years, "HW", hydro_cat),
         hydro_cat = ifelse(wy %in% hd$years, "HD", hydro_cat))


head(df)

# rename
df$bin_name <-ifelse(df$ez == 1, "1500-1900 m", df$ez)
df$bin_name <-ifelse(df$ez == 2, "1900-2300 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 3, "2300-2700 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 4, "2700-3100 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 5, "3100-4361 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 6, "3100-4361 m", df$bin_name)
head(df)

# names
df$basin <-ifelse(df$basin == "kern", "Kern", df$basin)
df$basin <-ifelse(df$basin == "usj", "USJ", df$basin)
df$basin <-ifelse(df$basin == "yuba", "Yuba", df$basin)
head(df)

fwrite(df, "./csvs/wa_max_all_years_v1.csv")

