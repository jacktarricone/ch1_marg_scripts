####### start mk results analysis table
# jack tarricone
# april 19th, 2023

library(terra)
library(dplyr)
library(tidyr)
library(data.table)

setwd("~/ch1_margulis")

# # read in df
# # fm
# fm_mean <-rast("./rasters/snow_metric_averages/fm_mean_f_25mm_27obs.tif")
# fm_mean_df <-as.data.frame(fm_mean, xy = TRUE, cells = TRUE)
# colnames(fm_mean_df)[4] <-"frac_melt"
# head(fm_mean_df)
# 
# # max
# max_mean <-rast("./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif")
# max_mean_df <-as.data.frame(max_mean, xy = TRUE, cells = TRUE)
# colnames(max_mean_df)[4] <-"max_swe_mm"
# head(max_mean_df)
# 
# # dom
# dom_mean <-rast("./rasters/snow_metric_averages/dom_mean_f_25mm_27obs.tif")
# dom_mean_df <-as.data.frame(dom_mean, xy = TRUE, cells = TRUE)
# colnames(dom_mean_df)[4] <-"dom_dowy"
# head(dom_mean_df)

# ez
ez <-rast("./rasters/categorized/dem_6zb.tif")
names(ez) <-"ez"

# dem
dem <-rast("./rasters/static/SNSR_DEM.tif")
names(dem) <-"elevation"

# cc
cc <-rast("./rasters/nlcd_cc/cc_w0.tif")
names(cc) <-"cc"

# temp
temp <-rast("./rasters/prism/prism_tmean_snsr_ondjfm.tif")
names(temp)<-"temp_c"

# insol
insol <-rast("./rasters/insolation/snsr_dem_insol_watts_masked_v1.tif")
names(insol) <-"insol_kwh"


# join
mean_df <-dplyr::full_join(max_mean_df, dom_mean_df)
mean_df$max_swe_m <-mean_df$max_swe_mm/1000
head(mean_df)

# fitlering for same cells
mean_aspect_df <-subset(mean_df, cell %in% ez_df$cell)
ez_filt <-subset(ez_df, cell %in% mean_aspect_df$cell)
dem_filt <-subset(dem_df, cell %in% ez_df$cell)
cc_filt <-subset(cc_df, cell %in% dem_df$cell)
fm_filt <-subset(fm_mean_df, cell %in% cc_df$cell)
temp_filt <-subset(temp_df, cell %in% fm_filt$cell)
insol_filt <-subset(insol_df, cell %in% temp_filt$cell)

# bind
mean_ez_df_v2 <-dplyr::full_join(mean_aspect_df, ez_filt)
mean_ez_df_v3 <-dplyr::full_join(mean_ez_df_v2, dem_filt)
mean_ez_df_v4 <-dplyr::full_join(mean_ez_df_v3, cc_filt)
mean_ez_df_v5 <-dplyr::full_join(mean_ez_df_v4, fm_filt)
mean_ez_df_v6 <-dplyr::full_join(mean_ez_df_v5, temp_filt)
mean_ez_df_v7 <-dplyr::full_join(mean_ez_df_v6, insol_filt)
mean_ez_df <-mean_ez_df_v7  %>% tidyr::drop_na()
write.csv(mean_ez_df, "./csvs/full_plotting_df_v1.csv")

# read in df
mean_ez_df <-fread("./csvs/full_plotting_df_v1.csv")

# inspect
head(mean_ez_df)
hist(mean_ez_df$temp_c, breaks = 100)
hist(mean_ez_df$insol_kwh, breaks = 100)
hist(mean_ez_df$frac_melt, breaks = 100)

# pull out PZs
ez1_n_df <-subset(mean_ez_df, ez == 1)
ez1_s_df <-subset(mean_ez_df, ez == 2)
ez2_n_df <-subset(mean_ez_df, ez == 3)
ez2_s_df <-subset(mean_ez_df, ez == 4)
ez3_n_df <-subset(mean_ez_df, ez == 5)
ez3_s_df <-subset(mean_ez_df, ez == 6)

# make list
ez_list <-list(ez1_n_df, ez1_s_df, 
               ez2_n_df, ez2_s_df, 
               ez3_n_df, ez3_s_df)
# names
ez_names <-c("EZ1_N", "EZ1_S", 
             "EZ2_N", "EZ2_S", 
             "EZ3_N", "EZ3_S")

# name list
names(ez_list) <-ez_names
names(ez_list[1])

head(mean_ez_df)

#####################################
# head_df for col referencing
df_paths <-list.files("./csvs/gridmet_dfs", full.names = TRUE, pattern = "full_stat")
df_list <-lapply(df_paths, fread)
colnames(df_list[[1]])[c(6,24)] <-c("mean_rh","rh_mean")
colnames(df_list[[2]])[c(6,24)] <-c("mean_rh","rh_mean")
colnames(df_list[[3]])[c(6,24)] <-c("mean_rh","rh_mean")
head(df_list[[1]])


north_results <-filter(results_v1, aspect_name == "North")
# make one big df
df_v1 <-bind_rows(df_list)

# elevations zones
df_v1$zone_name <-ifelse(df_v1$ez== 1, "1500-1900 m", df_v1$ez)
df_v1$zone_name <-ifelse(df_v1$ez == 2, "1900-2300 m", df_v1$zone_name)
df_v1$zone_name <-ifelse(df_v1$ez == 3, "2300-2700 m", df_v1$zone_name)
df_v1$zone_name <-ifelse(df_v1$ez == 4, "2700-3100 m", df_v1$zone_name)
df_v1$zone_name <-ifelse(df_v1$ez == 5, "3100-4361 m", df_v1$zone_name)
df_v1$zone_name <-ifelse(df_v1$ez == 6, "3100-4361 m", df_v1$zone_name)

# elevations zones
df_v1$aspect_name <-ifelse(df_v1$aspect == 1, "North", df_v1$aspect)
df_v1$aspect_name <-ifelse(df_v1$aspect == 2, "East", df_v1$aspect_name)
df_v1$aspect_name <-ifelse(df_v1$aspect == 3, "South", df_v1$aspect_name)
df_v1$aspect_name <-ifelse(df_v1$aspect == 4, "West", df_v1$aspect_name)

# filter
df <-filter(df_v1, aspect_name == "North" | aspect_name == "South")

# met data
met_results <-df %>%
  group_by(basin_name, zone_name, aspect_name) %>%
  summarise(mean_ez_temp_c  = round(mean(mean_temp_c),2),
            sd_ez_temp_c    = round(sd(mean_temp_c, na.rm = TRUE),2),
            mean_ez_ah = round(mean(mean_ah),2),
            sd_ez_ah    = round(sd(mean_ah, na.rm = TRUE),2),
            mean_ez_srad = round(mean(mean_srad),2),
            sd_ez_srad    = round(sd(mean_srad, na.rm = TRUE),2),)

print(met_results, n = nrow(met_results))


# summarize
snow_results <-df %>%
  group_by(basin_name, zone_name, aspect_name) %>%
  summarise(mean_ez_mswe = as.integer(mean(mswe_mean_mm)),
            sd_ez_mswe   = as.integer(sd(mswe_mean_mm, na.rm = TRUE)),
            mean_ez_dom = as.integer(mean(mean_dom_dowy)),
            sd_ez_dom    = as.integer(sd(mean_dom_dowy, na.rm = TRUE)),
            mean_ez_fm = round(mean(mean_fm),2),
            sd_ez_fm    = round(sd(mean_fm, na.rm = TRUE),2),
            mean_ez_mwa = as.integer(mean(mean_mwa)),
            sd_ez_mwa    = as.integer(sd(mean_mwa, na.rm = TRUE)))

# unite
snow_results_combined <- snow_results %>%
  unite(mean_sd_ez_mswe, c(mean_ez_mswe, sd_ez_mswe), sep = " ± ", remove = TRUE) %>%
  unite(mean_sd_ez_dom, c(mean_ez_dom, sd_ez_dom), sep = " ± ", remove = TRUE) %>%
  unite(mean_sd_ez_fm, c(mean_ez_fm, sd_ez_fm), sep = " ± ", remove = TRUE) %>%
  unite(mean_sd_ez_mwa, c(mean_ez_mwa, sd_ez_mwa), sep = " ± ", remove = TRUE)

snow_results_combined 

# Reshape the data into separate columns for aspect_name
snow_results_wide <- snow_results_combined %>%
  tidyr::pivot_wider(names_from = aspect_name,
              values_from = c(mean_sd_ez_mswe, mean_sd_ez_dom, mean_sd_ez_fm, mean_sd_ez_mwa))

print(snow_results_wide, n = nrow(snow_results_wide))

# save
write.csv(snow_results_wide, "./csvs/snow_avg_metric_results_table_v1.csv", row.names = FALSE)
system("open ./csvs/snow_avg_metric_results_table_v1.csv")

