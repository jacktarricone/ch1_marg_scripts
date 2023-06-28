####### start mk results analysis table
# jack tarricone
# april 19th, 2023

library(terra)
library(dplyr)
library(tidyr)
library(data.table)

setwd("~/ch1_margulis")

#####################################
# head_df for col referencing
df_paths <-list.files("./csvs/gridmet_dfs", full.names = TRUE, pattern = "full_stat")
df_list <-lapply(df_paths, fread)
colnames(df_list[[1]])[c(6,24)] <-c("mean_rh","rh_mean")
colnames(df_list[[2]])[c(6,24)] <-c("mean_rh","rh_mean")
colnames(df_list[[3]])[c(6,24)] <-c("mean_rh","rh_mean")
head(df_list[[1]])

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

######################################
######################################
############ snow data ###############
######################################
######################################

snow_results <-df %>%
  group_by(basin_name, zone_name, aspect_name) %>%
  summarise(mean_ez_mswe = as.integer(mean(mswe_mean_mm)),
            mean_ez_dom = as.integer(mean(mean_dom_dowy)),
            mean_ez_fm = round(mean(mean_fm),2),
            mean_ez_mwa = as.integer(mean(mean_mwa)))

# Reshape the data into separate columns for aspect_name
snow_results_wide <- snow_results  %>%
  tidyr::pivot_wider(names_from = aspect_name,
              values_from = c(mean_ez_mswe, mean_ez_dom, mean_ez_fm, mean_ez_mwa))

# calc diff
df_diff <- snow_results_wide %>%
  mutate(
    diff_ez_mswe = mean_ez_mswe_South - mean_ez_mswe_North,
    diff_ez_dom = mean_ez_dom_South - mean_ez_dom_North,
    diff_ez_fm = mean_ez_fm_South - mean_ez_fm_North,
    diff_ez_mwa = mean_ez_mwa_South - mean_ez_mwa_North
  )

# Sort the dataframe
df_sorted <- df_diff %>%
  select(basin_name, zone_name, matches("mswe"), matches("dom"), matches("fm"), matches("mwa"))

# View the sorted dataframe
df_sorted

# save
write.csv(df_sorted, "./csvs/snow_avg_metric_results_table_v4.csv", row.names = FALSE)
system("open ./csvs/snow_avg_metric_results_table_v4.csv")


######################################
######################################
############ met data ###############
######################################
######################################

# met data
met_results <-df %>%
  group_by(basin_name, zone_name, aspect_name) %>%
  summarise(mean_ez_temp_c  = round(mean(mean_temp_c),2),
            mean_ez_ah = round(mean(mean_ah),2),
            mean_ez_srad = as.integer(mean(mean_srad)),
            mean_cs_insol  = as.integer(mean(insol_watts)))

print(met_results, n = nrow(met_results))

# Reshape the data into separate columns for aspect_name
met_results_wide <- met_results   %>%
  tidyr::pivot_wider(names_from = aspect_name,
                     values_from = c(mean_ez_temp_c, mean_ez_ah, mean_ez_srad, mean_cs_insol))

# calc diff
met_diff <- met_results_wide  %>%
  mutate(
    diff_ez_temp_c = mean_ez_temp_c_South - mean_ez_temp_c_North,
    diff_ez_ah = mean_ez_ah_South - mean_ez_ah_North,
    diff_ez_srad = mean_ez_srad_South - mean_ez_srad_North,
    diff_ez_insol = mean_cs_insol_South - mean_cs_insol_North
    
  )

# Sort the dataframe
met_df_sorted <- met_diff %>%
  select(basin_name, zone_name, matches("temp"), matches("ah"), matches("srad"), matches("insol"))

# View the sorted dataframe
met_df_sorted

# save
write.csv(met_df_sorted, "./csvs/met_avg_metric_results_table_v1.csv", row.names = FALSE)
system("open ./csvs/met_avg_metric_results_table_v1.csv")



