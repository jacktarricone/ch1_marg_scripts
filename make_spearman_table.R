# make spearman table
# jack tarricone
# june 11th, 2023

library(tidyverse)
library(data.table)

# set wd
setwd("~/ch1_margulis")

# # read in csvs from each basin
# csv_list <-list.files("./csvs/spearman_fm_temp_results", full.names = TRUE)
# df_list <-lapply(csv_list[2:21], fread)
# big_df <-bind_rows(df_list)
# 
# # format aspect and elevation zones
# # add ez
# big_df$zone_name <-ifelse(big_df$ez == 1, "1500-1900 m", big_df$ez)
# big_df$zone_name <-ifelse(big_df$ez == 2, "1900-2300 m", big_df$zone_name)
# big_df$zone_name <-ifelse(big_df$ez == 3, "2300-2700 m", big_df$zone_name)
# big_df$zone_name <-ifelse(big_df$ez == 4, "2700-3100 m", big_df$zone_name)
# big_df$zone_name <-ifelse(big_df$ez == 5, "3100-3500 m", big_df$zone_name)
# big_df$zone_name <-ifelse(big_df$ez == 6, "3500-4361 m", big_df$zone_name)
# 
# # aspect name
# big_df$aspect_name <-ifelse(big_df$aspect == 1, "North", big_df$aspect)
# big_df$aspect_name <-ifelse(big_df$aspect == 2, "East",  big_df$aspect_name)
# big_df$aspect_name <-ifelse(big_df$aspect == 3, "South", big_df$aspect_name)
# big_df$aspect_name <-ifelse(big_df$aspect == 4, "West", big_df$aspect_name)
# 
# # capitalize names
# # aspect name
# big_df$basin_name <-stringr::str_to_title(big_df$basin_name)
# big_df$basin_name <-ifelse(big_df$basin_name == "Upper_sac",  "Upper Sacramento", big_df$basin_name)
# big_df$basin_name <-ifelse(big_df$basin_name == "Usj",        "Upper San Joaquin", big_df$basin_name)
# unique(big_df$basin_name)
# 
# fwrite(big_df, "./csvs/spearman_fm_temp_results/all_basins_spearman_results.csv", row.names = FALSE)

df <-fread("./csvs/spearman_fm_temp_results/all_basins_spearman_results.csv")
head(df)

# write function which caluclates pertaage of bin that is significant
results_v1 <-df %>%
  group_by(basin_name, zone_name) %>%
  summarise(percent_sig     = round((length(which(p_val < .05))/length(p_val))*100, 0))

test <-as.data.frame(results_v1 %>%
  pivot_wider(names_from = zone_name, values_from = percent_sig))

test

z %>%
  makeHeatmap(palette="Reds") %>%
  print(caption="Table 5. Heatmap table with 'Blue' palette")


