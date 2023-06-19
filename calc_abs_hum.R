# add abs_hum to col to basin dfs
# jack tarricone
# june 11th, 2023

library(tidyverse)
library(data.table)

setwd("~/ch1_margulis")

# list of paths to shape files
basin_paths <-list.files("./csvs/gridmet_dfs/", full.names = TRUE, pattern = "full_stats")

# head_df for col referencing
head_df <-fread("./csvs/gridmet_dfs/head_df.csv")
head(head_df)

# read in full df
# basin_df_list <-lapply(basin_paths, function(x) fread(x, nThread=14))

# define abs_hum function
absolute_humidity <- function(df) {
      df$abs_hum_gcm3 <-(6.112 * exp((17.67 * df$temp_mean_c) / (df$temp_mean_c + 243.5)) * df$'rh_mean_%' * 2.1674) / (273.15 + df$temp_mean_c)
      return(df)
    }
 
# apply to list of dfs   
abs_list <-lapply(basin_df_list, absolute_humidity)

# save the updated data frames
for (i in 1:length(abs_list)) {
    
    # pull out orginal names
    df_name <-basename(basin_paths[[i]])
    fwrite(abs_list[[i]], file = paste0(df_name, "_v2.csv"))
    
}
  
