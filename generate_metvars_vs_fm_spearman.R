# generate spearman df by basin
# max swe
# jack tarricone
# june 11th, 2023

library(terra)
library(tidyverse)
library(dtplyr)
library(scales)
library(cowplot)
library(viridis)
library(data.table)

setwd("~/ch1_margulis")


# head_df for col referencing
df_paths <-list.files("./csvs/gridmet_dfs", full.names = TRUE, pattern = "full_stat")
df_list <-lapply(df_paths, fread)
colnames(df_list[[1]])[c(6,24)] <-c("mean_rh","rh_mean")
colnames(df_list[[2]])[c(6,24)] <-c("mean_rh","rh_mean")
colnames(df_list[[3]])[c(6,24)] <-c("mean_rh","rh_mean")

basin_list <-df_list[[3]]
variable <-"abs_hum_gcm3"

# funciton to create basin results dataframes
generate_spearman_df <-function(basin_list, variable){
  
  # remove nans and set key
  basin <-na.omit(basin_list) 
  setkey(basin, cell)
  
  # extract name
  basin_name <-basin$basin_name[1]

  # run using data.table -- super fast
  results_v1 <-basin[,list(corr = cor.test(frac_melt, get(variable),
                                      method = 'spearman', exact = FALSE)), by = cell]

  # convert back to data frame
  results_raw <-as.data.frame(unlist(results_v1$corr))
  head(results_raw)

  # extract results from unlist/unorganized df
  # p_val
  p_val <-as.data.frame(as.numeric(results_raw[seq(2, nrow(results_raw), 7), ]))
  names(p_val)[1] <-"p_val"
  head(p_val)

  # rho
  rho_val <-as.data.frame(as.numeric(results_raw[seq(3, nrow(results_raw), 7), ]))
  names(rho_val)[1] <-"rho_val"
  head(rho_val)

  # s_stat
  s_stat <-as.data.frame(as.numeric(results_raw[seq(1, nrow(results_raw), 7), ]))
  names(s_stat)[1] <-"s_stat"
  head(s_stat)

  # format static values for binding
  single_cell_df <-as.data.frame(basin %>% group_by(cell) %>%
    slice(which.min(x)) %>%
    select(-c(frac_melt,variable,wy,mswe_mm,mwa_mm,dom_dowy,
              rh_mean, abs_hum_gcm3, srad_wm2, sph_kgkg)))
  
  # make spearman df
  cell <-as.vector(unique(results_v1$cell))
  spearman_df <-data.frame(cell,p_val,rho_val,s_stat)
  head(spearman_df)
  
  hist(spearman_df$rho_val, breaks = 100)
  hist(spearman_df$p_val, breaks = 100)
  
  # and check the cells are teh same
  identical(cell, single_cell_df$cell)

  # combine both for full df
  results_df <-full_join(single_cell_df,spearman_df)
  colnames(results_df)[15] <-"mean_mswe_mm"
  head(results_df)
  
  # save
  saving_name <-paste0(basin_name,"_",variable,"_vs_fm_spearman_results_v2.csv")
  fwrite(results_df, paste0("./csvs/spearman_results/kern_yuba_usj/",saving_name))
  print(paste0(basin_name, " is done!"))
  
}

# testing
var_list <-c("mswe_mm","abs_hum_gcm3","temp_mean_c", "srad_wm2")

# apply function to four variables
# temp_mean_c
lapply(df_list, function(x) generate_spearman_df(x, variable = "temp_mean_c"))
temp_paths <-list.files("./csvs/spearman_results/kern_yuba_usj", full.names = TRUE, pattern = 'temp')
temp_list <-lapply(temp_paths, fread)
temp_df <-as.data.table(bind_rows(temp_list))
head(temp_df)
fwrite(temp_df,"./csvs/spearman_results/KUY_temp_mean_c_vs_fm_spearman_results_v2.csv")

# mswe_mm
lapply(df_list, function(x) generate_spearman_df(x, variable = "mswe_mm"))
mswe_paths <-list.files("./csvs/spearman_results/kern_yuba_usj", full.names = TRUE, pattern = 'mswe')
mswe_list <-lapply(mswe_paths, fread)
mswe_df <-as.data.table(bind_rows(mswe_list))
head(mswe_df)
fwrite(mswe_df,"./csvs/spearman_results/KUY_mswe_mm_vs_fm_spearman_results_v2.csv")

# srad
lapply(df_list, function(x) generate_spearman_df(x, variable = "srad_wm2"))
srad_paths <-list.files("./csvs/spearman_results/kern_yuba_usj", full.names = TRUE, pattern = 'srad')
srad_list <-lapply(srad_paths, fread)
srad_df <-as.data.table(bind_rows(srad_list))
head(srad_df)
fwrite(srad_df,"./csvs/spearman_results/KUY_srad_wm2_vs_fm_spearman_results_v2.csv")

# ah
lapply(df_list, function(x) generate_spearman_df(x, variable = "abs_hum_gcm3"))
ah_paths <-list.files("./csvs/spearman_results/kern_yuba_usj", full.names = TRUE, pattern = 'abs')
ah_list <-lapply(ah_paths, fread)
ah_df <-as.data.table(bind_rows(ah_list))
head(ah_df)
fwrite(ah_df,"./csvs/spearman_results/KUY_abs_hum_gcm3_vs_fm_spearman_results_v2.csv")

# rh mean
lapply(df_list, function(x) generate_spearman_df(x, variable = "rh_mean"))
rh_paths <-list.files("./csvs/spearman_results/kern_yuba_usj", full.names = TRUE, pattern = 'rh')
rh_list <-lapply(rh_paths, fread)
rh_df <-as.data.table(bind_rows(rh_list))
head(rh_df)
fwrite(rh_df,"./csvs/spearman_results/KUY_rh_mean_vs_fm_spearman_results_v2.csv")
