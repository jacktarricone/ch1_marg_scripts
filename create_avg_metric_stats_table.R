####### start mk results analysis table
# jack tarricone
# april 19th, 2023

library(terra)
library(dplyr)
library(data.table)

setwd("~/ch1_margulis")

# read in df
# fm
fm_mean <-rast("./rasters/snow_metric_averages/fm_mean_f_25mm_27obs.tif")
fm_mean_df <-as.data.frame(fm_mean, xy = TRUE, cells = TRUE)
colnames(fm_mean_df)[4] <-"frac_melt"
head(fm_mean_df)

# max
max_mean <-rast("./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif")
max_mean_df <-as.data.frame(max_mean, xy = TRUE, cells = TRUE)
colnames(max_mean_df)[4] <-"max_swe_mm"
head(max_mean_df)

# dom
dom_mean <-rast("./rasters/snow_metric_averages/dom_mean_f_25mm_27obs.tif")
dom_mean_df <-as.data.frame(dom_mean, xy = TRUE, cells = TRUE)
colnames(dom_mean_df)[4] <-"dom_dowy"
head(dom_mean_df)

# aspect
ez <-rast("./rasters/categorized/dem_ez3_ns.tif")
ez_df <-as.data.frame(ez, xy = TRUE, cells = TRUE)
colnames(ez_df)[4] <-"ez"
head(ez_df)

# dem
dem <-rast("./rasters/static/SNSR_DEM.tif")
dem_df <-as.data.frame(dem, xy = TRUE, cells = TRUE)
colnames(dem_df)[4] <-"elevation"
head(dem_df)

# cc
cc <-rast("./rasters/nlcd_cc/cc_w0.tif")
cc_df <-as.data.frame(cc, xy = TRUE, cells = TRUE)
colnames(cc_df)[4] <-"cc_percent"
head(cc_df)

# temp
temp <-rast("./rasters/prism/prism_tmean_snsr_ondjfm.tif")
temp_df <-as.data.frame(temp, xy = TRUE, cells = TRUE)
colnames(temp_df)[4] <-"temp_c"
head(temp_df)

# insol
insol <-rast("./rasters/insolation/snsr_dem_insol_v2.tif")
insol_df <-as.data.frame(insol, xy = TRUE, cells = TRUE)
colnames(insol_df)[4] <-"insol_kwh"
head(insol_df)

insol_watts <-rast("./rasters/insolation/snsr_dem_insol_watts_masked_v1.tif")
insol_watts_df <-as.data.frame(insol_watts, xy = TRUE, cells = TRUE)
colnames(insol_df)[4] <-"insol_watts"


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

# make fun
create_avg_table <-function(x){
    
    # pull out name 
    pz_name <-names(ez_list[x])
    print(pz_name)
    
    # pull out df
    df <-ez_list[[x]]
  
    # header for table
    header <-c("PZ", 
               "Mean Elevation (m)", "SD Elevation (m)",
               "Mean Temp (C)", "SD Temp (C)",
               "Mean Insolation (kHw m^-2)", "SD Insolation (kHw m^-2)",
               "Mean CC (%)", "SD CC (%)",
               "Mean MSWE (m)", "SD MSWE (m)", 
               "Mean DOM (DOWY)", "SD DOM (DOWY)", 
               "Mean FM", "SD FM")
    
    ### calc metrics
    # elevation
    mean_ele <-round(mean(df$elevation),0)
    sd_ele <-round(sd(df$elevation),0)
    
    # temp
    mean_temp <-round(mean(df$temp_c),1)
    sd_temp <-round(sd(df$temp_c),1)
    
    # temp
    mean_insol <-round(mean(df$insol_kwh),1)
    sd_insol <-round(sd(df$insol_kwh),1)
    
    # cc
    mean_cc <-round(mean(df$cc_percent),0)
    sd_cc <-round(sd(df$cc_percent),0)
    
    # mswe
    mean_max <-round(mean(df$max_swe_m),2)
    sd_max <-round(sd(df$max_swe_m),2)
    
    # dom
    mean_dom <-round(mean(df$dom_dowy),0)
    sd_dom <-round(sd(df$dom_dowy),0)
    
    # fm
    mean_fm <-round(mean(df$frac_melt),2)
    sd_fm <-round(sd(df$frac_melt),2)
    
    # bind values together
    vals <-c(pz_name,
             mean_ele,sd_ele,
             mean_temp,sd_temp,
             mean_insol,sd_insol,
             mean_cc,sd_cc,
             mean_max,sd_max,
             mean_dom,sd_dom,
             mean_fm,sd_fm)
   
    # vals
    vals_w_header <-as.data.frame(t(vals))
    names(vals_w_header)[1:15] <-header
    # vals_w_header
    return(vals_w_header)
}

# test function
seq_list <-1:6
stats_list <-lapply(seq_list, create_avg_table)

# make df
results_df <-dplyr::bind_rows(stats_list)
print(results_df)

# save
write.csv(results_df, "./csvs/avg_metric_results_table_v3.csv", row.names = FALSE)
system("open ./csvs/avg_metric_results_table_v3.csv")
