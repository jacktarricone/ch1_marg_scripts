####### start mk results analysis table
# jack tarricone
# april 19th, 2023

library(terra)

# set working dir
setwd("~/ch1_margulis/")


# mwa
mwa_mean <-rast("./rasters/snow_metric_averages/mwa_djfm_v1.tif")
mwa_mean_df <-as.data.frame(mwa_mean, xy = TRUE, cells = TRUE)
colnames(mwa_mean_df)[4] <-"mwa_djfm_mm"
head(mwa_mean_df)

# max
max_mean <-rast("./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif")
max_mean_df <-as.data.frame(max_mean, xy = TRUE, cells = TRUE)
colnames(max_mean_df)[4] <-"max_swe_mm"
head(max_mean_df)

# bring in max dowy mean
dom_mean <-rast("./rasters/snow_metric_averages/dom_mean_f_25mm_27obs.tif")
dom_mean_df <-as.data.frame(dom_mean, xy = TRUE, cells = TRUE)
colnames(dom_mean_df)[4] <-"dom_dowy"
head(dom_mean_df)

# aspect
ez <-rast("./rasters/categorized/dem_ez3_ns.tif")
ez_df <-as.data.frame(ez, xy = TRUE, cells = TRUE)
colnames(ez_df)[4] <-"ez"
head(ez_df)

# join
mean_df <-dplyr::full_join(max_mean_df, dom_mean_df)
mean_df$max_swe_m <-mean_df$max_swe_mm/1000
head(mean_df)

# fitlering for same cells
mean_aspect_df <-subset(mean_df, cell %in% ez_df$cell)
ez_filt <-subset(ez_df, cell %in% mean_aspect_df$cell)
mwa_filt <-subset(mwa_mean_df, cell %in% ez_filt$cell)

# bind
mean_ez_df_v2 <-dplyr::full_join(mean_aspect_df, ez_filt)
mean_ez_df_v3 <-dplyr::full_join(mean_ez_df_v2, mwa_filt)
mean_ez_df_v4 <-mean_ez_df_v3  %>% drop_na()
head(mean_ez_df_v4)

# pull out north
ez1_n_df <-subset(mean_ez_df_v4, ez == 1)
ez1_s_df <-subset(mean_ez_df_v4, ez == 2)
ez2_n_df <-subset(mean_ez_df_v4, ez == 3)
ez2_s_df <-subset(mean_ez_df_v4, ez == 4)
ez3_n_df <-subset(mean_ez_df_v4, ez == 5)
ez3_s_df <-subset(mean_ez_df_v4, ez == 6)

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

# make fun
create_avg_table <-function(x){
    
    # pull out name 
    pz_name <-names(ez_list[x])
    print(pz_name)
    
    # pull out df
    df <-ez_list[[x]]
  
    # header for table
    header <-c("PZ", "Mean Max SWE (m)", "SD Max SWE (m)", "Mean DOM (DOWY)", "SD DOM (DOWY)", 
               "Mean MWA (cm)", "SD MWA (cm)")
    
    ### calc metrics
    # max
    mean_max <-round(mean(df$max_swe_m),2)
    sd_max <-round(sd(df$max_swe_m),2)
    
    # dom
    mean_dom <-round(mean(df$dom_dowy),0)
    sd_dom <-round(sd(df$dom_dowy),0)
    
    # dom
    mean_mwa <-round(mean((df$mwa_djfm_mm)/10),1)
    sd_mwa <-round(sd((df$mwa_djfm_mm)/10),1)
    
    # bind values together
    vals <-c(pz_name,mean_max,sd_max,mean_dom,sd_dom,mean_mwa,sd_mwa)
   
    # vals
    vals_w_header <-as.data.frame(t(vals))
    names(vals_w_header)[1:7] <-header
    # vals_w_header
    return(vals_w_header)
}

# test function
seq_list <-1:6
stats_list <-lapply(seq_list, create_avg_table)

# make df
results_df <-dplyr::bind_rows(stats_list)
print(results_df)
# write.csv(mk_results_df, "./csvs/max_mk_results_table_v2.csv", row.names = FALSE)

