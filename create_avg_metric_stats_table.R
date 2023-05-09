####### start mk results analysis table
# jack tarricone
# april 19th, 2023

library(terra)

setwd("~/ch1_margulis")

# set custom plot theme
theme_classic <-function(base_size = 11, base_family = "",
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22) {
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # no background and no grid
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # show axes
      # axis.line      = element_line(colour = "black", linewidth = rel(1)),
      
      # match legend key to panel.background
      legend.key       = element_blank(),
      
      # simple, black and white strips
      strip.background = element_rect(fill = "white", colour = "black", linewidth = rel(2)),
      # NB: size is 1 but clipped, it looks like the 0.5 of the axes
      
      complete = TRUE
    )
}

theme_set(theme_classic(14))

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
hist(ez_df$ez)

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

# bind
mean_ez_df_v2 <-dplyr::full_join(mean_aspect_df, ez_filt)
mean_ez_df_v3 <-dplyr::full_join(mean_ez_df_v2, dem_filt)
mean_ez_df_v4 <-dplyr::full_join(mean_ez_df_v3, cc_filt)
mean_ez_df_v5 <-dplyr::full_join(mean_ez_df_v4, fm_filt)
mean_ez_df <-mean_ez_df_v5  %>% drop_na()
head(mean_ez_df)

# pull out PZs
ez1_n_df <-subset(mean_ez_df_v5, ez == 1)
ez1_s_df <-subset(mean_ez_df_v5, ez == 2)
ez2_n_df <-subset(mean_ez_df_v5, ez == 3)
ez2_s_df <-subset(mean_ez_df_v5, ez == 4)
ez3_n_df <-subset(mean_ez_df_v5, ez == 5)
ez3_s_df <-subset(mean_ez_df_v5, ez == 6)

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
    header <-c("PZ", "Mean MSWE (m)", "SD MSWE (m)", "Mean DOM (DOWY)", "SD DOM (DOWY)", 
               "Mean FM", "SD FM")
    
    ### calc metrics
    # max
    mean_max <-round(mean(df$max_swe_m),2)
    sd_max <-round(sd(df$max_swe_m),2)
    
    # dom
    mean_dom <-round(mean(df$dom_dowy),0)
    sd_dom <-round(sd(df$dom_dowy),0)
    
    # dom
    mean_fm <-round(mean(df$frac_melt),2)
    sd_fm <-round(sd(df$frac_melt),2)
    
    # bind values together
    vals <-c(pz_name,mean_max,sd_max,mean_dom,sd_dom,mean_fm,sd_fm)
   
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


write.csv(results_df, "./csvs/avg_metric_results_table_v2.csv", row.names = FALSE)
system("open ./csvs/avg_metric_results_table_v2.csv")
