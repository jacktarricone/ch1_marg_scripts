####### start mk results analysis table
# jack tarricone
# april 19th, 2023

library(terra)

# set working dir
setwd("~/ch1_margulis")

# mwa
dem <-rast("./rasters/static/SNSR_DEM.tif")

# max
max_mean <-rast("./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif")

# bring in max dowy mean
dom_mean <-rast("./rasters/snow_metric_averages/dom_mean_f_25mm_27obs.tif")

# aspect
ez <-rast("./rasters/categorized/dem_ez3_ns.tif")

# list shape files
snsr_basin_paths <-list.files("./vectors/ca_basins", pattern = "\\.gpkg$", full.names = TRUE)
snsr_shp_list <-lapply(snsr_basin_paths, vect)

# rough format names
names_raw <-print(basename(snsr_basin_paths))
names <-gsub(".gpkg","",names_raw)
names


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
create_avg_table <-function(x, full_trend, sig_trend, names){
    
      # list shape files
  snsr_basin_paths <-list.files("./vectors/ca_basins", pattern = "\\.gpkg$", full.names = TRUE)
  snsr_shp_list <-lapply(snsr_basin_paths, vect)
  
  # rough format names
  names_raw <-print(basename(snsr_basin_paths))
  names <-gsub(".gpkg","",names_raw)
  names
  
      
    # pull out name 
    pz_name <-names(ez_list[i])
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
}

# test function
seq_list <-1:6
stats_list <-lapply(seq_list, create_avg_table)

# make df
results_df <-dplyr::bind_rows(stats_list)
print(results_df)
write.csv(results_df, "./csvs/avg_metric_results_table.csv", row.names = FALSE)
system("open ./csvs/avg_metric_results_table.csv")
