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

# mwa 
mwa_mean <-rast("./rasters/snow_metric_averages/mwa_djfm_v1.tif")

# aspect
ez <-rast("./rasters/categorized/dem_ez3_ns.tif")

# list shape files
snsr_basin_paths <-list.files("./vectors/ca_basins", pattern = "\\.gpkg$", full.names = TRUE)
snsr_shp_list <-lapply(snsr_basin_paths, vect)

# rough format names
names_raw <-print(basename(snsr_basin_paths))
names <-gsub(".gpkg","",names_raw)
names

# names
ez_names <-c("EZ1_N", "EZ1_S", 
             "EZ2_N", "EZ2_S", 
             "EZ3_N", "EZ3_S")

snsr_paths_test <-snsr_basin_paths[3:4]
storage_list <-list()

# make fun
for (i in 1:length(snsr_basin_paths)){
    
    # pull out name
    file <-snsr_basin_paths[[i]]
    name_raw <-basename(file)
    basin_name <-gsub(".gpkg","",name_raw)
    print(basin_name)

    # header for table
    header <-c("Basin","PZ", "Mean Max SWE (m)", "SD Max SWE (m)", "Mean DOM (DOWY)", "SD DOM (DOWY)", 
               "Mean MWA (cm)", "SD MWA (cm)")
    
    # load in shape file
    shp_file <-vect(file)
    
    ### crop and mask full shape for basin
    max_full <-crop(mask(max_mean, shp_file),shp_file)
    dom_full <-crop(mask(dom_mean, shp_file),shp_file)
    mwa_full <-crop(mask(mwa_mean, shp_file),shp_file)
    ez_basin <-crop(mask(ez, shp_file),shp_file)
    
    # storage mat
    output <-matrix(ncol = length(vals), nrow = length(ez_names))
    
    for (j in 1:length(ez_names)){
      
      # pull out name 
      pz_name <-ez_names[j]
      print(pz_name)
      
      # mask for ex
      max_mask <-terra::mask(max_full, ez_basin, maskvalue = j, inverse = TRUE)
      dom_mask <-terra::mask(dom_full, ez_basin, maskvalue = j, inverse = TRUE)
      mwa_mask <-terra::mask(mwa_full, ez_basin, maskvalue = j, inverse = TRUE)
      
      ### calc metrics
      # max
      mean_max <-round(as.numeric(global(max_mask/1000, mean, na.rm = TRUE)),2)
      sd_max <-round(as.numeric(global(max_mask/1000, sd, na.rm = TRUE)),2)
    
      # dom
      mean_dom <-round(as.integer(global(dom_mask, mean, na.rm = TRUE)),2)
      sd_dom <-round(as.integer(global(dom_mask, sd, na.rm = TRUE)),2)
    
      # dom
      mean_mwa <-round(as.numeric(global(mwa_mask/10, mean, na.rm = TRUE)),1)
      sd_mwa <-round(as.numeric(global(mwa_mask/10, sd, na.rm = TRUE)),1)
    
      # bind values together
      vals <-c(basin_name, pz_name,mean_max,sd_max,mean_dom,sd_dom,mean_mwa,sd_mwa)
   
      # fill mat by row
      output[j,] <-vals
      colnames(output)[1:8] <-header
      output_df <-as.data.frame(output)
    }
    
    storage_list[[i]] <-output_df
}


# make df
results_df <-dplyr::bind_rows(storage_list)
print(results_df)
write.csv(results_df, "./csvs/avg_metric_results_table_by_basin.csv", row.names = FALSE)
system("open ./csvs/avg_metric_results_table_by_basin.csv")
