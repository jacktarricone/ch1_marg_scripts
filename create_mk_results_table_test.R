####### start mk results analysis table
# jack tarricone
# april 19th, 2023

library(terra)

# set working dir
setwd("~/ch1_margulis/")

# bring in max mk raster
max_trend <-rast("./rasters/mk_results/max_trend.tif")
max_trend
hist(max_trend, breaks = 100)

# bring in sig trends
max_sig_trend <-rast("./rasters/mk_results/max_sig_trend.tif")

# list shape files
snsr_basin_paths <-list.files("./vectors/ca_basins", pattern = "\\.gpkg$", full.names = TRUE)
snsr_shp_list <-lapply(snsr_basin_paths, vect)

# rough format names
names_raw <-print(basename(snsr_basin_paths))
names <-gsub(".gpkg","",names_raw)
names

create_max_mk_table <-function(x, full_trend, sig_trend, names){

    header <-c("Basin", "Mean Max SWE Trend (cm/decade)", "Max SWE Trend SD (cm/decade)", 
            "Negative Trends (%)", "Sig. Trends (% of area)")
    
    # load in shape file
    shp_file <-vect(x)
    
    ### crop and mask full shape for basin
    basin_trend <-crop(mask(full_trend, shp_file),shp_file)
    plot(basin_trend)

    basin_sig_trend <-crop(mask(sig_trend , shp_file),shp_file)
    plot(basin_sig_trend)

    ### calc trend mean
    basin_trend_mean <-round(as.numeric(global(basin_trend, 
                                               mean, na.rm = TRUE)),2)

    ### calc trend sd
    basin_trend_sd <-round(as.numeric(global(basin_trend, 
                                             sd, na.rm = TRUE)),2)

    ### calc percent of negative trends
    total_pixels <-as.numeric(global(basin_trend, 
                                     fun="notNA"), na.rm = TRUE) # count pixels not na

    neg_pixels <-as.numeric(global(basin_trend < 0, 
                                   sum, na.rm=TRUE)) # count pixels with negative trend, https://stackoverflow.com/questions/67442419/number-of-cells-of-raster-under-a-condition

    percent_neg_trend <-round((neg_pixels/total_pixels)*100,1) # calculate percentage

    ### calc percent of negative trends
    sig_pixels <-as.numeric(global(basin_sig_trend, fun="notNA")) # count sig pixels
    percent_sig_trend <-round((sig_pixels/total_pixels)*100,1) # calculate percentage
    
    # rough format names
    name_raw <-basename(x)
    name <-gsub(".gpkg","",name_raw)
    print(name)
    
    # bind values together
    vals <-c(name,basin_trend_mean,basin_trend_sd, percent_neg_trend, percent_sig_trend)
   
     # vals
    vals_w_header_v1 <-as.data.frame(rbind(header,vals), row.names = FALSE)
    vals_w_header <-janitor::row_to_names(vals_w_header_v1, row_number = 1)
    return(vals_w_header)
}

# test function
mk_stats_list_basin <-lapply(snsr_basin_paths, function(x) create_max_mk_table(x,
                                                                               full_trend = max_trend, 
                                                                               sig_trend = max_sig_trend, 
                                                                               names = names))

# make df
mk_results_df <-dplyr::bind_rows(mk_stats_list_basin)
print(mk_results_df)
write.csv(mk_results_df, "./csvs/max_mk_results_table_v2.csv", row.names = FALSE)

