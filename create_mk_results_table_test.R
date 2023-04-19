####### start mk results analysis table
# jack tarricone
# april 19th, 2023

library(terra)

# set working dir
setwd("~/ch1_margulis/")

# bring in max mk raster
max_trend <-rast("./rasters/mk_results/max_slope_full.tif")
plot(max_trend)

max_sig_trend_v1 <-rast("./rasters/mk_results/max_sig_slope.tif")
max_sig_trend  <-subst(max_sig_trend_v1, 0, NA)
plot(max_sig_trend)

# list shape files
snsr_basin_paths <-list.files("./vectors/ca_basins/", pattern = "\\.gpkg$", full.names = TRUE)
snsr_shp_list <-lapply(snsr_basin_paths, vect)

# rough format names
names_raw <-print(basename(snsr_basin_paths))
names <-gsub(".gpkg","",names_raw)
names

basin_mask <-crop(mask(max_trend, snsr_shp_list[[18]]),snsr_shp_list[[18]])
plot(basin_mask)
hist(basin_mask, breaks = 100)

headers <-c("Basin", "Mean Max SWE Trend (mm/yr)", "Max SWE Trend SD (mm/yr)", 
            "Negative Trends (%)", "Sig. Trends (% of area)")

# calc trend mean
basin_trend_mean <-round(as.numeric(global(basin_mask, mean, na.rm = TRUE)),2)

# calc trend sd
basin_trend_sd <-round(as.numeric(global(basin_mask, sd, na.rm = TRUE)),2)

x <-basin_mask

# calc percent of negative trends
percent_neg_trend <-function(x){
  total_pixels <-as.numeric(global(x, fun="notNA")) # count pixels not na
  neg_pixels <-as.numeric(global(x, function(x) sum(x < 0, na.rm=TRUE))) # count pixels with negative trend
  perc_neg <-round((neg_pixels/total_pixels)*100,1) # calculate percentage
  return(perc_neg)
}

global(basin_mask, function(x) sum(x < 0, na.rm=TRUE))


headers <-c("Basin", "Mean Max SWE Trend (mm/yr)", "Max SWE Trend SD (mm/yr)", 
            "Negative Trends (%)", "Sig. Trends (% of area)")


## bring in SNSR shape file
# with terra
snsr <-vect("./vectors/snsr_shp.gpkg")
snsr_basins <-vect("./vectors/ca_basins/snsr_all_basins.shp")
plot(snsr_basins, add = TRUE)

# with sf
snsr_v1 <-st_read("./vectors/snsr_shp.gpkg")
snsr_sf <-st_geometry(snsr_v1)

# with sf
snsr_basins_v1 <-st_read("./vectors/ca_basins/snsr_all_basins.shp")
snsr_basins_sf <-st_geometry(snsr_basins_v1)
