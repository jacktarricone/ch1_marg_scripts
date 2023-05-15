# temp vs. fm multi regression
# jack tarricone
# may 15th, 2023

library(terra)

setwd("~/ch1_margulis")

# bring in data
american <-vect("./vectors/ca_basins/american.gpkg")

insol_v1 <-rast("./rasters/insolation/snsr_dem_insol_watts_masked_v1.tif")
fm_v1 <-rast("./rasters/snow_metric_averages/fm_mean_f_25mm_27obs.tif")
temp_v1 <-rast("./rasters/prism/prism_tmean_snsr_ondjfm.tif")
dem_v1 <-rast("./rasters/static/SNSR_DEM.tif")
slope_v1 <-terrain(dem_v1, v="slope", neighbors=8, unit="degrees")
ez_v1 <-rast("./rasters/categorized/dem_ez3_ns.tif")

# format
insol <-mask(crop(insol_v1,ext(american)), american)
fm <-mask(crop(fm_v1,ext(american)), american)
temp <-mask(crop(temp_v1,ext(american)), american)
dem <-mask(crop(dem_v1,ext(american)), american)
slope <-mask(crop(slope_v1,ext(american)), american)
ez <-mask(crop(ez_v1,ext(american)), american)

# convert to df
temp_df <-as.data.frame(temp, xy = TRUE, cells = TRUE)
colnames(temp_df)[4] <-"temp_deg_c"
insol_df <-as.data.frame(insol, xy = TRUE, cells = TRUE)
colnames(insol_df)[4] <-"watts"
fm_df <-as.data.frame(fm, xy = TRUE, cells = TRUE)
colnames(fm_df)[4] <-"frac_melt"
dem_df <-as.data.frame(dem, xy = TRUE, cells = TRUE)
colnames(dem_df)[4] <-"elevation"
ez_df <-as.data.frame(ez, xy = TRUE, cells = TRUE)
colnames(ez_df)[4] <-"ez"
slope_df <-as.data.frame(slope, xy = TRUE, cells = TRUE)
colnames(slope_df)[4] <-"slope_deg"

plot(slope)
head(ez_df)
head(slope_df)

# filter
fm_df_v1 <-subset(fm_df, cell %in% ez_df$cell)
insol_df_v1 <-subset(insol_df, cell %in% fm_df_v1$cell)
temp_df_v1 <-subset(temp_df, cell %in% insol_df_v1$cell)
dem_df_v1 <-subset(dem_df, cell %in% temp_df_v1$cell)
slope_df_v1 <-subset(slope_df, cell %in% dem_df_v1$cell)
ez_df_v1 <-subset(ez_df, cell %in% slope_df_v1$cell)

# join
plot_df_v1 <-dplyr::full_join(fm_df_v1, insol_df_v1)
plot_df_v2 <-dplyr::full_join(plot_df_v1, temp_df_v1)
plot_df_v3 <-dplyr::full_join(plot_df_v2, dem_df_v1)
plot_df_v4 <-dplyr::full_join(plot_df_v3, slope_df_v1)
plot_df_v5 <-dplyr::full_join(plot_df_v4, ez_df_v1)
plot_df <-plot_df_v5  %>% tidyr::drop_na()
head(plot_df)
