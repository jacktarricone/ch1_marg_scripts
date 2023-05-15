# temp vs. fm multi regression
# jack tarricone
# may 15th, 2023

library(terra)
library(tidyr)
library(ggplot2)
library(scales)
library(cowplot)
library(viridis)
library(data.table)

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
#write.csv(plot_df, "./csvs/american_plot_df.csv")

# pull out north
plot_n_df <-subset(plot_df, ez %in% c(1,3,5))
plot_s_df <-subset(plot_df, ez %in% c(2,4,6))

# create plotting function
plot_temp_vs_fm <-function(df, scale, title){
  
  plot <-ggplot() +
    geom_point(data = plot_df, aes(y = frac_melt, x= temp_deg_c, color = watts), alpha = .2) +
    scale_color_gradientn(colors = scale, name = expression(SW ~ '(W m'^{"-2"} ~ ')')) +
    scale_x_continuous(limits = c(-2,7), expand = (c(0,0))) +
    scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
    labs(x = "Mean Temperature (°C)", y = "FM")+
    annotate(geom="text", x = .5, y = .93, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          aspect.ratio = 1,
          legend.position  = 'right',
          plot.margin = unit(c(.25,.1,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(color = guide_colorbar(direction = "vertical",
                                 label.position = 'right',
                                 title.hjust = .5,
                                 barwidth = 1,
                                 barheight = 16,
                                 frame.colour = "black", 
                                 ticks.colour = "black"))
  return(plot)
}

## set color
scale2 <-c("grey",viridis(30, option = "B", direction = 1))

# plot
temp_fm_plot <-plot_temp_vs_fm(df = plot_df,
                                scale = scale2,
                                title = "American") 

plot(temp_fm_plot)

# save
ggsave(temp_fm_plot,
       file = "./plots/temp_fm_watts_v2.png",
       width = 6, 
       height = 5,
       dpi = 600)

system("open ./plots/temp_fm_watts_v2.png")





# plot
temp_fm_plot <-plot_temp_vs_dem(df = plot_df,
                                  bins = 100,
                                  scale = scale2,
                                  title = "American") 

plot(temp_fm_plot)

# plot
temp_ez_s_plot <-plot_temp_vs_dem(df = plot_s_df,
                                  bins = 100,
                                  scale = scale2,
                                  title = "South Facing") 
plot(temp_ez_s_plot)

# cowplot test
temp_n_v_s <-plot_grid(temp_ez_n_plot, temp_ez_s_plot,
                       labels = c("(a)", "(b)"),
                       ncol = 2,
                       align = "hv",
                       label_size = 22,
                       vjust =  2.4,
                       hjust = 0,
                       rel_widths = c(1/2, 1/2))
# save
ggsave(temp_n_v_s,
       file = "./plots/american_temp_fm_ns_v2.png",
       width = 12.5, 
       height = 5,
       dpi = 600)

system("open ./plots/american_temp_fm_ns_v2.png")
