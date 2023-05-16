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

# bring vectors
american <-vect("./vectors/ca_basins/american.gpkg")

# rasters
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
fm_df_v1 <-subset(fm_df, cell %in% slope_df$cell)
insol_df_v1 <-subset(insol_df, cell %in% fm_df_v1$cell)
temp_df_v1 <-subset(temp_df, cell %in% insol_df_v1$cell)
dem_df_v1 <-subset(dem_df, cell %in% temp_df_v1$cell)
slope_df_v1 <-subset(slope_df, cell %in% dem_df_v1$cell)
# ez_df_v1 <-subset(ez_df, cell %in% slope_df_v1$cell)

# join
plot_df_v1 <-dplyr::full_join(fm_df_v1, insol_df_v1)
plot_df_v2 <-dplyr::full_join(plot_df_v1, temp_df_v1)
plot_df_v3 <-dplyr::full_join(plot_df_v2, dem_df_v1)
plot_df_v4 <-dplyr::full_join(plot_df_v3, slope_df_v1)
# plot_df_v5 <-dplyr::full_join(plot_df_v4, ez_df_v1)
plot_df <-plot_df_v4  %>% tidyr::drop_na()
head(plot_df)
#write.csv(plot_df, "./csvs/american_plot_df.csv")
plot_df <-fread("./csvs/american_plot_df.csv")

# # pull out north
# plot_n_df <-subset(plot_df, ez %in% c(1,3,5))
# plot_s_df <-subset(plot_df, ez %in% c(2,4,6))

# create plotting function
plot_temp_vs_fm <-function(df, scale, title){
  
  plot <-ggplot() +
    geom_point(data = plot_df, aes(y = frac_melt, x= temp_deg_c, color = watts), alpha = .2, size = .5) +
    scale_color_gradientn(colors = scale, name = expression(atop("Mean SW",paste(~'(W m'^{"-2"},')')))) +
    scale_x_continuous(limits = c(-8,8), expand = (c(0,0))) +
    scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
    labs(x = "Mean Temperature (°C)", y = "FM")+
    annotate(geom="text", x = -5, y = .93, label= title, size = 8, fontface = "bold")+
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
scale2 <-c(viridis(30, option = "D", direction = 1))

# plot
american_temp_fm_plot <-plot_temp_vs_fm(df = plot_df,
                                scale = scale2,
                                title = "American") 

# save
ggsave(american_temp_fm_plot,
       file = "./plots/american_temp_fm_watts_v7.png",
       width = 6, 
       height = 5,
       dpi = 600)

system("open ./plots/american_temp_fm_watts_v7.png")


hist(plot_df$watts, breaks = 100)

# filter watts
w50 <-filter(plot_df, watts <= 50)
w100 <-filter(plot_df, watts > 50 & watts <= 100)
w150 <-filter(plot_df, watts > 100 & watts <= 150)
w200 <-filter(plot_df, watts > 150 & watts <= 200)
w250 <-filter(plot_df, watts > 200 & watts <= 250)

hist(w50$watts, breaks = 100)
lm(w)