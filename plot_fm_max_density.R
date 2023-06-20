# fm vs max
# june 20th, 2023

library(terra)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(cowplot)
library(viridis)
library(ggpointdensity)
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

# ez
ez <-rast("./rasters/categorized/dem_6zb.tif")
ez_df <-as.data.frame(ez, xy = TRUE, cells = TRUE)
colnames(ez_df)[4] <-"ez"

# aspect
aspect <-rast("./rasters/categorized/aspect_thres_4_classes.tif")
aspect_df <-as.data.frame(aspect, xy = TRUE, cells = TRUE)
colnames(aspect_df)[4] <-"aspect"
head(aspect_df)
hist(aspect_df$aspect)

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
mean_aspect_df <-subset(mean_df, cell %in% aspect_df$cell)
ez_filt <-subset(ez_df, cell %in% mean_aspect_df$cell)
dem_filt <-subset(dem_df, cell %in% ez_df$cell)
cc_filt <-subset(cc_df, cell %in% dem_df$cell)
fm_filt <-subset(fm_mean_df, cell %in% cc_df$cell)
aspect_filt <-subset(aspect_df, cell %in% fm_filt$cell)

# bind
mean_ez_df_v2 <-dplyr::full_join(mean_aspect_df, ez_filt)
mean_ez_df_v3 <-dplyr::full_join(mean_ez_df_v2, dem_filt)
mean_ez_df_v4 <-dplyr::full_join(mean_ez_df_v3, cc_filt)
mean_ez_df_v5 <-dplyr::full_join(mean_ez_df_v4, fm_filt)
mean_ez_df_v6 <-dplyr::full_join(mean_ez_df_v5, aspect_filt)
mean_ez_df <-mean_ez_df_v6  %>% drop_na()
head(mean_ez_df)

# pull out PZs
ez1_n_df <-subset(mean_ez_df, ez == 1 & aspect == 1)
ez2_n_df <-subset(mean_ez_df, ez == 2 & aspect == 1)
ez3_n_df <-subset(mean_ez_df, ez == 3 & aspect == 1)
ez4_n_df <-subset(mean_ez_df, ez == 4 & aspect == 1)
ez5_n_df <-subset(mean_ez_df, ez == 5 & aspect == 1)
ez6_n_df <-subset(mean_ez_df, ez == 6 & aspect == 1)

# pull out PZs
ez1_s_df <-subset(mean_ez_df, ez == 1 & aspect == 3)
ez2_s_df <-subset(mean_ez_df, ez == 2 & aspect == 3)
ez3_s_df <-subset(mean_ez_df, ez == 3 & aspect == 3)
ez4_s_df <-subset(mean_ez_df, ez == 4 & aspect == 3)
ez5_s_df <-subset(mean_ez_df, ez == 5 & aspect == 3)
ez6_s_df <-subset(mean_ez_df, ez == 6 & aspect == 3)

# create plotting function
plot_max_fm <-function(df, bins, scale, title){
  
  plot <-ggplot() +
    geom_tile(data = mean_ez_df, aes(y = frac_melt, x = max_swe_m), color = 'grey', fill = 'grey', width = 3/60, height = 1/60) +
    geom_bin2d(data = df, bins = bins, aes(y = frac_melt, x = max_swe_m, fill = after_stat(density))) +
    scale_fill_gradientn(colors = scale) +
    scale_y_continuous(limits = c(0,1), expand = (c(0,0))) +
    scale_x_continuous(limits = c(0,3),breaks = c(seq(0,3,1)), expand = (c(0,0))) +
    labs(x = "MSWE (m)", y = "FM")+
    annotate(geom="text", y=.95, x=2.5, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1), 
          aspect.ratio = 1,
          legend.position  = 'right',
          legend.title = element_blank(),
          plot.margin = unit(c(.25,.1,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(fill = guide_colorbar(direction = "vertical",
                                 label.position = 'right',
                                 title.hjust = .5,
                                 barwidth = 1,
                                 barheight = 20,
                                 frame.colour = "black", 
                                 ticks.colour = "black"))
  return(plot)
}

## set color
scale1 <-c("grey",viridis(30, option = "D", direction = 1))

# plot
### north
ez1_n_plot <-plot_max_fm(df = ez1_n_df, bins = 60, scale = scale1, title = "EZ1_N")
ez2_n_plot <-plot_max_fm(df = ez2_n_df, bins = 60, scale = scale1, title = "EZ2_N")
ez3_n_plot <-plot_max_fm(df = ez3_n_df, bins = 60, scale = scale1, title = "EZ3_N")
ez4_n_plot <-plot_max_fm(df = ez4_n_df, bins = 60, scale = scale1, title = "EZ4_N")
ez5_n_plot <-plot_max_fm(df = ez5_n_df, bins = 60, scale = scale1, title = "EZ5_N")
ez6_n_plot <-plot_max_fm(df = ez6_n_df, bins = 60, scale = scale1, title = "EZ6_N")

# south
ez1_s_plot <-plot_max_fm(df = ez1_s_df, bins = 60, scale = scale1, title = "EZ1_S")
ez2_s_plot <-plot_max_fm(df = ez2_s_df, bins = 60, scale = scale1, title = "EZ2_S")
ez3_s_plot <-plot_max_fm(df = ez3_s_df, bins = 60, scale = scale1, title = "EZ3_S")
ez4_s_plot <-plot_max_fm(df = ez4_s_df, bins = 60, scale = scale1, title = "EZ4_S")
ez5_s_plot <-plot_max_fm(df = ez5_s_df, bins = 60, scale = scale1, title = "EZ5_S")
ez6_s_plot <-plot_max_fm(df = ez6_s_df, bins = 60, scale = scale1, title = "EZ6_S")

# # save
# ggsave(ez1_n_plot,
#        file = "./plots/fm_max_ez1_n_plot_v6.png",
#        width = 6,
#        height = 5,
#        dpi = 600)
# 
# system("open ./plots/fm_max_ez1_n_plot_v6.png")

# cowplot test
cow <-plot_grid(ez1_n_plot, ez2_n_plot, ez3_n_plot,
               ez1_s_plot, ez2_s_plot, ez3_s_plot, 
               ez4_n_plot, ez5_n_plot, ez6_n_plot,
               ez4_s_plot, ez5_s_plot, ez6_s_plot,
               labels = c("(a)", "(b)","(c)",
                          "","","",
                          "(d)","(e)","(f)",
                          "","",""),
               ncol = 3,
               nrow = 4,
               align = "hv",
               label_size = 22,
               vjust =  2,
               hjust = -.2,
               rel_widths = c(1/3, 1/3, 1/3))
# save
ggsave(cow,
       file = "./plots/fm_max_cow_v5.png",
       width = 18, 
       height = 20,
       dpi = 300)

system("open ./plots/fm_max_cow_v5.png")
