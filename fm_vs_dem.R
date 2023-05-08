# max vs dem
# may 2nd, 2023

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

# pull out north
ez_n_df <-subset(mean_ez_df, ez %in% c(1,3,5))
ez_s_df <-subset(mean_ez_df, ez %in% c(2,4,6))

# create plotting function
plot_dem_vs_fm <-function(df, bins, scale, title){
  
  plot <-ggplot() +
    geom_tile(data = mean_ez_df, aes(x = elevation, y = frac_melt), color = 'grey', fill = 'grey', width = 22, height = .01) +
    geom_bin2d(data = df, bins = bins, aes(x = elevation, y= frac_melt, fill = ..density..)) +
    scale_fill_gradientn(colors = scale) +
    scale_x_continuous(limits = c(1500,4300), expand = (c(0,0))) +
    scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
    labs(y = "FM", x = "Elevation (m)")+
    annotate(geom="text", y=.95, x=3600, label= title, size = 8, fontface = "bold")+
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
scale1 <-c("grey",viridis(30, option = "H", direction = 1))

# plot
ez_n_plot <-plot_dem_vs_fm(df = ez_n_df,
                             bins = 80,
                             scale = scale1,
                             title = "North Facing") 
# save
ggsave(ez_n_plot,
       file = "./plots/fm_vs_dem_north_plot_v2.png",
       width = 6,
       height = 5,
       dpi = 600)

system("open ./plots/fm_vs_dem_north_plot_v2.png")

# plot
ez_s_plot <-plot_dem_vs_fm(df = ez_s_df,
                            bins = 80,
                            scale = scale1,
                            title = "South Facing") 

# save
ggsave(ez_s_plot,
       file = "./plots/fm_vs_dem_south_plot_v1.png",
       width = 6,
       height = 5,
       dpi = 600)

system("open ./plots/fm_vs_dem_south_plot_v1.png")

# cowplot test
n_v_s <-plot_grid(ez_n_plot, ez_s_plot,
                  labels = c("(a)", "(b)"),
                  ncol = 2,
                  align = "hv",
                  label_size = 22,
                  vjust =  2.4,
                  hjust = 0,
                  rel_widths = c(1/2, 1/2))
# save
ggsave(n_v_s,
       file = "./plots/fm_dem_ns_v1.png",
       width = 12, 
       height = 5,
       dpi = 600)

system("open ./plots/fm_dem_ns_v1.png")
