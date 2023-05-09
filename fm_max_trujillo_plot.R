# fm vs max
# april 6, 2023

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


# create plotting function
plot_max_fm <-function(df, bins, scale, title){
  
  plot <-ggplot() +
    geom_tile(data = mean_ez_df, aes(y = frac_melt, x = max_swe_m), color = 'grey', fill = 'grey', width = 2/100, height = 1/100) +
    geom_bin2d(data = df, bins = bins, aes(y = frac_melt, x = max_swe_m, fill = ..density..)) +
    scale_fill_gradientn(colors = scale) +
    scale_y_continuous(limits = c(0,1), expand = (c(0,0))) +
    scale_x_continuous(limits = c(0,2),breaks = c(seq(0,2,1)), expand = (c(0,0))) +
    labs(x = "MSWE (m)", y = "FM")+
    annotate(geom="text", y=.95, x=1.7, label= title, size = 8, fontface = "bold")+
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
ez1_n_plot <-plot_max_fm(df = ez1_n_df,
                          bins = 100,
                          scale = scale1,
                          title = "EZ1_N") 
# save
ggsave(ez1_n_plot,
       file = "./plots/fm_max_ez1_n_plot_v3.png",
       width = 6,
       height = 5,
       dpi = 600)

system("open ./plots/fm_max_ez1_n_plot_v3.png")

# plot
ez1_s_plot <-plot_max_fm(df = ez1_s_df,
                          bins = 100,
                          scale = scale1,
                          title = "EZ1_S") 

# plot
ez2_n_plot <-plot_max_fm(df = ez2_n_df,
                          bins = 100,
                          scale = scale1,
                          title = "EZ2_N") 

ez2_s_plot <-plot_max_fm(df = ez2_s_df,
                          bins = 100,
                          scale = scale1,
                          title = "EZ2_S") 

# plot
ez3_n_plot <-plot_max_fm(df = ez3_n_df,
                          bins = 100,
                          scale = scale1,
                          title = "EZ3_N")


ez3_s_plot <-plot_max_fm(df = ez3_s_df,
                          bins = 100,
                          scale = scale1,
                          title = "EZ3_S") 

# cowplot test
all_six_ez <-plot_grid(ez1_n_plot, ez2_n_plot, ez3_n_plot,
                       ez1_s_plot, ez2_s_plot, ez3_s_plot, 
                       labels = c("(a)", "(b)","(c)","(d)","(e)","(f)"),
                       ncol = 3,
                       nrow = 2,
                       align = "hv",
                       label_size = 22,
                       vjust =  2,
                       hjust = -.2,
                       rel_widths = c(1/3, 1/3, 1/3))
# save
ggsave(all_six_ez,
       file = "./plots/fm_max_ez6_v2.png",
       width = 18, 
       height = 10,
       dpi = 600)

system("open ./plots/fm_max_ez6_v2.png")






#################
### dom vs fm ###
#################


plot_dom_fm <-function(df, bins, scale, title){
  
  plot <-ggplot() +
    geom_tile(data = mean_ez_df, aes(y = frac_melt, x = dom_dowy), color = 'grey', fill = 'grey', width = 150/100, height = 1/100) +
    geom_bin2d(data = df, bins = bins, aes(y = frac_melt, x = dom_dowy, fill = ..density..)) +
    scale_fill_gradientn(colors = scale) +
    scale_y_continuous(limits = c(0,1), expand = (c(0,0))) +
    scale_x_continuous(limits = c(100,250),breaks = c(seq(100,250,50)), expand = (c(0,0))) +
    labs(x = "DOM (DOWY)", y = "FM")+
    annotate(geom="text", y=.95, x=220, label= title, size = 8, fontface = "bold")+
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
scale1 <-c("grey",viridis(30, option = "C", direction = 1))

# plot
dom_ez1_n_plot <-plot_dom_fm(df = ez1_n_df,
                             bins = 100,
                             scale = scale1,
                             title = "EZ1_N") 
# save
ggsave(dom_ez1_n_plot,
       file = "./plots/fm_dom_ez1_n_plot_v1.png",
       width = 6,
       height = 5,
       dpi = 600)

system("open ./plots/fm_dom_ez1_n_plot_v1.png")

# plot
dom_ez1_s_plot <-plot_dom_fm(df = ez1_s_df,
                         bins = 100,
                         scale = scale1,
                         title = "EZ1_S") 

# plot
dom_ez2_n_plot <-plot_dom_fm(df = ez2_n_df,
                         bins = 100,
                         scale = scale1,
                         title = "EZ2_N") 

dom_ez2_s_plot <-plot_dom_fm(df = ez2_s_df,
                         bins = 100,
                         scale = scale1,
                         title = "EZ2_S") 

# plot
dom_ez3_n_plot <-plot_dom_fm(df = ez3_n_df,
                         bins = 100,
                         scale = scale1,
                         title = "EZ3_N")


dom_ez3_s_plot <-plot_dom_fm(df = ez3_s_df,
                         bins = 100,
                         scale = scale1,
                         title = "EZ3_S") 

# cowplot test
dom_all_six_ez <-plot_grid(dom_ez1_n_plot, dom_ez2_n_plot, dom_ez3_n_plot,
                       dom_ez1_s_plot, dom_ez2_s_plot, dom_ez3_s_plot, 
                       labels = c("(a)", "(b)","(c)","(d)","(e)","(f)"),
                       ncol = 3,
                       nrow = 2,
                       align = "hv",
                       label_size = 22,
                       vjust =  2,
                       hjust = -.2,
                       rel_widths = c(1/3, 1/3, 1/3))
# save
ggsave(dom_all_six_ez,
       file = "./plots/fm_dom_ez6_v2.png",
       width = 18, 
       height = 10,
       dpi = 600)

system("open ./plots/fm_dom_ez6_v2.png")









#################
### dom vs max ###
#################

plot_dom_max <-function(df, bins, scale, title){
  
  plot <-ggplot() +
    geom_tile(data = mean_ez_df, aes(x = max_swe_m, y = dom_dowy), color = 'grey', fill = 'grey', width = 2/100, height = 150/100) +
    geom_bin2d(data = df, bins = bins, aes(x = max_swe_m, y = dom_dowy, fill = ..density..)) +
    scale_fill_gradientn(colors = scale) +
    scale_x_continuous(limits = c(0,2), expand = (c(0,0))) +
    scale_y_continuous(limits = c(100,250),breaks = c(seq(100,250,50)), expand = (c(0,0))) +
    labs(x = "MSWE (M)", y = "DOM (DOWY)")+
    annotate(geom="text", y=.95, x=220, label= title, size = 8, fontface = "bold")+
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
scale1 <-c("grey",viridis(30, option = "F", direction = 1))

# plot
dm_ez1_n_plot <-plot_dom_max(df = ez1_n_df,
                             bins = 100,
                             scale = scale1,
                             title = "EZ1_N") 
# save
ggsave(dm_ez1_n_plot,
       file = "./plots/max_dom_ez1_n_plot_v1.png",
       width = 6,
       height = 5,
       dpi = 600)

system("open ./plots/max_dom_ez1_n_plot_v1.png")

# plot
dm_ez1_s_plot <-plot_dom_max(df = ez1_s_df,
                             bins = 100,
                             scale = scale1,
                             title = "EZ1_S") 

# plot
dm_ez2_n_plot <-plot_dom_max(df = ez2_n_df,
                             bins = 100,
                             scale = scale1,
                             title = "EZ2_N") 

dm_ez2_s_plot <-plot_dom_max(df = ez2_s_df,
                             bins = 100,
                             scale = scale1,
                             title = "EZ2_S") 

# plot
dm_ez3_n_plot <-plot_dom_max(df = ez3_n_df,
                             bins = 100,
                             scale = scale1,
                             title = "EZ3_N")


dm_ez3_s_plot <-plot_dom_max(df = ez3_s_df,
                             bins = 100,
                             scale = scale1,
                             title = "EZ3_S") 

# cowplot test
dm_all_six_ez <-plot_grid(dm_ez1_n_plot, dm_ez2_n_plot, dm_ez3_n_plot,
                           dm_ez1_s_plot, dm_ez2_s_plot, dm_ez3_s_plot, 
                           labels = c("(a)", "(b)","(c)","(d)","(e)","(f)"),
                           ncol = 3,
                           nrow = 2,
                           align = "hv",
                           label_size = 22,
                           vjust =  2,
                           hjust = -.2,
                           rel_widths = c(1/3, 1/3, 1/3))
# save
ggsave(dm_all_six_ez,
       file = "./plots/max_dom_ez6_v1.png",
       width = 18, 
       height = 10,
       dpi = 600)

system("open ./plots/max_dom_ez6_v1.png")
