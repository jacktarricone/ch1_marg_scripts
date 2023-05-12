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

# read in df
mean_ez_df <-fread("./csvs/full_plotting_df_v1.csv")
head(mean_ez_df)

# pull out north
ez_n_df <-subset(mean_ez_df, ez %in% c(1,3,5))
ez_s_df <-subset(mean_ez_df, ez %in% c(2,4,6))

##############################
##############################
#########  insol  ############
##############################
##############################


# create plotting function
plot_insol_vs_fm <-function(df, bins, scale, title){
  
  plot <-ggplot() +
    geom_tile(data = mean_ez_df, aes(x = elevation, y = insol_kwh), color = 'grey', fill = 'grey', width = 1800/100, height = 7/100) +
    geom_bin2d(data = df, bins = bins, aes(x = elevation, y= insol_kwh, fill = ..density..)) +
    scale_fill_gradientn(colors = scale) +
    scale_x_continuous(limits = c(1500,4300), expand = (c(0,0))) +
    scale_y_continuous(limits = c(0,7),expand = (c(0,0))) +
    labs(y = expression(Insolation ~ '(kWh m'^{"-2"} ~ ')'), x = "Elevation (m)")+
    annotate(geom="text", y=6.7, x=3600, label= title, size = 8, fontface = "bold")+
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
insol_ez_n_plot <-plot_insol_vs_fm(df = ez_n_df,
                             bins = 100,
                             scale = scale1,
                             title = "North Facing") 
# save
ggsave(insol_ez_n_plot,
       file = "./plots/insol_vs_dem_north_plot_v1.png",
       width = 6,
       height = 5,
       dpi = 600)

system("open ./plots/insol_vs_dem_north_plot_v1.png")

# plot
insol_ez_s_plot <-plot_insol_vs_fm(df = ez_s_df,
                            bins = 80,
                            scale = scale1,
                            title = "South Facing") 

# save
ggsave(insol_ez_s_plot,
       file = "./plots/insol_vs_dem_south_plot_v1.png",
       width = 6,
       height = 5,
       dpi = 600)

system("open ./plots/insol_vs_dem_south_plot_v1.png")

# cowplot test
n_v_s <-plot_grid(insol_ez_n_plot, insol_ez_s_plot,
                  labels = c("(a)", "(b)"),
                  ncol = 2,
                  align = "hv",
                  label_size = 22,
                  vjust =  2.4,
                  hjust = 0,
                  rel_widths = c(1/2, 1/2))
# save
ggsave(n_v_s,
       file = "./plots/insol_dem_ns_v1.png",
       width = 11, 
       height = 5.5,
       dpi = 600)

system("open ./plots/insol_dem_ns_v1.png")

##############################
##############################
########## temp ##############
##############################
##############################


# create plotting function
plot_temp_vs_dem <-function(df, bins, scale, title){
  
  plot <-ggplot() +
    geom_tile(data = mean_ez_df, aes(x = elevation, y = temp_c), color = 'grey', fill = 'grey', width = 1800/100, height = 14/100) +
    geom_bin2d(data = df, bins = bins, aes(x = elevation, y= temp_c, fill = ..density..)) +
    geom_hline(yintercept = 0, linetype = 2, color = 'darkred', alpha = .5)+
    scale_fill_gradientn(colors = scale) +
    scale_x_continuous(limits = c(1500,4300), expand = (c(0,0))) +
    scale_y_continuous(limits = c(-7,7),expand = (c(0,0))) +
    labs(y = "Temperature (°C)", x = "Elevation (m)")+
    annotate(geom="text", y = 6.3, x = 3500, label= title, size = 8, fontface = "bold")+
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
scale2 <-c("grey",viridis(30, option = "B", direction = 1))

# plot
temp_ez_n_plot <-plot_temp_vs_dem(df = ez_n_df,
                                   bins = 100,
                                   scale = scale2,
                                   title = "North Facing") 
# save
ggsave(temp_ez_n_plot,
       file = "./plots/temp_vs_dem_north_plot_v3.png",
       width = 6,
       height = 5,
       dpi = 600)

system("open ./plots/temp_vs_dem_north_plot_v3.png")

# plot
temp_ez_s_plot <-plot_temp_vs_dem(df = ez_s_df,
                                   bins = 80,
                                   scale = scale2,
                                   title = "South Facing") 

# save
ggsave(temp_ez_s_plot,
       file = "./plots/temp_vs_dem_south_plot_v1.png",
       width = 6,
       height = 5,
       dpi = 600)

system("open ./plots/temp_vs_dem_south_plot_v1.png")

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
       file = "./plots/temp_dem_ns_v1.png",
       width = 11.5, 
       height = 5,
       dpi = 600)

system("open ./plots/temp_dem_ns_v1.png")

