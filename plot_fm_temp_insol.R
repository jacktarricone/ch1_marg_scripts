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
library(dplyr)

setwd("~/ch1_margulis")

# set custom plot theme
theme_classic <- function(base_size = 11, base_family = "",
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

theme_set(theme_classic(16))

# set working dir
setwd("~/ch1_margulis")

##############################################
yuba_df <-fread("./csvs/gridmet_dfs/yuba_full_stats.csv_v2.csv")
usj_df <-fread("./csvs/gridmet_dfs/usj_full_stats.csv_v2.csv")
kern_df <-fread("./csvs/gridmet_dfs/kern_full_stats.csv_v2.csv")

# join
joined_df <-as.data.table(dplyr::bind_rows(yuba_df, usj_df, kern_df))

# # read back in using data.table
# df <-joined_df[sample(.N, 50000)]
# df <-dt <- head(joined_df, 50000)
# head(df)

# remove unneed annual data
df_v1 <-dplyr::select(joined_df, 1:16)
head(df_v1)

# mean df for plotting
mean_df <-unique(df_v1)

# split up by basin
usj_df <-filter(mean_df, basin_name == "usj")
yuba_df <-filter(mean_df, basin_name == "yuba")
kern_df <-filter(mean_df, basin_name == "kern")
head(yuba_df)

hist(yuba_df$mean_temp_c, breaks = 100)

# create plotting function
plot_temp_vs_fm_top <-function(df, scale, title){
  
  plot <-ggplot() +
    geom_point(data = df, aes(y = mean_fm, x= mean_temp_c, color = insol_watts), alpha = .2, size = .5) +
    scale_color_gradientn(colors = scale, name = expression('CS Insolation' ~ '(W m'^{"-2"} ~ ')')) +
    scale_x_continuous(limits = c(-10,10), expand = (c(0,0))) +
    scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
    labs(x = "none", y = "FM")+
    annotate(geom="text", x = -7, y = .93, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          aspect.ratio = 1,
          legend.position  = 'top',
          axis.text.x = element_blank(),
          plot.margin = unit(c(.1,.3,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(color = guide_colorbar(direction = "horizontal",
                                  label.position = 'bottom',
                                  title.position = 'top',
                                  title.hjust = .5,
                                  barwidth = 20,
                                  barheight = 1,
                                  frame.colour = "black", 
                                  ticks.colour = "black"))
  return(plot)
}
plot_temp_vs_fm_mid <-function(df, scale, title){
  
  plot <-ggplot() +
    geom_point(data = df, aes(y = mean_fm, x= mean_temp_c, color = insol_watts), alpha = .2, size = .5) +
    scale_color_gradientn(colors = scale, name = expression('CS Insolation' ~ '(W m'^{"-2"} ~ ')')) +
    scale_x_continuous(limits = c(-10,10), expand = (c(0,0))) +
    scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
    labs(x = "Mean ONDJFM Temperature (°C)", y = "FM")+
    annotate(geom="text", x = -7, y = .93, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          aspect.ratio = 1,
          legend.position  = 'none',
          plot.margin = unit(c(.1,.3,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) 
    # guides(color = guide_colorbar(direction = "horizontal",
    #                               label.position = 'bottom',
    #                               title.position = 'top',
    #                               title.hjust = .5,
    #                               barwidth = 20,
    #                               barheight = 1,
    #                               frame.colour = "black", 
    #                               ticks.colour = "black"))
  return(plot)
}
plot_temp_vs_fm_bot <-function(df, scale, title){
  
  plot <-ggplot() +
    geom_point(data = df, aes(y = mean_fm, x= mean_temp_c, color = insol_watts), alpha = .2, size = .5) +
    scale_color_gradientn(colors = scale, name = expression('CS Insolation' ~ '(W m'^{"-2"} ~ ')')) +
    scale_x_continuous(limits = c(-10,10), expand = (c(0,0))) +
    scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
    labs(x = "Mean ONDJFM Temperature (°C)", y = "FM")+
    annotate(geom="text", x = -7, y = .93, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          aspect.ratio = 1,
          legend.position  = 'none',
          plot.margin = unit(c(.1,.3,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) 
    # guides(color = guide_colorbar(direction = "horizontal",
    #                               label.position = 'bottom',
    #                               title.position = 'top',
    #                               title.hjust = .5,
    #                               barwidth = 20,
    #                               barheight = 1,
    #                               frame.colour = "black", 
    #                               ticks.colour = "black"))
  return(plot)
}

## set color
scale2 <-c(viridis(30, option = "D", direction = 1))

# plot
yuba_insol <-plot_temp_vs_fm_top(df = yuba_df, scale = scale2, title = "Yuba") 
usj_insol <-plot_temp_vs_fm_mid(df = usj_df, scale = scale2, title = "USJ") 
kern_insol <-plot_temp_vs_fm_bot(df = kern_df, scale = scale2, title = "Kern") 

# save
ggsave(yuba_insol,
       file = "./plots/yuba_fm_temp_insol_v1.png",
       width = 5, 
       height = 5,
       dpi = 600)

system("open ./plots/kern_fm_temp_insol_v1.png")


insol_cow <-plot_grid(yuba_insol, usj_insol, kern_insol,
                       labels = c("(a)", "(b)", "(c)"),
                       nrow = 3,
                       align = "v",
                       label_size = 22,
                       vjust =  2.4,
                       hjust = 0,
                       rel_heights = c(, 1/2))

# create plotting function
plot_dem_fm_temp <-function(df, scale, title){
  
  plot <-ggplot() +
    geom_point(data = plot_df, aes(y = frac_melt, x= temp_deg_c, color = elevation), alpha = .2, size = .5) +
    scale_color_gradientn(colors = scale, name = "Elevation (m)") +
    scale_x_continuous(limits = c(-6,8), expand = (c(0,0))) +
    scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
    labs(x = "Mean ONDJFM Temperature (°C)", y = "FM")+
    annotate(geom="text", x = -3, y = .93, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          aspect.ratio = 1,
          legend.position  = 'top',
          plot.margin = unit(c(.25,.1,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(color = guide_colorbar(direction = "horizontal",
                                  label.position = 'bottom',
                                  title.position = 'top',
                                  title.hjust = .5,
                                  barwidth = 20,
                                  barheight = 1,
                                  frame.colour = "black", 
                                  ticks.colour = "black"))
  return(plot)
}

## set color
scale2 <-c(viridis(30, option = "A", direction = 1))

# plot
usj_temp_fm_plot_v2 <-plot_dem_fm_temp(df = plot_df,
                                   scale = scale2,
                                   title = "USJ") 

# save
ggsave(usj_temp_fm_plot_v2,
       file = "./plots/usj_temp_fm_dem_v2.png",
       width = 5, 
       height = 5.8,
       dpi = 600)

system("open ./plots/usj_temp_fm_dem_v2.png")

dm <-rast("./rasters/daymet/tmean_normal_1985_2016.tif")
hist(dm, breaks = 100, main = "Daymet 1985-2015 ONDJFM Mean Temp")

prism <-rast("./rasters/prism/prism_tmean_snsr_ondjfm.tif")
hist(prism, breaks = 100, main = "PRISM 1990-2019 ONDJFM Mean Temp")
