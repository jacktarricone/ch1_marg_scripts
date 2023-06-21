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
library(ggpubr)
library(gridExtra)
library(grid)

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

hist(yuba_df$insol_watts, breaks = 100)

#######################################
############ insol ####################
#######################################

# create plotting functions
plot_temp_vs_fm_top <-function(df, scale, title){
  
  plot <-ggplot() +
    geom_point(data = df, aes(y = mean_fm, x= mean_temp_c, color = insol_watts), alpha = .2, size = .5) +
    scale_color_gradientn(colors = scale, name = expression('CS Insolation' ~ '(W m'^{"-2"} ~ ')')) +
    scale_x_continuous(limits = c(-10,10), expand = (c(0,0))) +
    scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
    xlab(NULL) + ylab("FM")+
    annotate(geom="text", x = -6, y = .93, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          aspect.ratio = 1,
          legend.position  = 'top',
          axis.text.x = element_blank(),
          plot.margin = unit(c(0.33, 0.33, 0.33, 0.33), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(color = guide_colorbar(direction = "horizontal",
                                  label.position = 'bottom',
                                  title.position = 'top',
                                  title.hjust = .5,
                                  barwidth = 16, # good
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
    xlab(NULL) + ylab("FM")+
    annotate(geom="text", x = -6, y = .93, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          aspect.ratio = 1,
          legend.position  = 'none',
          axis.text.x = element_blank(),
          plot.margin = unit(c(0.33, 0.33, 0.33, 0.33), "cm"),
          legend.box.spacing = unit(0, "pt")) 
  return(plot)
}
plot_temp_vs_fm_bot <-function(df, scale, title){
  
  plot <-ggplot() +
    geom_point(data = df, aes(y = mean_fm, x= mean_temp_c, color = insol_watts), alpha = .2, size = .5) +
    scale_color_gradientn(colors = scale, name = expression('CS Insolation' ~ '(W m'^{"-2"} ~ ')')) +
    scale_x_continuous(limits = c(-10,10), expand = (c(0,0))) +
    scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
    labs(x = "Mean ONDJFM Temperature (°C)", y = "FM")+
    annotate(geom="text", x = -6, y = .93, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          aspect.ratio = 1,
          legend.position  = 'none',
          plot.margin = unit(c(0.33, 0.33, 0.33, 0.33), "cm"),
          legend.box.spacing = unit(0, "pt")) 
  return(plot)
}

## set color
scale2 <-c(viridis(30, option = "D", direction = 1))

# plot
yuba_insol <-plot_temp_vs_fm_top(df = yuba_df, scale = scale2, title = "(a)  Yuba") 
usj_insol <-plot_temp_vs_fm_mid(df = usj_df, scale = scale2, title = "(b)  USJ") 
kern_insol <-plot_temp_vs_fm_bot(df = kern_df, scale = scale2, title = "(c)  Kern") 

# save
ggsave(yuba_insol,
       file = "./plots/yuba_fm_temp_insol_v1.png",
       width = 4.5, 
       height = 5,
       dpi = 600)

system("open ./plots/yuba_fm_temp_insol_v1.png")

## for shared legend, this is it
insol_cow <-grid.arrange(yuba_insol, usj_insol,kern_insol,
                         nrow = 3,
                         heights = c(1.3, 1, 1.12)) 

# save
ggsave(insol_cow,
       file = "./plots/insol_cow_temp_fm_v3.png",
       width = 4.5, 
       height = 13,
       dpi = 300)

system("open ./plots/insol_cow_temp_fm_v3.png")


########################################
############ aspect ####################
########################################

# create plotting function
plot_temp_vs_fm_top_v2 <-function(df, title){
  
  plot <-ggplot() +
    geom_point(data = df, aes(y = mean_fm, x= mean_temp_c, color = factor(aspect)), alpha = .2, size = .5) +
    scale_color_manual(name = "Aspect",
                       values = c("1" = 'darkblue', "2" = 'darkgreen',
                                  "3" = 'tomato4', "4" = 'yellow'),
                       labels = c('North', 'East', 'South', 'West')) +
    scale_x_continuous(limits = c(-10,10), expand = (c(0,0))) +
    scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
    xlab(NULL) + ylab("FM")+
    annotate(geom="text", x = -6, y = .93, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
          aspect.ratio = 1,
          legend.position = 'top',
          legend.direction = 'horizontal',
          legend.spacing.x = unit(.2, 'cm'),
          axis.text.x = element_blank(),
          plot.margin = unit(c(.25,.25, 0,.25), "cm"))+
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 4, hjust = 0.5, nrow = 1)))
  return(plot)
}
plot_temp_vs_fm_mid_v2 <-function(df, title){
  
  plot <-ggplot() +
    geom_point(data = df, aes(y = mean_fm, x= mean_temp_c, color = factor(aspect)), alpha = .2, size = .5) +
    scale_color_manual(name = "Aspect",
                       values = c("1" = 'darkblue', "2" = 'darkgreen',
                                  "3" = 'tomato4', "4" = 'yellow'),
                       labels = c('North', 'East', 'South', 'West')) +
    scale_x_continuous(limits = c(-10,10), expand = (c(0,0))) +
    scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
    xlab(NULL) + ylab("FM")+
    annotate(geom="text", x = -6, y = .93, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          aspect.ratio = 1,
          legend.position  = 'none',
          axis.text.x = element_blank(),
          plot.margin = unit(c(0.33, 0.33, 0.33, 0.33), "cm"),
          legend.box.spacing = unit(0, "pt")) 
  return(plot)
}
plot_temp_vs_fm_bot_v2 <-function(df, title){
  
  plot <-ggplot() +
    geom_point(data = df, aes(y = mean_fm, x= mean_temp_c, color = factor(aspect)), alpha = .2, size = .5) +
    scale_color_manual(name = "Aspect",
                       values = c("1" = 'darkblue', "2" = 'darkgreen',
                                  "3" = 'tomato4', "4" = 'yellow'),
                       labels = c('North', 'East', 'South', 'West')) +
    scale_x_continuous(limits = c(-10,10), expand = (c(0,0))) +
    scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
    labs(x = "Mean ONDJFM Temperature (°C)", y = "FM")+
    annotate(geom="text", x = -6, y = .93, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          aspect.ratio = 1,
          legend.position  = 'none',
          plot.margin = unit(c(0.33, 0.33, 0.33, 0.33), "cm"),
          legend.box.spacing = unit(0, "pt")) 
  return(plot)
}

# plot
yuba_aspect <-plot_temp_vs_fm_top_v2(df = yuba_df, title = "(a)  Yuba") 
usj_aspect <-plot_temp_vs_fm_mid_v2(df = usj_df, title = "(b)  USJ") 
kern_aspect <-plot_temp_vs_fm_bot_v2(df = kern_df, title = "(c)  Kern") 

# save
ggsave(yuba_aspect,
       file = "./plots/yuba_fm_temp_aspect_v1.png",
       width = 4.5, 
       height = 5,
       dpi = 600)

system("open ./plots/yuba_fm_temp_aspect_v1.png")

## for shared legend, this is it
aspect_cow <-grid.arrange(yuba_aspect, usj_aspect, kern_aspect,
                         nrow = 3,
                         heights = c(1.3, 1, 1.12)) 

# save
ggsave(aspect_cow,
       file = "./plots/aspect_cow_temp_fm_v1.png",
       width = 4.5, 
       height = 13,
       dpi = 300)

system("open ./plots/aspect_cow_temp_fm_v1.png")



