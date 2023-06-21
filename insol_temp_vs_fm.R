# plot temp vs. fm
# color by insol and aspect
# june 20th, 2023

library(terra)
library(tidyverse)
library(ggplot2)
library(scales)
library(cowplot)
library(viridis)
library(data.table)

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
joined_df <-as.data.table(bind_rows(yuba_df, usj_df, kern_df))

# read back in using data.table
df <-joined_df[sample(.N, 50000)]
head(df)

# rename
df$bin_name <-ifelse(df$ez == 1, "1500-1900 m", df$ez)
df$bin_name <-ifelse(df$ez == 2, "1900-2300 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 3, "2300-2700 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 4, "2700-3100 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 5, "3100-3500 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 6, "3500-4361 m", df$bin_name)
head(df)


##############################
##############################
########## temp ##############
##############################
##############################

# create plotting function
plot_fm_vs_temp <-function(df, scale, title){
  
  plot <-ggplot() +
    geom_point(data = df, aes(x = frac_melt, y = temp_mean_c, fill = insol_watts)) +
    geom_hline(yintercept = 0, linetype = 2, color = 'darkred', alpha = .5)+
    scale_fill_gradientn(colors = scale1) +
    scale_x_continuous(limits = c(0,1), expand = (c(0,0))) +
    scale_y_continuous(limits = c(-7,7),expand = (c(0,0))) +
    labs(y = "Temperature (°C)", x = "FM")+
    annotate(geom="text", y = .7, x = 3500, label= title, size = 8, fontface = "bold")+
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
temp_ez_n_plot <-plot_fm_vs_temp(df = df, scale = scale2, title = "test") 
temp_ez_n_plot

# save
ggsave(temp_ez_n_plot,
       file = "./plots/temp_vs_fm_north_plot_v1.png",
       width = 6,
       height = 5,
       dpi = 600)

system("open ./plots/temp_vs_fm_north_plot_v1.png")

# plot
temp_ez_s_plot <-plot_temp_vs_dem(df = ez_s_df,
                                   bins = 80,
                                   scale = scale2,
                                   title = "South Facing") 

# save
ggsave(temp_ez_s_plot,
       file = "./plots/temp_vs_fm_south_plot_v1.png",
       width = 6,
       height = 5,
       dpi = 600)

system("open ./plots/temp_vs_fm_south_plot_v1.png")

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
       file = "./plots/temp_fm_ns_v1.png",
       width = 11.5, 
       height = 5,
       dpi = 600)

system("open ./plots/temp_fm_ns_v1.png")

