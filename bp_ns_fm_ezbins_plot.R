# updated snow metric box plots
# split by hydro cat means


library(terra)
library(lubridate)
library(tidyverse)
library(cowplot)
library(data.table)
library(ggsignif)


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

#### read in hydro_cat df
hydro_cat <-fread("./csvs/hydro_cat_years.csv")

##############################################
yuba_df <-fread("./csvs/gridmet_dfs/yuba_full_stats_v4.csv")
usj_df <-fread("./csvs/gridmet_dfs/usj_full_stats_v4.csv")
kern_df <-fread("./csvs/gridmet_dfs/kern_full_stats_v4.csv")
head(kern_df)

# join
joined_df_v1 <-as.data.table(bind_rows(yuba_df, usj_df, kern_df))
joined_df <-dplyr::filter(joined_df_v1, aspect != 2 & aspect != 4)

# read back in using data.table
df <-joined_df

# rename
df$bin_name <-ifelse(df$ez == 1, "1500-1900 m", df$ez)
df$bin_name <-ifelse(df$ez == 2, "1900-2300 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 3, "2300-2700 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 4, "2700-3100 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 5, "3100-4361 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 6, "3100-4361 m", df$bin_name)


# add grouped col
df$aspect_basin <-paste0(df$aspect,".",df$basin_name)
df$aspect_basin <-factor(df$aspect_basin, levels=c('1.kern', '3.kern', 
                                                   '1.usj', '3.usj',
                                                   '1.yuba','3.yuba'))

# test hists
hist(df$frac_melt, breaks = 50)
hist(df$mwa_mm, breaks = 50)

##################################
#### make time series box plot ###
##################################


# define 3 plotting functions
grouped_box_plot_top <-function(variable, min,max, ylab){
  
  p <-ggplot(df, mapping = aes(x = as.factor(bin_name), y = variable, fill = aspect_basin)) +
    geom_boxplot(linewidth = .3, width = .7, position = 'dodge', notch = T,
                 outlier.shape = 4, outlier.color = 'gray90', outlier.alpha = .05, outlier.size = .01) +
    scale_fill_manual(name = "Basin and Aspect",
                      values = c('1.kern' = 'azure4', '3.kern' = 'azure2', 
                                 '1.usj' = 'tomato4', '3.usj' = 'tomato1',
                                 '1.yuba' = 'chartreuse4', '3.yuba' = 'lightgreen'),
                      labels = c('Kern N','Kern S',  
                                 'USJ N', 'USJ S',
                                 'Yuba N','Yuba S')) +
    guides(fill = guide_legend(ncol = 6, override.aes = list(order = c(1,2,3,4,5,6)))) +
    xlab(NULL) + ylab(ylab) +
    scale_y_continuous(limits = c(min,max)) +
    theme_classic(11) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
          legend.position = 'top',
          legend.direction = 'horizontal',
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
          axis.text.x = element_blank(),
          # legend.background = element_rect(colour = "black", fill = 'white', size = .2),
          plot.margin = unit(c(.25,.25, 0,.25), "cm")) +
    geom_signif(
      y_position = c(240, 240), xmin = c(0.8, 1.8), xmax = c(1.2, 2.2),
      annotation = c("**", "NS"), tip_length = 0
    )
  return(p)
}

grouped_box_plot_mid <-function(variable, min,max, ylab){
  
  p <-ggplot(df, mapping = aes(x = as.factor(bin_name), y = variable, fill = interaction(aspect, as.factor(basin_name)))) +
    geom_boxplot(linewidth = .3, width = .7, 
                 outlier.shape = 4, outlier.color = 'gray90', outlier.alpha = .05, outlier.size = .01,
                 position = 'dodge', notch = T) +
    scale_fill_manual(name = "Basin and Aspect",
                      values = c('1.kern' = 'azure4', '3.kern' = 'azure2', 
                                 '1.usj' = 'tomato4', '3.usj' = 'tomato1',
                                 '1.yuba' = 'chartreuse4', '3.yuba' = 'lightgreen'),
                      labels = c('Kern N','Kern S',  
                                 'USJ N', 'USJ S',
                                 'Yuba N','Yuba S')) +
    guides(fill = guide_legend(ncol = 2, override.aes = list(order = c(1,2,3,4,5,6)))) +
    xlab(NULL) + ylab(ylab) +
    scale_y_continuous(limits = c(min,max)) +
    theme_classic(11) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
          legend.position = 'none',
          axis.text.x = element_blank(),
          legend.background = element_rect(colour = "black", fill = 'white', size = .2),
          plot.margin = unit(c(.25,0, 0,.25), "cm"))
  
  return(p)
}
grouped_box_plot_bot <-function(variable, min,max, ylab){
  
  p <-ggplot(df, mapping = aes(x = as.factor(bin_name), y = variable, fill = interaction(aspect, as.factor(basin_name)))) +
    geom_boxplot(linewidth = .3, width = .7, 
                 outlier.shape = 4, outlier.color = 'gray90', outlier.alpha = .05, outlier.size = .01,
                 position = 'dodge', notch = T) +
    scale_fill_manual(name = "Basin and Aspect",
                      values = c('1.kern' = 'azure4', '3.kern' = 'azure2', 
                                 '1.usj' = 'tomato4', '3.usj' = 'tomato1',
                                 '1.yuba' = 'chartreuse4', '3.yuba' = 'lightgreen'),
                      labels = c('Kern N','Kern S',  
                                 'USJ N', 'USJ S',
                                 'Yuba N','Yuba S')) +
    guides(fill = guide_legend(ncol = 2, override.aes = list(order = c(1,2,3,4,5,6)))) +
    xlab("Elevation Zone") + ylab(ylab) +
    scale_y_continuous(limits = c(min,max)) +
    theme_classic(11) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
          legend.position = 'none',
          # axis.text.x = element_blank(),
          legend.background = element_rect(colour = "black", fill = 'white', size = .2),
          plot.margin = unit(c(.25,0, 0,.25), "cm"))
  
  return(p)
}





####################################
######## plot 4 snow variables #####
####################################

max_p <-grouped_box_plot_top(variable = df$mswe_mm/10, 
                             min = 0, max = 250, ylab = "Max SWE (cm)")

max_p

mwa_p <-grouped_box_plot_mid(variable = df$mwa_mm/10, 
                             min = 0, max = 50, ylab = "MWA (cm)")

fm_p <-grouped_box_plot_mid(variable = df$frac_melt, min = 0, max = 1, ylab = "FM")

dom_p <-grouped_box_plot_bot(variable = df$dom_dowy, 
                             min = 50, max = 250, ylab = "DOM (DOWY)")


# cowplot
snow_cow <-plot_grid(max_p, mwa_p, fm_p, dom_p, 
                     labels = c("(a)", "(b)", "(c)","(d)"),
                     nrow = 4, 
                     align = "v",
                     axis = "b",
                     label_size = 14,
                     vjust =  2.4,
                     hjust = -2.6,
                     rel_heights = c(.28,.22,.22,.28))

# plot(snow_cow)

ggsave(snow_cow,
       file = "./plots/snow4_boxplot_v6.png",
       width = 9, 
       height = 7,
       units = "in",
       dpi = 300) 

system("open ./plots/snow4_boxplot_v6.png")

####################################
######## plot 4 met  variables #####
####################################
temp_p <-grouped_box_plot_top(variable = df$temp_mean_c, 
                              min = -7, max = 10, ylab = expression('ONDJFM ' ~T["Mean"] ~ '(°C)'))

rh_p <-grouped_box_plot_mid(variable = df$`rh_mean_%`, 
                            min = 35, max = 70, ylab = expression("RH (%)"))

srad_p <-grouped_box_plot_mid(variable = df$srad_wm2, 
                              min = 120, max = 170, ylab = expression("Insolation"~paste("(W m"^{-2},")")))

insol_p <-grouped_box_plot_bot(variable = df$insol_watts, 
                               min = 30, max = 270, ylab = expression("CS Insolation"~paste("(W m"^{-2},")")))

# cowplot test
met_cow <-plot_grid(temp_p, rh_p, srad_p, insol_p,
                    labels = c("(a)", "(b)", "(c)","(d)"),
                    nrow = 4, 
                    align = "v",
                    axis = "b",
                    label_size = 14,
                    vjust =  2.3,
                    hjust = -2.8,
                    rel_heights = c(.28,.22,.22,.28))

# plot(met_cow)

ggsave(met_cow,
       file = "./plots/met4_boxplot_v5.png",
       width = 9, 
       height = 7,
       units = "in",
       dpi = 300) 

system("open ./plots/met4_boxplot_v5.png")







