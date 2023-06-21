# fm box plot time series
# jack tarricone
# june 8th, 2023

library(terra)
library(lubridate)
library(tidyverse)
library(cowplot)
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

theme_set(theme_classic(14))

# set working dir
setwd("~/ch1_margulis")

# # read in stack american stack
# fm_list <-list.files("./rasters/snow_metrics/fm_apr1", full.names = TRUE)
# fm_stack <-rast(fm_list[33])
# fm_stack
# 
# # read in stack american stack
# max_list <-list.files("./rasters/snow_metrics/max_swe", full.names = TRUE)
# max_stack <-rast(max_list[2])
# max_stack
# 
# # bring in ns aspect
# dem_6b <-rast("./rasters/categorized/dem_6zb.tif")
# plot(dem_6b)
# 
# # bring in ns aspect
# aspect_v1 <-rast("./rasters/categorized/aspect_thres_4_classes.tif")
# aspect_ns <-subst(aspect_v1,c(2,4),NA)
# plot(aspect_ns)
# 
# # stack with dem bins and aspect
# fm_stack <-c(dem_6b,aspect_ns,fm_stack)
# max_stack <-c(dem_6b,aspect_ns,max_stack)
# 
# # convert to df
# fm_df <-as.data.frame(fm_stack, xy = TRUE, cells = TRUE, na.rm = TRUE)
# max_df <-as.data.frame(max_stack, xy = TRUE, cells = TRUE, na.rm = TRUE)
# 
# # years seq
# years <-seq(1985,2016,1)
# 
# # rename columns, which are the annual rasters, with the correct year name
# colnames(max_df)[6:ncol(max_df)] <-years
# colnames(max_df)[3:5] <-c("y","ez","aspect")
# 
# colnames(fm_df)[6:ncol(fm_df)] <-years
# colnames(fm_df)[3:5] <-c("y","ez","aspect")
# 
# # pivot longer for test
# # creates "year" col and "fm_percent" col while preserving meta data info
# fm_long_df <-as.data.frame((fm_df) %>%
#  pivot_longer(-c(cell,x,y,ez,aspect), names_to = "year", values_to = "frac_melt"))
# 
# head(fm_long_df)
# 
# max_long_df <-as.data.frame((max_df) %>%
#                              pivot_longer(-c(cell,x,y,ez,aspect), names_to = "year", values_to = "max_swe"))
# head(max_long_df)
# 
# fwrite(fm_long_df, "./csvs/fm_boxplot_df_v2.csv")
# fwrite(max_long_df, "./csvs/max_boxplot_df_v2.csv")

##############################################
yuba_df <-fread("./csvs/gridmet_dfs/yuba_full_stats.csv_v2.csv")
usj_df <-fread("./csvs/gridmet_dfs/usj_full_stats.csv_v2.csv")
kern_df <-fread("./csvs/gridmet_dfs/kern_full_stats.csv_v2.csv")

# join
joined_df_v1 <-as.data.table(bind_rows(yuba_df, usj_df, kern_df))
joined_df <-dplyr::filter(joined_df_v1, aspect != 2 & aspect != 4)

# read back in using data.table
df <-joined_df[sample(.N, 500000)]
head(df)

# rename
df$bin_name <-ifelse(df$ez == 1, "1500-1900 m", df$ez)
df$bin_name <-ifelse(df$ez == 2, "1900-2300 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 3, "2300-2700 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 4, "2700-3100 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 5, "3100-3500 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 6, "3500-4361 m", df$bin_name)

head(df)

# test hists
hist(joined_df$frac_melt, breaks = 50)
hist(df$frac_melt, breaks = 50)
mean(joined_df$frac_melt, na.rm = TRUE)
mean(df$frac_melt, na.rm = TRUE)

##################################
#### make time series box plot ###
##################################

# define 3 plotting functions
# top
ggplot(df, mapping = aes(x = as.factor(bin_name), y = mswe_mm/10, fill = interaction(aspect, as.factor(basin_name)))) +
  geom_boxplot(linewidth = .3, width = .4, outlier.size = .01, outlier.shape = 1, position = 'dodge') +
  scale_fill_manual(name = "Aspect and Basin Name",
                    values = c('1.kern' = 'azure4', '3.kern' = 'azure2', 
                               '1.usj' = 'tomato4', '3.usj' = 'tomato1',
                               '1.yuba' = 'chartreuse4', '3.yuba' = 'lightgreen'),
                    labels = c('Kern N','Kern S',  
                               'USJ N', 'USJ S',
                               'Yuba N','Yuba S')) +
  guides(fill = guide_legend(ncol = 6, override.aes = list(order = c(1,2,3,4,5,6)))) +
  xlab('none') + ylab(ylab) +
  scale_y_continuous(limits = c(0,300)) +
  theme_classic(11) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        axis.text.x = element_blank(),
        # legend.background = element_rect(colour = "black", fill = 'white', size = .2),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))


grouped_box_plot_top <-function(variable, min,max, ylab){
  
p <-ggplot(df, mapping = aes(x = as.factor(bin_name), y = variable, fill = interaction(aspect, as.factor(basin_name)))) +
  geom_boxplot(linewidth = .3, width = .4, outlier.size = .01, outlier.shape = 1, position = 'dodge') +
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
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))
  
  return(p)
}

grouped_box_plot_mid <-function(variable, min,max, ylab){
  
  p <-ggplot(df, mapping = aes(x = as.factor(bin_name), y = variable, fill = interaction(aspect, as.factor(basin_name)))) +
    geom_boxplot(linewidth = .3, width = .4, outlier.size = .01, outlier.shape = 1, position = 'dodge') +
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
    geom_boxplot(linewidth = .3, width = .4, outlier.size = .01, outlier.shape = 1, position = 'dodge') +
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


# plot 3 snow variables 
max_p <-grouped_box_plot_top(variable = df$mswe_mm/10, 
                             min = 0, max = 300, ylab = "Max SWE (cm)")

dom_p <-grouped_box_plot_mid(variable = df$dom_dowy, 
                             min = 50, max = 250, ylab = "DOM (DOWY)")

fm_p <-grouped_box_plot_bot(variable = df$frac_melt, min = 0, max = 1, ylab = "FM")

max_p

# cowplot test
snow_cow <-plot_grid(max_p, dom_p, fm_p,
                labels = c("(a)", "(b)", "(c)"),
                nrow = 3, 
                align = "v",
                axis = "b",
                label_size = 14,
                vjust =  2.2,
                hjust = -2.8,
                rel_heights = c(.4, .35,.35))

plot(snow_cow)

ggsave(snow_cow,
       file = "./plots/snow3_boxplot_v1.png",
       width = 9, 
       height = 7,
       units = "in",
       dpi = 300) 

system("open ./plots/snow3_boxplot_v1.png")

# met variables
temp_p <-grouped_box_plot(variable = df$temp_mean_c, min = -7, max = 10, ylab = "Temp Mean (C)",
                                  lx = .85, ly = .85)

ah_p <-grouped_box_plot(variable = df$abs_hum_gcm3, min = 1, max = 5, ylab = "abs_hum",
                          lx = .85, ly = .85)
ah_p

insol_p <-grouped_box_plot(variable = df$insol_watts, min = 0, max = 270, ylab = "insol",
                        lx = .85, ly = .85)
insol_p 

srad_p <-grouped_box_plot(variable = df$srad_wm2, min = 120, max = 165, ylab = "srad",
                           lx = .85, ly = .25)
srad_p 

# test save
ggsave(fm,
       file = "./plots/fm_ez_ns_boxplot_test_v4.png",
       width = 7, 
       height = 3,
       units = "in",
       dpi = 300) 

system("open ./plots/fm_ez_ns_boxplot_test_v4.png")


# test save
ggsave(max_p,
       file = "./plots/basins_max_boxplot_v1.png",
       width = 9, 
       height = 4,
       units = "in",
       dpi = 300) 

system("open ./plots/basins_max_boxplot_v1.png")

# cowplot test
cow <-plot_grid(fm, max_p,
                labels = c("(a)", "(b)"),
                nrow = 2, 
                align = "v",
                axis = "b",
                label_size = 14,
                vjust =  2.2,
                hjust = -2.8,
                rel_heights = c(.47, .53))

plot(cow)

ggsave(cow,
       file = "./plots/fm_max_boxplot_v2.png",
       width = 6, 
       height = 5,
       units = "in",
       dpi = 300) 

system("open ./plots/fm_max_boxplot_v2.png")




