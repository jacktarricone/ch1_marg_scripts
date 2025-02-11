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
df <-fread("./csvs/hydro_cat/full_df_hydro_cat_v3.csv")
head(df)

# rename
df$bin_name <-ifelse(df$ez == 1, "1500-1900 m", df$ez)
df$bin_name <-ifelse(df$ez == 2, "1900-2300 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 3, "2300-2700 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 4, "2700-3100 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 5, "3100-4361 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 6, "3100-4361 m", df$bin_name)

# names
df$basin_name <-ifelse(df$basin_name == "kern", "Kern", df$basin_name)
df$basin_name <-ifelse(df$basin_name == "usj", "USJ", df$basin_name)
df$basin_name <-ifelse(df$basin_name == "yuba", "Yuba", df$basin_name)

# add grouped col
df$aspect_basin <-paste0(df$aspect,".",df$basin_name)
head(df)

df$aspect_basin <-factor(df$aspect_basin, levels=c('1.Kern', '3.Kern', 
                                                   '1.USJ', '3.USJ',
                                                   '1.Yuba','3.Yuba'))
#
cw_df <-filter(df, hydr0_cat == "CW")
hd_df <-filter(df, hydr0_cat == "HD")
hw_df <-filter(df, hydr0_cat == "HW")
cd_df <-filter(df, hydr0_cat == "CD")
head(cw_df)

# test hists
hist(cw_df$mean_mswe_mm, breaks = 50)
hist(hd_df$mean_mswe_mm, breaks = 50, col = 'red', add = T)
hist(df$mean_wa, breaks = 50)

##################################
#### make time series box plot ###
##################################

head(cw_df)


# define 3 plotting functions
box_plot <-function(df, display_name){

  grouped_box_plot_top <-function(df,variable, min,max, ylab){
  
  p <-ggplot(df, mapping = aes(x = as.factor(bin_name), y = variable, fill = aspect_basin)) +
    geom_boxplot(linewidth = .3, width = .7, position = 'dodge',
                 outlier.shape = 4, outlier.color = 'gray90', outlier.alpha = .05, outlier.size = .01) +
    scale_fill_manual(name = "Basin and Aspect",
                      values = c('1.Kern' = 'azure4', '3.Kern' = 'azure2', 
                                 '1.USJ' = 'tomato4', '3.USJ' = 'tomato1',
                                 '1.Yuba' = 'chartreuse4', '3.Yuba' = 'lightgreen'),
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
          plot.margin = unit(c(.25,.25, 0,.25), "cm")) +
  annotate("text", x = 1, y = 180, label = display_name, color = "black", size = 7, fontface = 2)
  return(p)
}
  grouped_box_plot_mid <-function(df,variable, min,max, ylab){
  
  p <-ggplot(df, mapping = aes(x = as.factor(bin_name), y = variable, fill = interaction(aspect, as.factor(basin_name)))) +
    geom_boxplot(linewidth = .3, width = .7, 
                 outlier.shape = 4, outlier.color = 'gray90', outlier.alpha = .05, outlier.size = .01,
                 position = 'dodge') +
    scale_fill_manual(name = "Basin and Aspect",
                      values = c('1.Kern' = 'azure4', '3.Kern' = 'azure2', 
                                 '1.USJ' = 'tomato4', '3.USJ' = 'tomato1',
                                 '1.Yuba' = 'chartreuse4', '3.Yuba' = 'lightgreen'),
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
  grouped_box_plot_bot <-function(df,variable, min,max, ylab){
  
  p <-ggplot(df, mapping = aes(x = as.factor(bin_name), y = variable, fill = interaction(aspect, as.factor(basin_name)))) +
    geom_boxplot(linewidth = .3, width = .7, 
                 outlier.shape = 4, outlier.color = 'gray90', outlier.alpha = .05, outlier.size = .01,
                 position = 'dodge') +
    scale_fill_manual(name = "Basin and Aspect",
                      values = c('1.Kern' = 'azure4', '3.Kern' = 'azure2', 
                                 '1.USJ' = 'tomato4', '3.USJ' = 'tomato1',
                                 '1.Yuba' = 'chartreuse4', '3.Yuba' = 'lightgreen'),
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

  max_p <-grouped_box_plot_top(df, variable = df$mean_mswe_mm/10, 
                             min = 0, max = 200, ylab = "Max SWE (cm)")
  
  wa_p <-grouped_box_plot_mid(df, variable = df$mean_wa/10, 
                             min = 0, max = 30, ylab = "WA (cm)")

  fwa_p <-grouped_box_plot_mid(df, variable = df$mean_fwa, min = 0, max = 1, ylab = "FWA (-)")

  dom_p <-grouped_box_plot_bot(df, variable = df$mean_dom_dowy, 
                             min = 100, max = 230, ylab = "DOM (DOWY)")

  # cowplot
  snow_cow <-plot_grid(max_p, wa_p, fwa_p, dom_p, 
                     labels = c("(a)", "(b)", "(c)","(d)"),
                     nrow = 4, 
                     align = "v",
                     axis = "b",
                     label_size = 14,
                     vjust =  2.4,
                     hjust = -2.6,
                     rel_heights = c(.28,.22,.22,.28))
  plot(snow_cow)
  return(snow_cow)
}

snow_cow_cw <-box_plot(df = cw_df, display_name = "Cold/Wet")
snow_cow_hd <-box_plot(df = hd_df, display_name = "Hot/Dry")

snow_cow <-plot_grid(snow_cow_cw, snow_cow_hd,
                     ncol = 2, 
                     rel_heights = c(.5,.5))

ggsave(snow_cow_cw,
       file = "./plots/hydro_cat_cw_v2.png",
       width = 9, 
       height = 7,
       units = "in",
       dpi = 300) 

system("open ./plots/hydro_cat_cw_v2.png")

ggsave(snow_cow_hd,
       file = "./plots/hydro_cat_hd_v2.png",
       width = 9, 
       height = 7,
       units = "in",
       dpi = 300) 

system("open ./plots/hydro_cat_hd_v2.png")

ggsave(snow_cow,
       file = "./plots/hydro_cat_hdcw_v2.png",
       width = 15, 
       height = 7,
       units = "in",
       dpi = 300) 

system("open ./plots/hydro_cat_hdcw_v2.png")




