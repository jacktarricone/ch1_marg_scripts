####### start mk results analysis table
# jack tarricone
# april 19th, 2023

library(terra)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)
library(cowplot)
library(viridis)

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

setwd("~/ch1_margulis")

#####################################
# head_df for col referencing
##############################################
# df <-fread("./csvs/hydro_cat/full_df_hydro_cat_v1.csv")
# df$basin_name <-ifelse(df$basin_name == "kern","Kern",df$basin_name)
# df$basin_name <-ifelse(df$basin_name == "usj","USJ",df$basin_name)
# df$basin_name <-ifelse(df$basin_name == "yuba","Yuba",df$basin_name)
# df$hydr0_cat <-ifelse(df$hydr0_cat == "cw","CW",df$hydr0_cat)
# df$hydr0_cat <-ifelse(df$hydr0_cat == "hd","HD",df$hydr0_cat)
# 
# # creat new zones of 200 m
# df$ez2 <-NA
# df$ez2 <-ifelse(df$elevation > 1500 & df$elevation <= 1700, 1, df$ez2)
# df$ez2 <-ifelse(df$elevation > 1700 & df$elevation <= 1900, 2, df$ez2)
# df$ez2 <-ifelse(df$elevation > 1900 & df$elevation <= 2100, 3, df$ez2)
# df$ez2 <-ifelse(df$elevation > 2100 & df$elevation <= 2300, 4, df$ez2)
# df$ez2 <-ifelse(df$elevation > 2300 & df$elevation <= 2500, 5, df$ez2)
# df$ez2 <-ifelse(df$elevation > 2500 & df$elevation <= 2700, 6, df$ez2)
# df$ez2 <-ifelse(df$elevation > 2700 & df$elevation <= 2900, 7, df$ez2)
# df$ez2 <-ifelse(df$elevation > 2900 & df$elevation <= 3100, 8, df$ez2)
# df$ez2 <-ifelse(df$elevation > 3100 & df$elevation <= 3300, 9, df$ez2)
# df$ez2 <-ifelse(df$elevation > 3300 & df$elevation <= 3500, 10, df$ez2)
# df$ez2 <-ifelse(df$elevation > 3500 & df$elevation <= 3700, 11, df$ez2)
# df$ez2 <-ifelse(df$elevation > 3700 & df$elevation <= 3900, 12, df$ez2)
# df$ez2 <-ifelse(df$elevation > 3900 & df$elevation <= 4100, 13, df$ez2)
# df$ez2 <-ifelse(df$elevation > 4100 & df$elevation <= 4400, 14, df$ez2)
# 
# # rename
# # df$bin_name <-ifelse(df$ez == 1, "1500-1900 m", df$ez)
# # df$bin_name <-ifelse(df$ez == 2, "1900-2300 m", df$bin_name)
# # df$bin_name <-ifelse(df$ez == 3, "2300-2700 m", df$bin_name)
# # df$bin_name <-ifelse(df$ez == 4, "2700-3100 m", df$bin_name)
# # df$bin_name <-ifelse(df$ez == 5, "3100-4361 m", df$bin_name)
# # df$bin_name <-ifelse(df$ez == 6, "3100-4361 m", df$bin_name)
# 
# # filter aspects
# df <-dplyr::filter(df, aspect != 2 & aspect != 4)
# df <-dplyr::filter(df, hydr0_cat != "hw" & hydr0_cat != "cd")
# 
# # check
# head(df)
# unique(df$hydr0_cat)
# 
# # filter
# df$aspect_name <-ifelse(df$aspect ==  1, "North", df$aspect)
# df$aspect_name <-ifelse(df$aspect ==  3, "South", df$aspect_name)
# head(df)
# 
# fwrite(df, "./full_df_hydro_cat_v2.csv")

df <-fread("./full_df_hydro_cat_v2.csv")
head(df)

######################################
######################################
############ snow data ###############
######################################
######################################

snow_results <-df %>%
  group_by(basin_name, ez2, hydr0_cat, aspect_name) %>%
  summarise(mean_ez_mswe = as.integer(mean(mean_mswe_mm)),
            mean_ez_dom = as.integer(mean(mean_dom_dowy)),
            mean_ez_fm = round(mean(mean_fm),2),
            mean_ez_mwa = as.integer(mean(mean_mwa)),
            mean_ez_tmean = round(mean(mean_tmean),2))

# Reshape the data into separate columns for aspect_name
snow_results_wide <- snow_results  %>%
  tidyr::pivot_wider(names_from = aspect_name,
              values_from = c(mean_ez_mswe, mean_ez_dom, mean_ez_fm, mean_ez_mwa, mean_ez_tmean))


# calc diff
df_diff <- snow_results_wide %>%
  mutate(
    mean_ez_tmean = (mean_ez_tmean_North + mean_ez_tmean_South)/2,
    diff_ez_mswe = mean_ez_mswe_South - mean_ez_mswe_North,
    diff_ez_dom = mean_ez_dom_South - mean_ez_dom_North,
    diff_ez_fm = mean_ez_fm_South - mean_ez_fm_North,
    diff_ez_mwa = mean_ez_mwa_South - mean_ez_mwa_North
  )

# Sort the dataframe
df_sorted <- df_diff %>%
  select(basin_name, ez2, mean_ez_tmean,matches("mswe"), matches("dom"), matches("fm"), matches("mwa"))

usj_cw <-filter(df_sorted, basin_name == "USJ" & hydr0_cat == "CW")
usj_hd <-filter(df_sorted, basin_name == "USJ" & hydr0_cat == "HD")
mean(usj_hd$diff_ez_fm)
mean(usj_cw$diff_ez_fm)
max(usj_hd$diff_ez_fm)

usj_cw7 <-filter(df_sorted, basin_name == "USJ" & hydr0_cat == "CW" & ez2 >= 7)
usj_hd7 <-filter(df_sorted, basin_name == "USJ" & hydr0_cat == "HD" & ez2 >= 7)
mean(usj_hd7$diff_ez_fm)
mean(usj_cw7$diff_ez_fm)


# View the sorted dataframe
head(df_sorted)
usj <-as.data.frame(filter(snow_results, basin_name == "USJ"))
kern <-as.data.frame(filter(snow_results, basin_name == "Kern"))
yuba <-as.data.frame(filter(snow_results, basin_name == "Yuba"))

### good plot but too busy
# ggplot(snow_results, aes(x=ez2,y=mean_ez_fm,linetype=hydr0_cat,color=as.factor(aspect_name), shape=as.factor(basin_name))) +
#   geom_line(size = .7)+
#   geom_point(size = 5)+
#   ylab("FM (-)") + xlab("Elevation Zone")+
#   scale_x_continuous(limits = c(1,14), breaks = seq(1,13,2))+
#   scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))+
#   scale_linetype_manual(name = "Hydro Cat", values = c("HD" = "longdash", "CW" = "dotted")) +
#   scale_shape_manual(name = "Basin", values = c("Kern" = 8, "USJ" = 17, "Yuba" = 15)) +
#   scale_color_manual(name = "Aspect",
#                      values = c('North' = 'darkblue', 'South' = 'tomato'),
#                      labels = c('NF','SF')) +
#   theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
#         legend.position = 'top',
#         legend.direction = 'horizontal',
#         axis.text.x = element_blank(),
#         axis.title.x = element_blank(),
#         legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
#         plot.margin = unit(c(.25,.25, 0,.25), "cm"))

  # annotate("text", x = 11.5, y = 1*.95,  
  #          label = "(a) Yuba", color = "Black", size = 6, hjust = 0)+
  # annotate("text", x = 8, y = 1*.93,  
  #          label = name, color = "Black", size = 8, hjust = 0)
######
# by elevations
######

big_plot <-function(variable, ylab,ylim1,ylim2,by,name){

  yuba_p <-ggplot(yuba, aes(x=ez2,y=get(variable),linetype=hydr0_cat,color=as.factor(aspect_name))) +
    geom_line(size = .7)+
    ylab(ylab) + xlab("Elevation Zone")+
    scale_x_continuous(limits = c(1,14), breaks = seq(1,13,2))+
    scale_y_continuous(limits = c(ylim1,ylim2), breaks = seq(ylim1,ylim2,by))+
    scale_linetype_manual(name = "Hydro Cat", values = c("HD" = "longdash", "CW" = "dotted")) +
    scale_color_manual(name = "Aspect",
                      values = c('North' = 'darkblue', 'South' = 'tomato'),
                       labels = c('NF','SF')) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
          legend.position = 'top',
          legend.direction = 'horizontal',
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
          plot.margin = unit(c(.25,.25, 0,.25), "cm")) +
    annotate("text", x = 11.5, y = ylim2*.95,  
            label = "(a) Yuba", color = "Black", size = 6, hjust = 0)+
    annotate("text", x = 8, y = ylim2*.93,  
            label = name, color = "Black", size = 8, hjust = 0)

usj_p <-ggplot(usj, aes(x=ez2,y=get(variable),linetype=hydr0_cat,color=as.factor(aspect_name))) +
    geom_line(size = .7)+
    ylab(ylab) + xlab("Elevation Zone")+
    scale_x_continuous(limits = c(1,14), breaks = seq(1,13,2))+
    scale_y_continuous(limits = c(ylim1,ylim2), breaks = seq(ylim1,ylim2,by))+
    scale_linetype_manual(name = "Hydro Cat", values = c("HD" = "longdash", "CW" = "dotted")) +
    scale_color_manual(name = "Aspect",
                       values = c('North' = 'darkblue', 'South' = 'tomato'),
                       labels = c('NF','SF')) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
          legend.position = 'none',
          legend.direction = 'horizontal',
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
          plot.margin = unit(c(.25,.25, 0,.25), "cm")) +
    annotate("text", x = 11.5, y = ylim2*.95,  
             label = "(b) USJ", color = "Black", size = 6, hjust = 0)

  kern_p <-ggplot(kern, aes(x=ez2,y=get(variable),linetype=hydr0_cat,color=as.factor(aspect_name))) +
      geom_line(size = .7)+
      ylab(ylab) + xlab("Elevation Zone")+
      scale_x_continuous(limits = c(1,14), breaks = seq(1,13,2))+
      scale_y_continuous(limits = c(ylim1,ylim2), breaks = seq(ylim1,ylim2,by))+
      scale_linetype_manual(name = "Hydro Cat", values = c("HD" = "longdash", "CW" = "dotted")) +
      scale_color_manual(name = "Aspect",
                         values = c('North' = 'darkblue', 'South' = 'tomato'),
                         labels = c('NF','SF')) +
      theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
            legend.position = 'none',
            legend.direction = 'horizontal',
            legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
            plot.margin = unit(c(.25,.25, 0,.25), "cm")) +
      annotate("text", x = 11.5, y = ylim2*.95,  
               label = "(c) Kern", color = "Black", size = 6, hjust = 0)

  snow_cow <-plot_grid(yuba_p, usj_p, kern_p, 
                       nrow = 3, 
                       align = "v",
                       axis = "b",
                       rel_heights = c(.36,.33,.36))
}

#### mswe
mswe_big <-big_plot(variable = "mean_ez_mswe", ylab = "MSWE (mm)",
                ylim1 = 0, ylim2 = 2000,by = 400, name = "MSWE")

ggsave(mswe_big,
       file = "./plots/mswe_diff_elevation_plot_v1.png",
       width = 6,
       height = 7,
       units = "in",
       dpi = 300)

system("open ./plots/mswe_diff_elevation_plot_v1.png")

### fm
fm_big <-big_plot(variable = "mean_ez_fm", ylab = "FM (-)",
                    ylim1 = 0, ylim2 = 1,by = .2, name = "FM")

ggsave(fm_big,
       file = "./plots/fm_diff_elevation_plot_v1.png",
       width = 6,
       height = 7,
       units = "in",
       dpi = 300)

system("open ./plots/fm_diff_elevation_plot_v1.png")

max(usj$mean_ez_mwa)

### mwa
mwa_big <-big_plot(variable = "mean_ez_mwa", ylab = "MWA (mm)",
                  ylim1 = 0, ylim2 = 350,by = 50, name = "MWA")

ggsave(mwa_big,
       file = "./plots/mwa_diff_elevation_plot_v1.png",
       width = 6,
       height = 7,
       units = "in",
       dpi = 300)

system("open ./plots/mwa_diff_elevation_plot_v1.png")

### dom
dom_big <-big_plot(variable = "mean_ez_dom", ylab = "DOM (DOWY)",
                   ylim1 = 100, ylim2 = 250,by = 50, name = "DOM")

ggsave(dom_big,
       file = "./plots/dom_diff_elevation_plot_v1.png",
       width = 6,
       height = 7,
       units = "in",
       dpi = 300)

system("open ./plots/dom_diff_elevation_plot_v1.png")
