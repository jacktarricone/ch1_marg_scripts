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

# View the sorted dataframe
df_sorted
usj <-as.data.frame(filter(snow_results, basin_name == "USJ"))
kern <-as.data.frame(filter(snow_results, basin_name == "Kern"))
yuba <-as.data.frame(filter(snow_results, basin_name == "Yuba"))

min(df_sorted$diff_ez_fm)

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












yuba_p <-ggplot(yuba, aes(x=ez2,y=mean_ez_fm,linetype=hydr0_cat,color=as.factor(aspect_name))) +
  geom_line(size = .7)+
  ylab("FM (-)") + xlab("Elevation Zone")+
  scale_x_continuous(limits = c(1,14), breaks = seq(1,13,2))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))+
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
  annotate("text", x = 11.5, y = .95,  
           label = "(a) Yuba", color = "Black", size = 6, hjust = 0)
yuba_p

usj_p <-ggplot(usj, aes(x=ez2,y=mean_ez_fm,linetype=hydr0_cat,color=as.factor(aspect_name))) +
  geom_line(size = .7)+
  ylab("FM (-)") + xlab("Elevation Zone")+
  scale_x_continuous(limits = c(1,14), breaks = seq(1,13,2))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))+
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
  annotate("text", x = 11.5, y = .95,  
           label = "(b) USJ", color = "Black", size = 6, hjust = 0)

usj_p

kern_p <-ggplot(kern, aes(x=ez2,y=mean_ez_fm,linetype=hydr0_cat,color=as.factor(aspect_name))) +
  geom_line(size = .7)+
  ylab("FM (-)") + xlab("Elevation Zone")+
  scale_x_continuous(limits = c(1,14), breaks = seq(1,13,2))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.2))+
  scale_linetype_manual(name = "Hydro Cat", values = c("HD" = "longdash", "CW" = "dotted")) +
  scale_color_manual(name = "Aspect",
                     values = c('North' = 'darkblue', 'South' = 'tomato'),
                     labels = c('NF','SF')) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'none',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm")) +
  annotate("text", x = 11.5, y = .95,  
           label = "(c) Kern", color = "Black", size = 6, hjust = 0)

kern_p

snow_cow <-plot_grid(yuba_p, usj_p, kern_p, 
                     nrow = 3, 
                     align = "v",
                     axis = "b",
                     rel_heights = c(.36,.33,.36))

ggsave(snow_cow,
       file = "./plots/ns_metric_diff_elevation_plot_v1.png",
       width = 6, 
       height = 7,
       units = "in",
       dpi = 300) 

system("open ./plots/ns_metric_diff_elevation_plot_v1.png")



# ggsave(snow_cow,
#        file = "./plots/metric_diff_elevation_plot_v1.png",
#        width = 6, 
#        height = 9,
#        units = "in",
#        dpi = 300) 
# 
# system("open ./plots/metric_diff_elevation_plot_v1.png")




























######
# by elevations
######

fm <-ggplot(df_sorted, aes(x=ez2,y=diff_ez_fm,linetype=basin_name,color=hydr0_cat)) +
  geom_line() +
  ylab("SF-NF FM (-)") + xlab("Elevation Zone")+
  scale_x_continuous(limits = c(1,14), breaks = seq(1,13,2))+
  scale_linetype_manual(name = "Basin", values = c("Kern" = "solid", "USJ" = "dotted", "Yuba" = "longdash")) +
  scale_color_manual(name = "Basin",
                    values = c('CW' = 'darkblue', 'HD' = 'tomato'),
                    labels = c('CW','HD')) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
          legend.position = 'top',
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.direction = 'horizontal',
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
          plot.margin = unit(c(.25,.25, 0,.25), "cm")) +
  annotate("text", x = 11, y = max(df_sorted$diff_ez_fm)*.93,  
           label = "(a) FM", color = "Black", size = 6, hjust = 0)

mwa <-ggplot(df_sorted, aes(x=ez2,y=diff_ez_mwa,linetype=basin_name,color=hydr0_cat)) +
  geom_line() +
  ylab("SF-NF MWA (mm)") + xlab("Elevation Zone")+
  scale_x_continuous(limits = c(1,14), breaks = seq(1,13,2))+
  scale_y_continuous(limits = c(0,300), breaks = seq(0,300,100))+
  scale_linetype_manual(name = "Basin", values = c("Kern" = "solid", "USJ" = "dotted", "Yuba" = "longdash")) +
  scale_color_manual(name = "Basin",
                     values = c('CW' = 'darkblue', 'HD' = 'tomato'),
                     labels = c('CW','HD')) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm")) +
  annotate("text", x = 11, y = max(df_sorted$diff_ez_mwa)*1.05, 
           label = "(b) MWA", color = "Black", size = 6, hjust = 0)

mswe <-ggplot(df_sorted, aes(x=ez2,y=diff_ez_mswe,linetype=basin_name,color=hydr0_cat)) +
  geom_line() +
  ylab("SF-NF MSWE (mm)") + xlab("Elevation Zone")+
  scale_x_continuous(limits = c(1,14), breaks = seq(1,13,2))+
  scale_y_continuous(limits = c(-600,0), breaks = seq(0,-600,-200))+
  scale_linetype_manual(name = "Basin", values = c("Kern" = "solid", "USJ" = "dotted", "Yuba" = "longdash")) +
  scale_color_manual(name = "Basin",
                     values = c('CW' = 'darkblue', 'HD' = 'tomato'),
                     labels = c('CW','HD')) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'none',
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm")) +
  annotate("text", x = 11, y = min(df_sorted$diff_ez_mswe)*.07, 
           label = "(c) MSWE", color = "Black", size = 6, hjust = 0)

dom <-ggplot(df_sorted, aes(x=ez2,y=diff_ez_dom,linetype=basin_name,color=hydr0_cat)) +
  geom_line() +
  ylab("SF-NF DOM (days)") + xlab("Elevation Zone")+
  scale_x_continuous(limits = c(1,14), breaks = seq(1,13,2))+
  scale_y_continuous(limits = c(-40,0), breaks = seq(0,-40,-10))+
  scale_linetype_manual(name = "Basin", values = c("Kern" = "solid", "USJ" = "dotted", "Yuba" = "longdash")) +
  scale_color_manual(name = "Basin",
                     values = c('CW' = 'darkblue', 'HD' = 'tomato'),
                     labels = c('CW','HD')) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'none',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm")) +
  annotate("text", x = 11, y = min(df_sorted$diff_ez_dom)*.07, 
            label = "(d) DOM", color = "Black", size = 6, hjust = 0)

snow_cow <-plot_grid(fm, mwa, mswe, dom, 
                     nrow = 4, 
                     align = "v",
                     axis = "b",
                     label_size = 14,
                     vjust =  2.4,
                     hjust = -2.6,
                     rel_heights = c(.27,.22,.22,.27))

ggsave(snow_cow,
       file = "./plots/metric_diff_elevation_plot_v1.png",
       width = 6, 
       height = 9,
       units = "in",
       dpi = 300) 

system("open ./plots/metric_diff_elevation_plot_v1.png")

# save
# write.csv(df_sorted, "./csvs/snow_avg_metric_results_table_v4.csv", row.names = FALSE)
# system("open ./csvs/snow_avg_metric_results_table_v4.csv")


######################################
######################################
############ met data ###############
######################################
######################################

# met data
met_results <-df %>%
  group_by(basin_name, zone_name, aspect_name) %>%
  summarise(mean_ez_temp_c  = round(mean(mean_temp_c),2),
            mean_ez_ah = round(mean(mean_ah),2),
            mean_ez_srad = as.integer(mean(mean_srad)),
            mean_cs_insol  = as.integer(mean(insol_watts)))

print(met_results, n = nrow(met_results))

# Reshape the data into separate columns for aspect_name
met_results_wide <- met_results   %>%
  tidyr::pivot_wider(names_from = aspect_name,
                     values_from = c(mean_ez_temp_c, mean_ez_ah, mean_ez_srad, mean_cs_insol))

# calc diff
met_diff <- met_results_wide  %>%
  mutate(
    diff_ez_temp_c = mean_ez_temp_c_South - mean_ez_temp_c_North,
    diff_ez_ah = mean_ez_ah_South - mean_ez_ah_North,
    diff_ez_srad = mean_ez_srad_South - mean_ez_srad_North,
    diff_ez_insol = mean_cs_insol_South - mean_cs_insol_North
    
  )

# Sort the dataframe
met_df_sorted <- met_diff %>%
  select(basin_name, zone_name, matches("temp"), matches("ah"), matches("srad"), matches("insol"))

# View the sorted dataframe
met_df_sorted

# save
write.csv(met_df_sorted, "./csvs/met_avg_metric_results_table_v2.csv", row.names = FALSE)
system("open ./csvs/met_avg_metric_results_table_v2.csv")



