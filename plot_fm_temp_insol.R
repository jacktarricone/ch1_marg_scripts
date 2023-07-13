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
yuba_df <-fread("./csvs/gridmet_dfs/yuba_full_stats_v4.csv")
usj_df <-fread("./csvs/gridmet_dfs/usj_full_stats_v4.csv")
kern_df <-fread("./csvs/gridmet_dfs/kern_full_stats_v4.csv")

# join
joined_df <-as.data.table(dplyr::bind_rows(yuba_df, usj_df, kern_df))

# remove unneeded annual data
head(df_v1)

# add bins col for box plot
mean_df <-joined_df %>%
  dplyr::mutate(bin = cut_width(mean_temp_c, width = 1, boundary=0))

# split up by basin
usj_p1 <-filter(mean_df, basin_name == "usj")
usj_p <-filter(usj_df1,  aspect == 1 | aspect == 3)

yuba_p1 <-filter(mean_df, basin_name == "yuba")
yuba_p <-filter(yuba_p1, aspect == 1 | aspect == 3)

kern_p1 <-filter(mean_df, basin_name == "kern")
kern_p <-filter(kern_p1,aspect == 1 | aspect == 3)


hist(yuba_p$aspect)
hist(kern_p$aspect)
hist(usj_p$aspect)


#######################################
############ aspect ####################
#######################################

# set scale
yuba_scale <-c(rep("dodgerblue3",8),rep("firebrick",8))

# yuba
yuba_aspect <-ggplot(yuba_p, mapping = aes(x = mean_temp_c,
                             y = mean_fm, fill = interaction(bin,aspect))) +
  geom_boxplot(linewidth = .5, varwidth = FALSE, 
               outlier.size = .3, outlier.shape = 4, outlier.color = "grey80", outlier.alpha = .01) +
  xlab(NA) + ylab("FM")+ 
  geom_vline(xintercept = 0, linetype = 2, alpha = .5)+
  annotate(geom="text", x = -4.5, y = .93, label= "(a) Yuba", size = 8, fontface = "bold")+
  scale_x_continuous(limits = c(-6,9), breaks = seq(-6,9,1), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,1)) +
  scale_fill_discrete(type = yuba_scale) +
  theme_classic(14) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))
yuba_aspect

# usj
usj_scale <-c(rep("dodgerblue3",15),rep("firebrick",15))

### plot
usj_aspect <-ggplot(usj_p, mapping = aes(x = mean_temp_c,
                                           y = mean_fm, fill = interaction(bin,aspect))) +
  geom_boxplot(linewidth = .5, varwidth = FALSE, 
               outlier.size = .3, outlier.shape = 4, outlier.color = "grey80", outlier.alpha = .01) +
  xlab(NA) + ylab("FM")+ 
  geom_vline(xintercept = 0, linetype = 2, alpha = .5)+
  annotate(geom="text", x = -4.5, y = .93, label= "(b) USJ", size = 8, fontface = "bold")+
  scale_x_continuous(limits = c(-6,9), breaks = seq(-6,9,1), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,1)) +
  scale_fill_discrete(type = usj_scale) +
  theme_classic(14) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))
usj_aspect


### kern
# set scale
kern_scale <-c(rep("dodgerblue3",15),rep("firebrick",15))

kern_aspect <-ggplot(kern_p, mapping = aes(x = mean_temp_c,
                                           y = mean_fm, fill = interaction(bin,aspect))) +
  geom_boxplot(linewidth = .5, varwidth = FALSE, 
               outlier.size = .3, outlier.shape = 4, outlier.color = "grey80", outlier.alpha = .01) +
  xlab(expression('ONDJFM ' ~T["Mean"] ~ '(°C)')) + ylab("FM")+ 
  geom_vline(xintercept = 0, linetype = 2, alpha = .5)+
  annotate(geom="text", x = -4.5, y = .93, label= "(c) Kern", size = 8, fontface = "bold")+
  scale_x_continuous(limits = c(-6,9), breaks = seq(-6,9,1), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,1)) +
  scale_fill_discrete(type = kern_scale) +
  theme_classic(14) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'none',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))
kern_aspect


## for shared legend, this is it
aspect_cow <-grid.arrange(yuba_aspect, usj_aspect, kern_aspect,
                         nrow = 3,
                         heights = c(1, 1, 1.15)) 

# save
ggsave(aspect_cow,
       file = "./plots/aspect_temp_fm_boxplots_v1.png",
       width = 8, 
       height = 9,
       dpi = 300)

system("open ./plots/aspect_temp_fm_boxplots_v1.png")


########################################
############.   mwa ####################
########################################

### yuba mwa
yuba_mwa <-ggplot(yuba_p, mapping = aes(x = mean_temp_c,
                                           y = mean_mwa, fill = interaction(bin,aspect))) +
  geom_boxplot(linewidth = .5, varwidth = FALSE, 
               outlier.size = .3, outlier.shape = 4, outlier.color = "grey80", outlier.alpha = .01) +
  xlab(NA) + ylab("MWA (mm)")+ 
  annotate(geom="text", x = -5, y = 580, label= "", size = 8, fontface = "bold")+
  geom_vline(xintercept = 0, linetype = 2, alpha = .5)+
  scale_x_continuous(limits = c(-6,9), breaks = seq(-6,9,1), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100)) +
  scale_fill_discrete(type = yuba_scale) +
  theme_classic(14) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))
yuba_mwa


### usj mwa
usj_mwa <-ggplot(usj_p, mapping = aes(x = mean_temp_c,
                                        y = mean_mwa, fill = interaction(bin,aspect))) +
  geom_boxplot(linewidth = .5, varwidth = FALSE, 
               outlier.size = .3, outlier.shape = 4, outlier.color = "grey80", outlier.alpha = .01) +
  xlab(NA) + ylab("MWA (mm)")+ 
  annotate(geom="text",  x = -5, y = 580, label= "", size = 8, fontface = "bold")+
  geom_vline(xintercept = 0, linetype = 2, alpha = .5)+
  scale_x_continuous(limits = c(-6,9), breaks = seq(-6,9,1), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100)) +
  scale_fill_discrete(type = usj_scale) +
  theme_classic(14) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'none',
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))
usj_mwa

### kern mwa
kern_mwa <-ggplot(kern_p, mapping = aes(x = mean_temp_c,
                                      y = mean_mwa, fill = interaction(bin,aspect))) +
  geom_boxplot(linewidth = .5, varwidth = FALSE, 
               outlier.size = .3, outlier.shape = 4, outlier.color = "grey80", outlier.alpha = .01) +
  annotate(geom="text",  x = -5, y = 580, label= "", size = 8, fontface = "bold")+
  xlab(expression('ONDJFM ' ~T["Mean"] ~ '(°C)')) + ylab("MWA (mm)")+ 
  geom_vline(xintercept = 0, linetype = 2, alpha = .5)+
  scale_x_continuous(limits = c(-6,9), breaks = seq(-6,9,1), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,600), breaks = seq(0,600,100)) +
  scale_fill_discrete(type = kern_scale) +
  theme_classic(14) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'none',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))
kern_mwa


## for shared legend, this is it
mwa_cow <-grid.arrange(yuba_mwa, usj_mwa, kern_mwa,
                          nrow = 3,
                          heights = c(1, 1, 1.15)) 

# save
ggsave(mwa_cow,
       file = "./plots/aspect_temp_mwa_boxplots_v2.png",
       width = 8, 
       height = 9,
       dpi = 300)

system("open ./plots/aspect_temp_mwa_boxplots_v1.png")

## for shared legend, this is it
full_cow <-grid.arrange(aspect_cow,mwa_cow,
                       ncol = 2,
                       widths = c(1, 1)) 

# save
ggsave(full_cow,
       file = "./plots/aspect_temp_mwa_fum_full_boxplots_v2.png",
       width = 16, 
       height = 9,
       dpi = 300)

system("open ./plots/aspect_temp_mwa_fum_full_boxplots_v2.png")

