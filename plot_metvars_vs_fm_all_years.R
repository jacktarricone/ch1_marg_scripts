# plot metvars
# jack tarricone
# june 11th, 2023

library(terra)
library(tidyverse)
library(dtplyr)
library(scales)
library(cowplot)
library(viridis)
library(data.table)

setwd("~/ch1_margulis")


# head_df for col referencing
df_paths <-list.files("./csvs/gridmet_dfs", full.names = TRUE, pattern = "full_stat")
df_list <-lapply(df_paths, fread)
colnames(df_list[[1]])[c(6,24)] <-c("mean_rh","rh_mean")
colnames(df_list[[2]])[c(6,24)] <-c("mean_rh","rh_mean")
colnames(df_list[[3]])[c(6,24)] <-c("mean_rh","rh_mean")

yuba <-df_list[[3]]
# add bins col for box plot
yubs_bins <-yuba  %>%
  mutate(tmean_bin = cut_width(temp_mean_c, width = 1, boundary=-10),
         rhmean_bin = cut_width(rh_mean, width = 5, boundary=0),
         ah_bin = cut_width(abs_hum_gcm3, width = .5, boundary=0),
         srad_bin = cut_width(srad_wm2 , width = 5, boundary=0))

## plot
scale1 <-c("grey99",viridisLite::viridis(30, option = "H", direction = 1))

# ## density
# ggplot(df_v3, aes(x = cc_mean, y = gain_sd, fill = ..density..)) +
#   geom_bin2d(bins = 50) +
#   scale_fill_gradientn(colors = scale1) +
#   theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

# cc scale
cc_scale <-colorRampPalette(c("#f7fcf5", "#00441b"))
cc_scale(13)

ys <- yuba[sample(.N, 1000), ]
y1 <-filter(yuba, ez ==1)
listu <-unique(y1$cell)
y2 <-subset(y1, y1$cell %in% listu[10:15])
head(y2)

ggplot(ys, aes(y = frac_melt, x = temp_mean_c))+
  geom_point(alpha = .5, size = 2, shape = 4)

ggplot(ys, aes(y = frac_melt, x = rh_mean))+
  geom_point(alpha = .5, size = 2, shape = 4, color = "firebrick")

ggplot(ys, aes(y = frac_melt, x = rh_mean))+
  geom_point(alpha = .5, size = 2, shape = 4, color = "firebrick")




scale1 <-c("grey",viridis(30, option = "D", direction = 1))

tmean_p <-ggplot() +
  geom_bin2d(data = ys, bins = 70, aes(x = temp_mean_c, y= frac_melt, fill = after_stat(density))) +
  scale_fill_gradientn(colors = scale1) +
  scale_x_continuous(limits = c(-8,8), expand = (c(0,0))) +
  scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
  labs(y = "fm", x = "tmean")+
  annotate(geom="text", y=.8, x=-5, label= "tmean", size = 8, fontface = "bold")+
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
hist(ys$rh_mean, breaks = 100)
rhmean_p <-ggplot() +
  geom_bin2d(data = ys, bins = 70, aes(x = rh_mean, y= frac_melt, fill = after_stat(density))) +
  scale_fill_gradientn(colors = scale1) +
  scale_x_continuous(limits = c(40,70), expand = (c(0,0))) +
  scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
  labs(y = "fm", x = "rh_mean")+
  annotate(geom="text", y=.8, x=45, label= "rhmean", size = 8, fontface = "bold")+
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

hist(ys$srad_wm2, breaks = 100)
srad_p <-ggplot() +
  geom_bin2d(data = ys, bins = 70, aes(x = srad_wm2, y= frac_melt, fill = after_stat(density))) +
  scale_fill_gradientn(colors = scale1) +
  scale_x_continuous(limits = c(120,150), expand = (c(0,0))) +
  scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
  labs(y = "fm", x = "srad")+
  annotate(geom="text", y=.8, x=45, label= "srad_", size = 8, fontface = "bold")+
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

hist(ys$abs_hum_gcm3, breaks = 100)
abs_hum_p <-ggplot() +
  geom_bin2d(data = ys, bins = 70, aes(x = abs_hum_gcm3, y= frac_melt, fill = after_stat(density))) +
  scale_fill_gradientn(colors = scale1) +
  scale_x_continuous(limits = c(2.5,5), expand = (c(0,0))) +
  scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
  labs(y = "fm", x = "abs_hum")+
  annotate(geom="text", y=.8, x=45, label= "abs_hum", size = 8, fontface = "bold")+
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

