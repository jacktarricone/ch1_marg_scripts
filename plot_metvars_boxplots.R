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


# head_df for col referencing
df_paths <-list.files("./csvs/gridmet_dfs", full.names = TRUE, pattern = "full_stat")
df_list <-lapply(df_paths, fread)
colnames(df_list[[1]])[c(6,24)] <-c("mean_rh","rh_mean")
colnames(df_list[[2]])[c(6,24)] <-c("mean_rh","rh_mean")
colnames(df_list[[3]])[c(6,24)] <-c("mean_rh","rh_mean")

# pull out yuba
yuba <-df_list[[3]]
usj <-df_list[[2]]
kern <-df_list[[1]]

# add bins col for box plot
yuba_bins <-yuba %>%
  mutate(tmean_bin = cut_width(temp_mean_c, width = 1, boundary=-10),
         rhmean_bin = cut_width(rh_mean, width = 2, boundary=0),
         ah_bin = cut_width(abs_hum_gcm3, width = .15, boundary=0),
         srad_bin = cut_width(srad_wm2 , width = 2, boundary=0))

usj <- usj %>%
  mutate(tmean_bin = cut_width(temp_mean_c, width = 1, boundary=-10),
         rhmean_bin = cut_width(rh_mean, width = 2, boundary=0),
         ah_bin = cut_width(abs_hum_gcm3, width = .15, boundary=0),
         srad_bin = cut_width(srad_wm2 , width = 2, boundary=0))

head(yuba_bins)

# box plot test
# cc scale
scale <-colorRampPalette(c("darkblue","#f7fcf5", "darkred"))
scale(13)


plot_4_met_bp <-function(df){
  
  # temp
  temp_p <-ggplot(yuba_bins, mapping = aes(x = temp_mean_c, y = frac_melt, fill = as.factor(tmean_bin))) +
    geom_boxplot(linewidth = .5, varwidth = TRUE, 
                 outlier.size = .3, outlier.shape = 1, outlier.color = "grey80", outlier.alpha = .01) +
    xlab("tmean (c)") + ylab("frac_melt")+ 
    scale_x_continuous(limits = c(-10,10), breaks = seq(-10,10,2), expand = c(0,.5)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_fill_discrete(type = scale(13)) +
    theme_classic(14) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
          legend.position = "none",
          legend.title = element_blank(),
          legend.margin = margin(-5,1,1,1),
          legend.box.background = element_rect(colour = "black"))

# srad
scale2 <-colorRampPalette(c("brown","yellow"))

srad_p <-ggplot(yuba_bins, mapping = aes(x = srad_wm2, y = frac_melt, fill = as.factor(srad_bin))) +
  geom_boxplot(linewidth = .5, varwidth = TRUE, 
               outlier.size = .3, outlier.shape = 1, outlier.color = "grey80", outlier.alpha = .01) +
  xlab("srad (w/m^2)") + ylab("frac_melt")+ 
  scale_x_continuous(limits = c(120,150), breaks = seq(120,150,5), expand = c(0,.5)) +
  scale_y_continuous(limits = c(0,1)) +
  scale_fill_discrete(type = scale2(15)) +
  theme_classic(14) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
        legend.position = "none",
        legend.title = element_blank(),
        legend.margin = margin(-5,1,1,1),
        legend.box.background = element_rect(colour = "black"))

  # rh
  scale3 <-colorRampPalette(c("orange","purple"))
  
  rh_p <-ggplot(yuba_bins, mapping = aes(x = rh_mean, y = frac_melt, fill = as.factor(rhmean_bin))) +
    geom_boxplot(linewidth = .5, varwidth = TRUE, 
                 outlier.size = .3, outlier.shape = 1, outlier.color = "grey80", outlier.alpha = .01) +
    xlab("rh_mean (%)") + ylab("frac_melt")+ 
    scale_x_continuous(limits = c(40,70), breaks = seq(40,70,5), expand = c(0,.5)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_fill_discrete(type = scale3(15)) +
    theme_classic(14) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
          legend.position = "none",
          legend.title = element_blank(),
          legend.margin = margin(-5,1,1,1),
          legend.box.background = element_rect(colour = "black"))
  
  # ah
  ah_p <-ggplot(yuba_bins, mapping = aes(x = abs_hum_gcm3, y = frac_melt, fill = as.factor(ah_bin))) +
    geom_boxplot(linewidth = .5, varwidth = TRUE, 
                 outlier.size = .3, outlier.shape = 1, outlier.color = "grey80", outlier.alpha = .01) +
    xlab("abs_hum (g/cm^3)") + ylab("frac_melt")+ 
    scale_x_continuous(limits = c(2.5,5), breaks = seq(2.5,5,.5), expand = c(0,.5)) +
    scale_y_continuous(limits = c(0,1)) +
    scale_fill_discrete(type = scale3(18)) +
    theme_classic(14) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linetype = 1),
          legend.position = "none",
          legend.title = element_blank(),
          legend.margin = margin(-5,1,1,1),
          legend.box.background = element_rect(colour = "black"))
  
  # cowplot test
  cow <-plot_grid(temp_p, rh_p, srad_p, ah_p,
                  labels = c("(a)","(b)","(c)","(d)"),
                  nrow = 2, 
                  ncol = 2,
                  align = "hv",
                  label_size = 15,
                  vjust =  3,
                  hjust = -.7,
                  rel_heights = c(1/2,1/2),
                  rel_widths = c(1/2,1/2))
 return(cow)
  }

# test save
# make tighter together
ggsave(cow,
       file = "./plots/metvars_boxplots_v1.png",
       width = 8, 
       height = 6,
       dpi = 300)

system("open ./plots/metvars_boxplots_v1.png")


ys <- yuba[sample(.N, 1000), ]
y1 <-filter(yuba, ez ==3)
listu <-unique(yuba$cell)
y2 <-subset(y1, yuba$cell %in% listu[50:100])
head(y2)

ggplot(ys, aes(y = frac_melt, x = temp_mean_c))+
  geom_point(alpha = .5, size = 2, shape = 4)

ggplot(ys, aes(y = frac_melt, x = rh_mean))+
  geom_point(alpha = .5, size = 2, shape = 4, color = "firebrick")


# plot
mycolors <-rev(brewer.pal(9, "Spectral"))

ggplot(y2, aes(y = frac_melt, x = abs_hum_gcm3, color = wy))+
  geom_point(alpha = .8, size = 2, shape = 4) +
  scale_color_gradientn(colors = mycolors, limits = c(1985,2016), oob = squish) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        panel.background = element_rect(fill = 'white'),
        aspect.ratio = 1,
        legend.position  = 'bottom',
        plot.margin = unit(c(.25,.1,.1,.1), "cm"),
        legend.box.spacing = unit(0, "pt")) +
  guides(color = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position = 'bottom',
                               title.hjust = .5,
                               barwidth = 15,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black"))

ggsave(file = "./plots/random_fm_ah_wy_v1.png",
       width = 4.4, 
       height = 5,
       dpi = 300)

system("open ./plots/random_fm_ah_wy_v1.png")


ggplot(y2, aes(y = mswe_mm, x = abs_hum_gcm3, color = wy))+
  geom_point(alpha = .8, size = 2, shape = 4) +
  scale_color_gradientn(colors = mycolors, limits = c(1985,2016), oob = squish) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        panel.background = element_rect(fill = 'white'),
        aspect.ratio = 1,
        legend.position  = 'bottom',
        plot.margin = unit(c(.25,.1,.1,.1), "cm"),
        legend.box.spacing = unit(0, "pt")) +
  guides(color = guide_colorbar(direction = "horizontal",
                                label.position = 'top',
                                title.position = 'bottom',
                                title.hjust = .5,
                                barwidth = 15,
                                barheight = 1,
                                frame.colour = "black", 
                                ticks.colour = "black"))

ggsave(file = "./plots/random_mswe_ah_wy_v1.png",
       width = 4.4, 
       height = 5,
       dpi = 300)

system("open ./plots/random_mswe_ah_wy_v1.png")

ggplot(y2, aes(y = srad_wm2, x = abs_hum_gcm3, color = wy))+
  geom_point(alpha = .8, size = 2, shape = 4) +
  scale_color_gradientn(colors = mycolors, limits = c(1985,2016), oob = squish) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        panel.background = element_rect(fill = 'white'),
        aspect.ratio = 1,
        legend.position  = 'bottom',
        plot.margin = unit(c(.25,.1,.1,.1), "cm"),
        legend.box.spacing = unit(0, "pt")) +
  guides(color = guide_colorbar(direction = "horizontal",
                                label.position = 'top',
                                title.position = 'bottom',
                                title.hjust = .5,
                                barwidth = 15,
                                barheight = 1,
                                frame.colour = "black", 
                                ticks.colour = "black"))

ggsave(file = "./plots/random_srad_ah_wy_v1.png",
       width = 4.4, 
       height = 5,
       dpi = 300)

system("open ./plots/random_srad_ah_wy_v1.png")

ggplot(y2, aes(y = temp_mean_c, x = abs_hum_gcm3, color = wy))+
  geom_point(alpha = .8, size = 2, shape = 4) +
  scale_color_gradientn(colors = mycolors, limits = c(1985,2016), oob = squish) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        panel.background = element_rect(fill = 'white'),
        aspect.ratio = 1,
        legend.position  = 'bottom',
        plot.margin = unit(c(.25,.1,.1,.1), "cm"),
        legend.box.spacing = unit(0, "pt")) +
  guides(color = guide_colorbar(direction = "horizontal",
                                label.position = 'top',
                                title.position = 'bottom',
                                title.hjust = .5,
                                barwidth = 15,
                                barheight = 1,
                                frame.colour = "black", 
                                ticks.colour = "black"))

ggsave(file = "./plots/random_temp_ah_wy_v1.png",
       width = 4.4, 
       height = 5,
       dpi = 300)

system("open ./plots/random_temp_ah_wy_v1.png")



test <-filter(y2, abs_hum_gcm3 > 3.6)


