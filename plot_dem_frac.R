library(terra)
library(sf)
library(dplyr)
library(tidyr)
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

# shape files
kern_sf <-st_geometry(st_read("./vectors/ca_basins/kern.gpkg"))
kern_v <-vect(kern_sf)

# usj
usj_sf <-st_geometry(st_read("./vectors/ca_basins/usj.gpkg"))
usj_v <-vect(usj_sf)

# yuba
yuba_sf <-st_geometry(st_read("./vectors/ca_basins/yuba.gpkg"))
yuba_v <-vect(yuba_sf)

##############
##### dem ####
##############

#### read in metrics
dem_v1 <-rast('./rasters/static/SNSR_DEM.tif')

# crop and mask
kern <-mask(crop(dem_v1,ext(kern_v)), kern_v)
usj <-mask(crop(dem_v1,ext(usj_v)), usj_v)
yuba <-mask(crop(dem_v1,ext(yuba_v)), yuba_v)

# make dfs
k1 <-as.data.frame(kern)
k1$basin <-rep("Kern", nrow(k1))

y1 <-as.data.frame(yuba)
y1$basin <-rep("Yuba", nrow(y1))

u1 <-as.data.frame(usj)
u1$basin <-rep("USJ", nrow(u1))

# bind
df <-bind_rows(k1,y1,u1)

# plot
p <-ggplot(df, aes(x = dem, y = 1 - after_stat(y), color = basin)) +
  stat_ecdf(size = 1.1)+
  ylab("Fraction of area above z") + xlab("Elevation, z (m)")+
  scale_x_continuous(limits = c(1500,4100), breaks = seq(1500,4000,500), expand = c(.01,.001))+
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,.25), expand = c(.01,.01))+
  scale_linetype_manual(name = "Hydro Cat", values = c("HD" = "longdash", "CW" = "dotted")) +
  scale_color_manual(name = "Aspect",
                     values = c('Kern' = 'darkblue', 'Yuba' = 'purple', 'USJ' = 'darkgreen'),
                     labels = c('Kern','Yuba', 'USJ')) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'none',
        plot.margin = unit(c(0.1,0.1,0.1,0.1), "cm"),
        aspect.ratio=1) +
  annotate("text", x = 1550, y = .4,  
           label = "Yuba", color = "purple", size = 6, hjust = 0)+
  annotate("text", x = 2300, y = .25,  
           label = "Kern", color = "darkblue", size = 6, hjust = 0)+
  annotate("text", x = 2700, y = .5,  
           label = "USJ", color = "darkgreen", size = 6, hjust = 0)


ggsave(p,
       file = "./plots/dem_pdf.pdf",
       width = 4,
       height = 4,
       units = "in")          

system("open ./plots/dem_pdf.pdf")
