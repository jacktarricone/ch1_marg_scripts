# fusion study area plot and table creation
# plot swe change sd rasters
# jack tarricone
# june 13th

library(terra)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)
library(scales)
library(ggpubr)



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

theme_set(theme_classic(17))

setwd("~/ch1_margulis")

## bring in SNSR shape file
# with terra
snsr <-vect("./vectors/snsr_shp.gpkg")
snsr_basins <-vect("./vectors/ca_basins/snsr_all_basins.shp")

# with sf
snsr_v1 <-st_read("./vectors/snsr_shp.gpkg")
snsr_sf <-st_geometry(snsr_v1)
snsr_v <-vect(snsr_sf)

# ca state boundary
ca_v1 <-st_geometry(st_read("/Users/jacktarricone/ch1_jemez/vector_data/states/cb_2018_us_state_20m.shp"))
ca <-st_transform(ca_v1, "EPSG:4326")

# kern
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

# bring in mswe_mean
general_sa_v1 <-rast("./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif")
general_sa <-ifel(general_sa_v1 > 0, -99, general_sa_v1)
plot(general_sa)

# mask aspect
aspect_v1 <-rast("./rasters/categorized/aspect_thres_4_classes.tif")
aspect_v2 <-ifel(aspect_v1 == 4, NA, aspect_v1)
aspect_v3 <-ifel(aspect_v1 == 2, NA, aspect_v2)
aspect <-mask(aspect_v3, general_sa, maskvalue = NA)
plot(aspect)

# crop and mask
kern_sa <-mask(crop(aspect,ext(kern_v)), kern_v)
snsr_kern <-crop(snsr_v, kern_v)
plot(kern_sa)
plot(kern_v, add = TRUE)
plot(snsr_kern, add = TRUE)

usj_sa <-mask(crop(aspect,ext(usj_v)), usj_v)
snsr_usj <-crop(snsr_v, usj_v)
plot(usj_sa)
plot(usj_v, add = TRUE)
plot(snsr_usj, add = TRUE)

yuba_sa <-mask(crop(aspect,ext(yuba_v)), yuba_v)
snsr_yuba <-crop(snsr_v, yuba_v)
plot(yuba_sa)
plot(yuba_v, add = TRUE)
plot(snsr_yuba, add = TRUE)

# make dfs
kern_sa_df <-as.data.frame(kern_sa, xy = TRUE, cells = TRUE)
usj_sa_df <-as.data.frame(usj_sa, xy = TRUE, cells = TRUE)
yuba_sa_df <-as.data.frame(yuba_sa, xy = TRUE, cells = TRUE)

head(kern_sa_df)
# make sfs
snsr_kern_sf <-st_as_sf(snsr_kern)
snsr_usj_sf <-st_as_sf(snsr_usj)
snsr_yuba_sf <-st_as_sf(snsr_yuba)

kern_plot <-ggplot(kern_sa_df) +
  geom_sf(data = snsr_kern_sf, fill = "gray70", color = "black", linewidth = .01, inherit.aes = FALSE) +
  geom_raster(mapping = aes(x,y, fill = as.factor(aspect))) +
  geom_sf(data = snsr_kern_sf, fill = NA, color = "darkorange", linewidth = .3, inherit.aes = FALSE) +
  geom_sf(data = kern_sf, fill = NA, color = "black", linewidth = .4, inherit.aes = FALSE) +
  scale_fill_manual(values= c("darkblue","darkred")) +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.box.spacing = unit(0, "pt")) 
kern_plot

usj_plot <-ggplot(usj_sa_df) +
  geom_sf(data = snsr_usj_sf, fill = "gray70", color = "black", linewidth = .01, inherit.aes = FALSE) +
  geom_raster(mapping = aes(x,y, fill = as.factor(aspect))) +
  geom_sf(data = snsr_usj_sf, fill = NA, color = "darkorange", linewidth = .3, inherit.aes = FALSE) +
  geom_sf(data = usj_sf, fill = NA, color = "black", linewidth = .4, inherit.aes = FALSE) +
  scale_fill_manual(values= c("darkblue","darkred")) +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.box.spacing = unit(0, "pt")) 
usj_plot

yuba_plot <-ggplot(yuba_sa_df) +
  geom_sf(data = snsr_yuba_sf, fill = "gray70", color = "black", linewidth = .01, inherit.aes = FALSE) +
  geom_raster(mapping = aes(x,y, fill = as.factor(aspect))) +
  geom_sf(data = snsr_yuba_sf, fill = NA, color = "darkorange", linewidth = .3, inherit.aes = FALSE) +
  geom_sf(data = yuba_sf, fill = NA, color = "black", linewidth = .4, inherit.aes = FALSE) +
  scale_fill_manual(values= c("darkblue","darkred")) +
  theme(panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.box.spacing = unit(0, "pt")) 
yuba_plot


yu_cow <-plot_grid(kern_plot,yuba_plot, usj_plot,
                     labels = c("(a)","(b)", "(c)"),
                     align = "h", # Align them both, horizontal and vertical
                     ncol = 3,
                     vjust =  2,
                     hjust = -3,
                     rel_widths = c(1.14, 4, 3),
                     axis = "t")  

ggsave(yu_cow,
       file = "./plots/kuy_snow_metric_area_v2.png",
       width = 8, 
       height = 2.7,
       dpi = 300)

system("open ./plots/kuy_snow_metric_area_v2.png")

