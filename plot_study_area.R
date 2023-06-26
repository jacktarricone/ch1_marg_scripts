### plot study area static metrics
# dem, canopy cover

# jack tarricone

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
plot(ca)
ca_v <-vect(ca)
plot(dem_v1)
plot(ca_v, add = TRUE)
plot(kern_v, add = TRUE)
plot(usj_v, add = TRUE)
plot(yuba_v, add = TRUE)

# kern
kern_sf <-st_geometry(st_read("./vectors/ca_basins/kern.gpkg"))
kern_v <-vect(kern)

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
names(dem_v1) <-"dem"
dem_full_df <-as.data.frame(dem_v1, xy = TRUE, cells = TRUE)

cc_v1 <-rast("./rasters/nlcd_cc/cc_w0.tif")
names(cc_v1) <-"cc_percent"

ez_bins_v1 <-rast("./rasters/categorized/dem_6zb.tif")
ez_bins <-ifel(6 == ez_bins_v1, 5, ez_bins_v1)
names(ez_bins) <-"ez"

aspect <-rast("./rasters/categorized/aspect_thres_4_classes.tif")

# stack
stack <-c(dem_v1,cc_v1,ez_bins,aspect)

# crop and mask
kern_stack <-mask(crop(stack,ext(kern_v)), kern_v)
usj_stack <-mask(crop(stack,ext(usj_v)), usj_v)
yuba_stack <-mask(crop(stack,ext(yuba_v)), yuba_v)
plot(kern_stack)

# convert to df for geom_raster
kern_df_v1 <-as.data.frame(kern_stack, xy = TRUE, cells = TRUE)
usj_df_v1 <-as.data.frame(usj_stack, xy = TRUE, cells = TRUE)
yuba_df_v1 <-as.data.frame(yuba_stack, xy = TRUE, cells = TRUE)

# make cat variables fun
add_cat_vars <-function(df){
  df$bin_name <-ifelse(df$ez == 1, "1500-1900 m", df$ez)
  df$bin_name <-ifelse(df$ez == 2, "1900-2300 m", df$bin_name)
  df$bin_name <-ifelse(df$ez == 3, "2300-2700 m", df$bin_name)
  df$bin_name <-ifelse(df$ez == 4, "2700-3100 m", df$bin_name)
  df$bin_name <-ifelse(df$ez == 5, "3100-4361 m", df$bin_name)
  df$bin_name <-ifelse(df$ez == 6, "3100-4361 m", df$bin_name)
  
  df$aspect_name <-ifelse(df$aspect == 1, "North", df$aspect)
  df$aspect_name <-ifelse(df$aspect == 2, "East", df$aspect_name)
  df$aspect_name <-ifelse(df$aspect == 3, "South", df$aspect_name)
  df$aspect_name <-ifelse(df$aspect == 4, "West", df$aspect_name)
  return(df)
}

# run
kern_df <-add_cat_vars(df = kern_df_v1)
usj_df <-add_cat_vars(df = usj_df_v1)
yuba_df <-add_cat_vars(df = yuba_df_v1)


## define color scales
# set scale
topo_table <-read.csv("./gis/topo_colors.csv")
topo_colors <-c(topo_table$colors)
cc_scale <-c(brewer.pal(9, 'YlGn'))
ez_colors <-c("palegreen", "palegreen4", "yellow", "sienna4", "white")

### define functions with and without color bor
# ez
plot_ez <-function(df, shp){
  
  ez_plot <-ggplot(df) +
    geom_sf(data = shp, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE) +
    geom_raster(data = df, mapping = aes(x,y, fill = as.factor(df$bin_name))) +
    scale_fill_manual(values = ez_colors) +
    labs(fill = "EZ") +
    geom_sf(data = shp, fill = NA, color = "black", linewidth = .3, inherit.aes = FALSE) + # for black line
    theme(panel.border = element_blank(),
          legend.key = element_rect(color = "black"),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.text = element_text(size=10),
          plot.margin = unit(c(0,0,0,0), "cm"),
          legend.key.size = unit(.5, "cm")) + # Increase the spacing between legend items
          guides(fill=guide_legend(nrow=2,byrow=TRUE))
  return(ez_plot)
}
plot_ez_nl <-function(df, shp){
  
  ez_plot <-ggplot(df) +
    geom_sf(data = shp, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE) +
    geom_raster(data = df, mapping = aes(x,y, fill = as.factor(df$bin_name))) +
    scale_fill_manual(values = ez_colors) +
    labs(fill = "EZ") +
    geom_sf(data = shp, fill = NA, color = "black", linewidth = .3, inherit.aes = FALSE) + # for black line
    theme(panel.border = element_blank(),
          legend.key = element_rect(color = "black"),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none')
  return(ez_plot)
}

# dem
plot_dem <-function(df, shp){
  
  dem_plot <-ggplot(df) +
       geom_sf(data = shp, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE) +
       geom_raster(mapping = aes(x,y, fill = df$dem)) +
       geom_sf(data = shp, fill = NA, color = "black", linewidth = .3, inherit.aes = FALSE) +
       coord_sf(label_graticule = "NW") +
       scale_fill_gradientn(colors = topo_colors, limits = c(1500,4000), na.value="#ebe9eb") + # max of color bar so it saturates
       labs(fill = "Elevation (m)") +
       theme(panel.border = element_blank(),
             axis.text.x = element_blank(),
             axis.title.y = element_blank(),
             axis.title.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank(),
             legend.position = "bottom",
             plot.margin = unit(c(0,0,0,0), "cm"),
             legend.box.spacing = unit(0, "pt")) +
       guides(fill = guide_colorbar(direction = "horizontal",
                                    label.position = 'bottom',
                                    title.position ='top',
                                    title.hjust = .5,
                                    barwidth = 14,
                                    barheight = 1,
                                    frame.colour = "black", 
                                    ticks.colour = "black")) 

    return(dem_plot)
}
plot_dem_nl <-function(df, shp){
  
  dem_plot <-ggplot(df) +
    geom_sf(data = shp, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE) +
    geom_raster(mapping = aes(x,y, fill = df$dem)) +
    geom_sf(data = shp, fill = NA, color = "black", linewidth = .3, inherit.aes = FALSE) +
    coord_sf(label_graticule = "NW") +
    scale_fill_gradientn(colors = topo_colors, limits = c(1500,4000), na.value="#ebe9eb") + # max of color bar so it saturates
    labs(fill = "Elevation (m)") +
    theme(panel.border = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "none",
          plot.margin = unit(c(0,0,0,0), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(fill = guide_colorbar(direction = "vertical",
                                 label.position = 'right',
                                 title.position ='top',
                                 title.hjust = .5,
                                 barwidth = 1,
                                 barheight = 10,
                                 frame.colour = "black", 
                                 ticks.colour = "black")) 
  
  return(dem_plot)
}

# plot
plot_cc <-function(df, shp){
  
  cc_plot <-ggplot(df) +
    geom_sf(data = shp, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE) +
    geom_raster(mapping = aes(x,y, fill = df$cc_percent)) +
    geom_sf(data = shp, fill = NA, color = "black", linewidth = .3, inherit.aes = FALSE) +
    scale_fill_gradientn(colors = cc_scale, limits = c(0,70), na.value="#ebe9eb") + # max of color bar so it saturates
    labs(fill = "CC (%)") +
    theme(panel.border = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = "bottom",
          plot.margin = unit(c(0,0,0,0), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(fill = guide_colorbar(direction = "horizontal",
                                 label.position = 'bottom',
                                 title.position ='top',
                                 title.hjust = .5,
                                 barwidth = 14,
                                 barheight = 1,
                                 frame.colour = "black", 
                                 ticks.colour = "black")) 
  
  return(cc_plot)
}
plot_cc_nl <-function(df, shp){
  
  cc_plot <-ggplot(df) +
    geom_sf(data = shp, fill = "gray80", color = "black", linewidth = .1, inherit.aes = FALSE) +
    geom_raster(mapping = aes(x,y, fill = df$cc_percent)) +
    geom_sf(data = shp, fill = NA, color = "black", linewidth = .3, inherit.aes = FALSE) +
    scale_fill_gradientn(colors = cc_scale, limits = c(0,70), na.value="#ebe9eb") + # max of color bar so it saturates
    labs(fill = "CC (%)") +
    theme(panel.border = element_blank(),
          axis.text.x = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          legend.position = 'none',
          plot.margin = unit(c(0,0,0,0), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(fill = guide_colorbar(direction = "vertical",
                                 label.position = 'right',
                                 title.position ='top',
                                 title.hjust = .5,
                                 barwidth = 1,
                                 barheight = 10.9,
                                 frame.colour = "black", 
                                 ticks.colour = "black")) 
  
  return(cc_plot)
}

## kern
kern_dem_plot <-plot_dem_nl(kern_df, kern_sf)
kern_cc_plot <-plot_cc_nl(kern_df, kern_sf)
kern_ez_plot <-plot_ez_nl(kern_df, kern_sf)
kern_cow <-ggarrange(kern_dem_plot, kern_cc_plot, kern_ez_plot,
                     labels = c("(b) Kern"),
                     align = "h", # Align them both, horizontal and vertical
                     ncol = 3,
                     vjust =  2.7,
                     hjust = .3,
                     widths = c(1, 1, 1),
                     font.label = list(size = 18, color = "black", face = "bold"))  
## yuba
yuba_dem_plot <-plot_dem_nl(yuba_df, yuba_sf)
yuba_cc_plot <-plot_cc_nl(yuba_df, yuba_sf)
yuba_ez_plot <-plot_ez_nl(yuba_df, yuba_sf)
yuba_cow <-ggarrange(yuba_dem_plot, yuba_cc_plot, yuba_ez_plot,
                     labels = c("(c) Yuba"),
                     align = "h", # Align them both, horizontal and vertical
                     ncol = 3,
                     vjust =  2.7,
                     hjust = -.5,
                     widths = c(1, 1, 1),
                     font.label = list(size = 18, color = "black", face = "bold"))  

## usj
usj_dem_plot <-plot_dem(usj_df, usj_sf)
usj_cc_plot <-plot_cc(usj_df, usj_sf)
usj_ez_plot <-plot_ez(usj_df, usj_sf)
usj_cow <-ggarrange(usj_dem_plot, usj_cc_plot, usj_ez_plot,
                     labels = c("(d) USJ"),
                     align = "h", # Align them both, horizontal and vertical
                     ncol = 3,
                     vjust =  2.7,
                     hjust = -.5,
                     widths = c(1, 1, 1),
                     font.label = list(size = 18, color = "black", face = "bold"))  

## ca inset plot
ca_plot <-ggplot(dem_full_df) +
    geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .1, inherit.aes = FALSE) +
    geom_sf(data = ca, fill = NA, color = "black", linewidth = .1) +
    geom_raster(mapping = aes(x,y, fill = dem, alpha = .5), inherit.aes = FALSE) +
    geom_sf(data = kern_sf, fill = NA, color = "red", linewidth = .5) +
    geom_sf(data = yuba_sf, fill = NA, color = "orange", linewidth = .5) +
    geom_sf(data = usj_sf, fill = NA, color = "purple", linewidth = .5) +
    geom_text(x = -122.8, y = 41.8, label = "(a)", color = "black", size = 6, fontface = "bold")+
    coord_sf(label_graticule = "NW") +
    ylim(limits = c(35.4219961410279, 41.858638835292)) +
    scale_x_continuous(breaks = c(-122,-120,-118), limits = c(-123.065041220838, -117.651990878793), position = 'top') +
    scale_fill_gradientn(colors = topo_colors, limits = c(1500,4000), na.value="#ebe9eb") + # max of color bar so it saturates
    theme(panel.border = element_rect(fill = NA, color = "black"),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          legend.position = 'none',
          plot.margin = unit(c(0,0,0,0), "cm"),
          legend.box.spacing = unit(0, "pt")) 

# full
kern_and_inset <-ggarrange(ca_plot, kern_cow,
                           ncol = 2,
                           widths  = c(.55,1)) 

full_cow <-ggarrange(kern_and_inset,yuba_cow,usj_cow,
                     nrow = 3,
                     heights = c(1.4,.9,1.4))  
ggsave(full_cow,
       file = "./plots/kuy_study_area_v2.png",
       width = 11.5, 
       height = 11.5,
       dpi = 300)

system("open ./plots/kuy_study_area_v2.png")
