### plot study area static metrics
# dem, canopy cover

# jack tarricone

library(terra)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)
library(scales)

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
cc_v1 <-rast("./rasters/nlcd_cc/cc_w0.tif")
ez_bins_v1 <-rast("./rasters/categorized/dem_6zb.tif")
ez_bins <-ifel(6 == ez_bins_v1, 5, ez_bins_v1)
aspect <-rast("./rasters/categorized/aspect_thres_4_classes.tif")

# stack
stack <-c(dem_v1,cc_v1,ez_bins,aspect)

# crop and mask
kern_stack <-mask(crop(stack,ext(kern_v)), kern_v)
usj_stack <-mask(crop(stack,ext(usj_v)), usj_v)
yuba_stack <-mask(crop(stack,ext(yuba_v)), yuba_v)

# convert to df for geom_raster
kern_df <-as.data.frame(kern_stack, xy = TRUE, cells = TRUE)
usj_df <-as.data.frame(usj_stack, xy = TRUE, cells = TRUE)
yuba_df <-as.data.frame(yuba_stack, xy = TRUE, cells = TRUE)


# make cat variables
ez_df$bin_name <-ifelse(ez_df$SNSR_DEM == 1, "1500-1900 m", ez_df$SNSR_DEM)
ez_df$bin_name <-ifelse(ez_df$SNSR_DEM == 2, "1900-2300 m", ez_df$bin_name)
ez_df$bin_name <-ifelse(ez_df$SNSR_DEM == 3, "2300-2700 m", ez_df$bin_name)
ez_df$bin_name <-ifelse(ez_df$SNSR_DEM == 4, "2700-3100 m", ez_df$bin_name)
ez_df$bin_name <-ifelse(ez_df$SNSR_DEM == 5, "3100-4361 m", ez_df$bin_name)
ez_df$bin_name <-ifelse(ez_df$SNSR_DEM == 6, "3100-4361 m", ez_df$bin_name)

aspect_df$aspect_name <-ifelse(aspect_df$aspect == 1, "North", aspect_df$aspect)
aspect_df$aspect_name <-ifelse(aspect_df$aspect == 2, "East", aspect_df$aspect_name)
aspect_df$aspect_name <-ifelse(aspect_df$aspect == 3, "South", aspect_df$aspect_name)
aspect_df$aspect_name <-ifelse(aspect_df$aspect == 4, "West", aspect_df$aspect_name)



head(ez_df)
unique(ez_df$cat)

######################
######################
######## topo ########
######################
######################

# set scale
topo_table <-read.csv("./gis/topo_colors.csv")
topo_colors <-c(topo_table$colors)

# plot
dem_plot <-ggplot(dem_df) +
       geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .1, inherit.aes = FALSE) +
       geom_tile(mapping = aes(x,y, fill = SNSR_DEM)) +
       geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .3, inherit.aes = FALSE) +
       coord_sf(label_graticule = "NW") +
       scale_x_continuous(breaks = c(-122,-120,-118), position = 'top') +
       scale_fill_gradientn(colors = topo_colors, limits = c(1500,3800), na.value="#ebe9eb") + # max of color bar so it saturates
       labs(fill = "Elevation (m)") +
       theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
             axis.text.x =element_text(color="black"),
             axis.title.y = element_blank(),
             axis.title.x = element_blank(),
             axis.text.y = element_text(color="black"),
             legend.position = "bottom",
             plot.margin = unit(c(0,0,0,0), "cm"),
             legend.box.spacing = unit(0, "pt")) +
       guides(fill = guide_colorbar(direction = "horizontal",
                                    label.position = 'top',
                                    title.position ='bottom',
                                    title.hjust = .5,
                                    barwidth = 18,
                                    barheight = 1,
                                    frame.colour = "black", 
                                    ticks.colour = "black")) 
# save
ggsave(dem_plot,
       file = "./plots/dem_test_v12.png",
       width = 5.1, 
       height = 8.5,
       dpi = 600)

system("open ./plots/dem_test_v12.png")

#######################
##### cc test plot ####
#######################

# set scale 
cc_scale <-c(brewer.pal(9, 'YlGn'))

# plot
cc_plot <-ggplot(cc_df) +
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .1, inherit.aes = FALSE) + # for gray 
  geom_tile(mapping = aes(x,y, fill = nlcd_full)) +
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .3, inherit.aes = FALSE) + # for black line
  coord_sf(label_graticule = "N") +
  scale_x_continuous(breaks = c(-122,-120,-118), position = 'top') +
  scale_fill_gradientn(colors = cc_scale, limits = c(0,80), oob = squish) + # max of color bar so it saturates
  labs(fill = "Canopy Cover (%)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        axis.text.x =element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color="black"),
        legend.position = "bottom",
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.box.spacing = unit(0, "pt")) +
  guides(fill = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position ='bottom',
                               title.hjust = .5,
                               barwidth = 18,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black")) 

# save
ggsave(cc_plot,
       file = "./plots/cc_test_v9.png",
       width = 4.8, 
       height = 8.5,
       dpi = 600)

system("open ./plots/cc_test_v9.png")


#######################
#####     temp     ####
#######################

# set scale 
temp_scale <-rev(brewer.pal(9, 'Spectral'))
head(temp_df)

# plot
temp_plot <-ggplot(temp_df) +
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .1, inherit.aes = FALSE) + # for gray 
  geom_tile(mapping = aes(x,y, fill = mean)) +
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .3, inherit.aes = FALSE) + # for black line
  coord_sf(label_graticule = "N") +
  scale_x_continuous(breaks = c(-122,-120,-118), position = 'top') +
  scale_fill_gradientn(colors = temp_scale, limits = c(-6,6), oob = squish) + # max of color bar so it saturates
  labs(fill = "Mean ONDJFM Temperature (°C)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        axis.text.x =element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color="black"),
        legend.position = "bottom",
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.box.spacing = unit(0, "pt")) +
  guides(fill = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position ='bottom',
                               title.hjust = .5,
                               barwidth = 18,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black")) 

# save
ggsave(temp_plot,
       file = "./plots/temp_plot_v3.png",
       width = 4.8, 
       height = 8.5,
       dpi = 600)

system("open ./plots/temp_plot_v3.png")



#######################
#####     insol   ####
#######################

# set scale 
insol_scale <-viridis(30, option = "A")
head(insol_df)

# plot
insol_plot <-ggplot(insol_df) +
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .1, inherit.aes = FALSE) + # for gray 
  geom_tile(mapping = aes(x,y, fill = mean)) +
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # for black line
  coord_sf(label_graticule = "N") +
  scale_x_continuous(breaks = c(-122,-120,-118), position = 'top') +
  scale_fill_gradientn(colors = insol_scale, limits = c(30,260), oob = squish) + # max of color bar so it saturates
  labs(fill = expression(Mean ~ ONDJDM ~ Insolation ~ '(W m'^{"-2"} ~ ')')) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        axis.text.x =element_text(color="black"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_text(color="black"),
        legend.position = "bottom",
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.box.spacing = unit(0, "pt")) +
  guides(fill = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position ='bottom',
                               title.hjust = .5,
                               barwidth = 18,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black")) 

# save
ggsave(insol_plot,
       file = "./plots/insol_plot_v3.png",
       width = 4.8, 
       height = 8.5,
       dpi = 600)

system("open ./plots/insol_plot_v3.png")


#######################
######  aspect ########
#######################

aspect_colors <-c("palegreen", "palegreen4", "sienna1", "sienna4", "grey70", "grey30")

# plot
ez_plot <-ggplot(ez_df) +
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .1, inherit.aes = FALSE) + # for gray
  geom_tile(data = ez_df, mapping = aes(x,y, fill = cat)) +
  scale_fill_manual(# name = expression(paste("Zones")),
                    values = aspect_colors, 
                    breaks = c("EZ1_N","EZ1_S", "EZ2_N", "EZ2_S","EZ3_N", "EZ3_S")) +
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .3, inherit.aes = FALSE) + # for black line
  coord_sf(label_graticule = "N") +
  scale_x_continuous(breaks = c(-122,-120,-118), position = 'top') +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(color="black"),
        axis.text.y = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.box.spacing = unit(0, "pt")) 

# save
ggsave(ez_plot,
       file = "./plots/ez_bins_plot_v5.png",
       width = 4.8, 
       height = 8.5,
       dpi = 600)

system("open ./plots/ez_bins_plot_v5.png")


# cowplot test
full <-plot_grid(dem_plot, cc_plot, ez_plot,
                 labels = c("(a)", "(b)", "(c)"),
                 ncol = 3, 
                 align = "hv",
                 label_size = 22,
                 vjust =  3,
                 hjust = -2,
                 rel_widths = c(1/3, 1/3, 1/3))
# test save
# make tighter together
ggsave(full,
       file = "./plots/study_area_v8.png",
       width = 15.5, 
       height = 8.5,
       dpi = 600)

system("open ./plots/study_area_v8.png")
  