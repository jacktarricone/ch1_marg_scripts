### plot study area static metrics
# dem, canopy cover

# jack tarricone

library(terra)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)

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

setwd("~/ch1_margulis/")

## bring in SNSR shape file
# with terra
snsr <-vect("./vectors/snsr_shp.gpkg")

# with sf
snsr_v1 <-st_read("./vectors/snsr_shp.gpkg")
snsr_sf <-st_geometry(snsr_v1)


##############
##### dem ####
##############

#### read in metrics
dem_v1 <-rast('./rasters/static/SNSR_DEM.tif')
dem <-crop(dem_v1, ext(snsr))
plot(dem)

cc_v1 <-rast("./rasters/nlcd_cc/cc_wNA.tif")
cc <-crop(cc_v1, ext(snsr))
plot(cc)

# convert to df for geom_raster
dem_df <-as.data.frame(dem_v1, xy = TRUE, cells = TRUE)
cc_df <-as.data.frame(cc_v1, xy = TRUE, cells = TRUE)

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
       geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
       coord_sf(label_graticule = "NW") +
       scale_x_continuous(breaks = c(-122,-120,-118), position = 'top') +
       geom_tile(mapping = aes(x,y, fill = SNSR_DEM)) +
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
       file = "./plots/dem_test_v8.png",
       width = 5.1, 
       height = 8.5,
       dpi = 600)

system("open ./plots/dem_test_v8.png")

#######################
##### cc test plot ####
#######################

# set scale 
display.brewer.all()
cc_scale <-brewer.pal(9, 'YlGn')

# plot
cc_plot <-ggplot(cc_df) +
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
  coord_sf(label_graticule = "N") +
  scale_x_continuous(breaks = c(-122,-120,-118), position = 'top') +
  geom_tile(mapping = aes(x,y, fill = nlcd_full)) +
  scale_fill_gradientn(colors = cc_scale, limits = c(0,80), na.value="#004529") + # max of color bar so it saturates
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
       file = "./plots/cc_test_v4.png",
       width = 5.1, 
       height = 8.5,
       dpi = 600)

system("open ./plots/cc_test_v4.png")


# cowplot test
full <-plot_grid(dem_plot, cc_plot, cc_plot,
                 labels = c("(a)", "(b)", "(c)"),
                 ncol = 3, 
                 align = "hv",
                 label_size = 22,
                 vjust =  2,
                 hjust = -.2,
                 rel_widths = c(1/3, 1/3, 1/3))
# test save
# make tighter together
ggsave(full,
       file = "./plots/study_area_v2.png",
       width = 13.5, 
       height = 8,
       dpi = 600)

system("open ./plots/study_area_v2.png")
  