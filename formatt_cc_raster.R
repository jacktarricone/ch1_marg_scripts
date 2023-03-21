### plot average snow metric rasters
# jack tarricone

library(terra)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)

#set working directory
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

theme_set(theme_classic(17))

## bring in SNSR shape file
# with terra
snsr <-vect("./vectors/snsr_shp.gpkg")

# with sf
snsr_v1 <-st_read("./vectors/snsr_shp.gpkg")
snsr_sf <-st_geometry(snsr_v1)

## dem
dem <-rast("./rasters/static/SNSR_DEM.tif")

## bring in raw cc
cc_v1 <-rast("./rasters/nlcd_cc/nlcd_full.tif")
cc_v1
plot(cc_v1)
hist(cc_v1)

# set rough crop
rough_crop <-ext(-2493045, -1893045, 177285, 2410005)

## crop
cc_v2 <-crop(cc_v1, rough_crop)
plot(cc_v2)

### reproj
cc_v3 <-project(cc_v2, crs(dem), method = 'bilinear')
cc_v3
plot(cc_v3)

# resample
cc_v4 <-resample(cc_v3, dem)
cc_v4
plot(cc_v4)

# mask
cc_v5 <-mask(cc_v4, snsr)
plot(cc_v5)
# riteRaster(cc_v5, "./rasters/nlcd_cc/cc_w0.tif")

# 0 to NaN
cc_v6 <-subst(cc_v5, 0, NA)
cc_v6
# writeRaster(cc_v6, "./rasters/nlcd_cc/cc_wNA.tif")

#######################
##### cc test plot ####
#######################

# convert to df for geom_raster
cc_df <-as.data.frame(cc_v5, xy = TRUE, cells = TRUE)
head(cc_df)

# set scale 
display.brewer.all()
cc_scale <-brewer.pal(9, 'Greens')

# plot
cc_plot <-ggplot(cc_df) +
       geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
       geom_tile(mapping = aes(x,y, fill = nlcd_full)) +
       scale_fill_gradientn(colors = cc_scale, limits = c(0,100), na.value='white') + # max of color bar so it saturates
       scale_x_continuous(expand = c(0, 0)) +
       scale_y_continuous(expand = c(0, 0)) +
       labs(fill = "Canopy Cover (%)") +
       theme(panel.border = element_rect(color = NA, fill=NA),
             axis.title.y = element_blank(),
             axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank(),
             legend.position = "bottom",
             plot.margin = unit(c(0,0,0,0), "cm"),
             legend.box.spacing = unit(0, "pt")) +
       guides(fill = guide_colorbar(direction = "horizontal",
                                    label.position = 'top',
                                    title.position ='bottom',
                                    title.hjust = .5,
                                    barwidth = 15,
                                    barheight = 1,
                                    frame.colour = "black", 
                                    ticks.colour = "black"))

# save
ggsave(cc_plot,
       file = "./cc_test_v1.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/cc_test_v1.png")
