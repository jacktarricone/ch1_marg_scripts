### plot average snow metric rasters
# jack tarricone

library(terra)
library(RColorBrewer)
library(sf)

#set working directory
setwd("~/ch1_margulis")

## bring in SNSR shape file
# with terra
snsr <-vect("./static/polygon/SNSR_shp_fixed.shp")

# with sf
snsr_v1 <-st_read("./static/polygon/SNSR_shp_fixed.shp")
snsr_sf <-st_geometry(snsr_v1)
plot(snsr_sf, axes = TRUE)

##############
##### mwa ####
##############

# read in
mwa <-rast('./snow_metric_rasters/terra_rasters/averages/mwa_mean.tif')

# quick plot
plot(mwa)
plot(snsr, add = TRUE, lwd = .15)

# quick hist s
hist(mwa, breaks = 200)

# convert to df for geom_raster
mwa_df <-as.data.frame(mwa, xy = TRUE, cells = TRUE)
head(mwa_df)


# set scale 
display.brewer.all()
scale <-brewer.pal(9, 'YlOrRd')


p1 <-ggplot(mwa_df) +
       geom_tile(mapping = aes(x,y, fill = mean)) +
       geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
       scale_fill_gradientn(colors = scale, limits = c(0,200), na.value="transparent",
                            guide_colorbar(title = "Z Values", 
                                           title.position = "top", 
                                           direction = "horizontal", 
                                           barwidth = 10, 
                                           barheight = 1, 
                                           label.position = "bottom",
                                           label.theme = element_text(size = 12)))
       labs(fill = "MWA (mm)", x = "Lon (deg)", y = "Lat (deg)") +
       theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1))
       
print(p1)

ggsave(p1,
       file = "./plots/mwa_test_v1.png",
       width = 7, 
       height = 9,
       dpi = 300)
  