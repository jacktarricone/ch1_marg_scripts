### plot average snow metric rasters
# jack tarricone

library(terra)
library(RColorBrewer)

#set working directory
setwd("~/ch1_margulis")

# bring in SNSR shape file
snsr <-vect("./static/polygon/SNSR_shp_fixed.shp")

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


ggplot(mwa_df) +
  geom_tile(mapping = aes(x,y, fill = mean)) +
  scale_fill_gradientn(colors = scale, limits = c(0,200), na.value="transparent") +
  labs(title = "MWA", fill = "MWA (mm)", x = "Lon (deg)", y = "Lat (deg)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth = 1)) 
  