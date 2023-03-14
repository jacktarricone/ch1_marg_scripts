### plot average snow metric rasters
# jack tarricone

library(terra)
library(RColorBrewer)
library(sf)

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

theme_set(theme_classic(14))

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
max <-rast('./snow_metric_rasters/terra_rasters/averages/max_mean.tif')
sdd <-rast('./snow_metric_rasters/terra_rasters/averages/sdd_mean.tif')

# quick plot
plot(mwa)
plot(snsr, add = TRUE, lwd = .15)

# quick hist s
hist(mwa, breaks = 200)

# convert to df for geom_raster
mwa_df <-as.data.frame(mwa, xy = TRUE, cells = TRUE)
head(mwa_df)


######################
######################
######## mwa #########
######################
######################

# set scale 
display.brewer.all()
mwa_scale <-brewer.pal(9, 'YlOrRd')


mwa_plot <-ggplot(mwa_df) +
       geom_tile(mapping = aes(x,y, fill = mean)) +
       geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
       scale_fill_gradientn(colors = scale, limits = c(0,200), na.value="transparent") +
       labs(fill = "MWA (mm)") +
       theme(panel.border = element_rect(color = NA, fill=NA),
             axis.title.y = element_blank(),
             axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank(),
             legend.position = "bottom") +
       guides(fill = guide_colorbar(direction = "horizontal",
                                    barwidth = 16,
                                    barheight = 1))
       
print(mwa_plot)

setwd("./plots/")
ggsave(p1,
       file = "./plots/mwa_test_v6.png",
       width = 5.5, 
       height = 9,
       dpi = 600)

ggsave(p1,
       file = "mwa_test_v6.pdf",
       width = 5.5, 
       height = 9)

system("open ./plots/mwa_test_v6.png")
  