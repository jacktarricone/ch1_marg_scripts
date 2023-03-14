### plot average snow metric rasters
# jack tarricone

library(terra)
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

#### read in metrics
# mwa
mwa_mm <-rast('./snow_metric_rasters/terra_rasters/averages/mwa_mean.tif')
mwa_cm <-mwa_mm/10 
hist(mwa_cm, breaks = 200)

# max
max_mm <-rast('./snow_metric_rasters/terra_rasters/averages/max_mean.tif')
max_m <-max_mm/1000
hist(max_m, breaks = 200)

sdd <-rast('./snow_metric_rasters/terra_rasters/averages/sdd_mean.tif')
hist(sdd, breaks = 200)

# quick plot
plot(mwa)
plot(snsr, add = TRUE, lwd = .15)

# quick hist s
hist(mwa, breaks = 200)

# convert to df for geom_raster
mwa_df <-as.data.frame(mwa_cm, xy = TRUE, cells = TRUE)
max_df <-as.data.frame(max_m, xy = TRUE, cells = TRUE)
sdd_df <-as.data.frame(sdd, xy = TRUE, cells = TRUE)


######################
######################
######## mwa #########
######################
######################

# set scale 
display.brewer.all()
mwa_scale <-brewer.pal(9, 'YlOrRd')

# plot
mwa_plot <-ggplot(mwa_df) +
       geom_tile(mapping = aes(x,y, fill = mean)) +
       geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
       scale_fill_gradientn(colors = scale, limits = c(0,30), na.value="gray40") +
       labs(fill = "MWA (cm)") +
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
       
# save
ggsave(mwa_plot,
       file = "./plots/mwa_test_v9.png",
       width = 5.5, 
       height = 9,
       dpi = 600)

system("open ./plots/mwa_test_v9.png")


######################
######################
######## max #########
######################
######################

# set scale 
display.brewer.all()
max_scale <-brewer.pal(9, 'Blues')

# plot
max_plot <-ggplot(max_df) +
  geom_tile(mapping = aes(x,y, fill = mean)) +
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
  scale_fill_gradientn(colors = max_scale, limits = c(0,1.5), na.value="gray40") +
  labs(fill = "Max SWE (m)") +
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

# save
ggsave(max_plot,
       file = "./plots/max_test_v3.png",
       width = 5.5, 
       height = 9,
       dpi = 600)

system("open ./plots/max_test_v3.png")

######################
######################
######## sdd #########
######################
######################

# set scale 
display.brewer.all()
sdd_scale <-brewer.pal(9, 'Spectral')

# plot
sdd_plot <-ggplot(sdd_df) +
  geom_tile(mapping = aes(x,y, fill = mean)) +
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
  scale_fill_gradientn(colors = sdd_scale, limits = c(100,365), na.value="gray40") +
  labs(fill = "SDD (DOWY)") +
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

# save
ggsave(sdd_plot,
       file = "./plots/sdd_test_v3.png",
       width = 5.5, 
       height = 9,
       dpi = 600)

system("open ./plots/sdd_test_v3.png")

  