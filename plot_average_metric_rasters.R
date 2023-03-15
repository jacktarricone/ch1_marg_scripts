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
snsr <-vect("./static/polygon/SNSR_shp_fixed.shp")

# with sf
snsr_v1 <-st_read("./static/polygon/SNSR_shp_fixed.shp")
snsr_sf <-st_geometry(snsr_v1)

##############
##### mwa ####
##############

crop_ext <-ext(-123, -117.6, 35.4, 41.8)

ext(mwa_mm)

#### read in metrics
# mwa
mwa_mm <-rast('./snow_metric_rasters/terra_rasters/averages/mwa_mean.tif')
mwa_cm_v1 <-mwa_mm/10
mwa_cm <-crop(mwa_cm_v1, crop_ext)
hist(mwa_cm, breaks = 200)

# max
max_mm <-rast('./snow_metric_rasters/terra_rasters/averages/max_mean.tif')
max_m_v1 <-max_mm/1000
max_m <-crop(max_m_v1, crop_ext)
hist(max_m, breaks = 200)

# sdd
sdd_v1 <-rast('./snow_metric_rasters/terra_rasters/averages/sdd_mean.tif')
sdd <-crop(sdd_v1, crop_ext)
hist(sdd, breaks = 200)

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
mwa_scale <-brewer.pal(9, 'YlOrRd')

# plot
mwa_plot <-ggplot(mwa_df) +
       geom_tile(mapping = aes(x,y, fill = mean)) +
       geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
       scale_fill_gradientn(colors = mwa_scale, limits = c(0,30), na.value="gray40") +
       scale_x_continuous(expand = c(0, 0)) +
       scale_y_continuous(expand = c(0, 0)) +
       labs(fill = "MWA (cm)") +
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
ggsave(mwa_plot,
       file = "./plots/mwa_test_v13.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/mwa_test_v13.png")


######################
######################
######## max #########
######################
######################

# set scale 
max_scale <-brewer.pal(9, 'Blues')

# plot
max_plot <-ggplot(max_df) +
  geom_tile(mapping = aes(x,y, fill = mean)) +
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
  scale_fill_gradientn(colors = max_scale, limits = c(0,1.5), na.value="#08306B") +
  labs(fill = "Max (m)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
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
ggsave(max_plot,
       file = "./plots/max_test_v11.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/max_test_v11.png")

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
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
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
ggsave(sdd_plot,
       file = "./plots/sdd_test_v4.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/sdd_test_v4.png")

# cowplot test
full <-plot_grid(mwa_plot, max_plot, sdd_plot,
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
       file = "./plots/full_test_v12.png",
       width = 13.5, 
       height = 8,
       dpi = 600)

system("open ./plots/full_test_v12.png")
  