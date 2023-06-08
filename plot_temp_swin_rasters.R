### plot average mean temp and shortwave in rasters
# jack tarricone

library(terra)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)
library(scales)
library(viridis)

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
snsr_basins <-vect("./vectors/ca_basins/snsr_all_basins.shp")

# with sf
snsr_v1 <-st_read("./vectors/snsr_shp.gpkg")
snsr_sf <-st_geometry(snsr_v1)

# with sf
snsr_basins_v1 <-st_read("./vectors/ca_basins/snsr_all_basins.shp")
snsr_basins_sf <-st_geometry(snsr_basins_v1)

#### read in rasters
# sw
sw_v1 <-rast("./rasters/insolation/snsr_dem_insol_watts_masked_v1.tif")
sw <-crop(sw_v1, ext(snsr))
plot(sw)

# fm
temp_v2 <-rast("./rasters/daymet/tmean_normal_1985_2016.tif")
temp_v1 <-crop(temp_v2, ext(snsr))
temp <-mask(temp_v1, sw, maskvalue = NA)
plot(temp)

# na rast
# create na df
# make NA rast for plotting
na_v1 <-subst(temp, NA, -999)
values(na_v1)[values(na_v1) > -999] = NA
na <-mask(na_v1, snsr)

# convert to df for geom_raster
# fm_df <-as.data.frame(fm_cm, xy = TRUE, cells = TRUE)
sw_df <-as.data.frame(sw, xy = TRUE, cells = TRUE)
temp_df <-as.data.frame(temp, xy = TRUE, cells = TRUE)
na_df <-as.data.frame(na, xy = TRUE, cells = TRUE)

hist(sw_df$mean, breaks = 100)
max(sw_df$mean)

######################
######################
########  sw #########
######################
######################

# set scale 
sw_scale <-c(viridis(30, option = "C", direction = 1))

# plot
sw_plot <-ggplot(sw_df) +
  geom_tile(mapping = aes(x,y, fill = mean)) +
  geom_tile(data = na_df, mapping = aes(x,y, fill = mean), color = 'grey50') + # plot nan points as gray
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .05, inherit.aes = FALSE) + # 
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE) + # inherit.aes makes this work
  scale_fill_gradientn(colors = sw_scale, limits = c(40,260), oob = squish) + # fm of color bar so it saturates
  labs(fill = expression(SW["in"] ~ '(W m'^{"-2"} ~ ')')) +
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
ggsave(sw_plot,
       file = "./plots/sw_plot_v3.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/sw_plot_v3.png")

######################
######################
######## temp #########
######################
######################


# set scale 
temp_scale <-rev(brewer.pal(9, 'Spectral'))

# plot
temp_plot <-ggplot(temp_df) +
  geom_tile(mapping = aes(x,y, fill = mean)) +
  geom_tile(data = na_df, mapping = aes(x,y, fill = mean), color = 'grey50') +
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .05, inherit.aes = FALSE) + # for gray
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE) + # inherit.aes makes this work
  scale_fill_gradientn(colors = temp_scale, limits = c(-6,6), oob = squish) + # max of color bar so it saturates
  labs(fill = "Mean ONDJFM Temp (°C)") +
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
ggsave(temp_plot,
       file = "./plots/temp_plot_v3.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/temp_plot_v3.png")


######################################################
######################################################
######################################################
######################################################
# cowplot test
full <-plot_grid(sw_plot, temp_plot,                
                 labels = c("(a)", "(b)"),
                 ncol = 2,
                 nrow = 1,
                 align = "hv",
                 label_size = 22,
                 vjust =  2,
                 hjust = -.2,
                 rel_widths = c(1/2, 1/2))
# test save
# make tighter together
ggsave(full,
       file = "./plots/sw_temp_plot_v1.png",
       width = 9, 
       height = 8,
       dpi = 600)

system("open ./plots/sw_temp_plot_v1.png")
  