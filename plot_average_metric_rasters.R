### plot average snow metric rasters
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

##############
##### mwa ####
##############

#### read in metrics
# mwa
mwa_mm <-rast('./rasters/snow_metric_averages/mwa_djfm_v1.tif')
mwa_cm_v1 <-mwa_mm/10
mwa_cm <-crop(mwa_cm_v1, ext(snsr))
hist(mwa_cm, breaks = 200)

# max
max_mm <-rast('./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif')
max_m_v1 <-max_mm/1000
max_m <-crop(max_m_v1, ext(snsr))

# dom
max_dowy_v1 <-rast('./rasters/snow_metric_averages/dom_mean_f_25mm_27obs.tif')
max_dowy <-crop(max_dowy_v1, ext(snsr))
plot(max_dowy)
plot(snsr, add = TRUE)

# create na df
# make NA rast for plotting
na_v1 <-subst(max_m, NA, -999)
values(na_v1)[values(na_v1) > -999] = NA
na <-mask(na_v1, snsr)
plot(na)

# convert to df for geom_raster
# mwa_df <-as.data.frame(mwa_cm, xy = TRUE, cells = TRUE)
max_df <-as.data.frame(max_m, xy = TRUE, cells = TRUE)
mwa_df <-as.data.frame(mwa_cm, xy = TRUE, cells = TRUE)
max_dowy_df <-as.data.frame(max_dowy, xy = TRUE, cells = TRUE)
na_df <-as.data.frame(na, xy = TRUE, cells = TRUE)


######################
######################
######## mwa #########
######################
######################

# set scale 
mwa_scale <-c(viridis(30, option = "E", direction = 1))

# plot
mwa_plot <-ggplot(mwa_df) +
  geom_tile(mapping = aes(x,y, fill = lyr.1)) +
  geom_tile(data = na_df, mapping = aes(x,y, fill = lyr.1), color = 'grey50') + # plot nan points as gray
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .05, inherit.aes = FALSE) + # 
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE) + # inherit.aes makes this work
  scale_fill_gradientn(colors = mwa_scale, limits = c(0,50), oob = squish) + # mwa of color bar so it saturates
  labs(fill = "MWA (cm)") +
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
ggsave(mwa_plot,
       file = "./plots/mwa_test_v3.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/mwa_test_v3.png")

######################
######################
######## max #########
######################
######################

# set scale 
max_scale <-brewer.pal(9, 'YlGnBu')

# plot
max_plot <-ggplot(max_df) +
  geom_tile(mapping = aes(x,y, fill = mean)) +
  geom_tile(data = na_df, mapping = aes(x,y, fill = mean), color = 'grey50') +
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .05, inherit.aes = FALSE) + # for gray
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE) + # inherit.aes makes this work
  scale_fill_gradientn(colors = max_scale, limits = c(0,1.5), oob = squish) + # max of color bar so it saturates
  labs(fill = "Max SWE (m)") +
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
       file = "./plots/max_test_v17.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/max_test_v17.png")

######################
######################
######## sdd #########
######################
######################

# # set scale 
# display.brewer.all()
# sdd_scale <-brewer.pal(9, 'Spectral')
# min(sdd_scale)
# 
# # plot
# sdd_plot <-ggplot(sdd_df) +
#   geom_sf(data = snsr_sf, fill = "gray80", color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
#   geom_tile(mapping = aes(x,y, fill = mean)) +
#   geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
#   scale_fill_gradientn(colors = sdd_scale, limits = c(130,365), na.value="gray80") +
#   labs(fill = "SDD (DOWY)") +
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   theme(panel.border = element_rect(color = NA, fill=NA),
#         axis.title.y = element_blank(),
#         axis.title.x = element_blank(),
#         axis.text.x = element_blank(),
#         axis.text.y = element_blank(),
#         axis.ticks = element_blank(),
#         legend.position = "bottom",
#         plot.margin = unit(c(0,0,0,0), "cm"),
#         legend.box.spacing = unit(0, "pt")) +
#   guides(fill = guide_colorbar(direction = "horizontal",
#                                label.position = 'top',
#                                title.position ='bottom',
#                                title.hjust = .5,
#                                barwidth = 15,
#                                barheight = 1,
#                                frame.colour = "black", 
#                                ticks.colour = "black"))
# 
# # save
# ggsave(sdd_plot,
#        file = "./plots/sdd_test_v5.png",
#        width = 4.5, 
#        height = 8,
#        dpi = 600)
# 
# system("open ./plots/sdd_test_v5.png")


######################
######################
##### max_dowy #######
######################
######################

# set scale 
display.brewer.all()
max_dowy_scale <-brewer.pal(9, 'Spectral')

# plot
max_dowy_plot <-ggplot(max_dowy_df) +
  geom_tile(mapping = aes(x,y, fill = lyr.1)) +
  geom_tile(data = na_df, mapping = aes(x,y, fill = mean), color = 'grey50') +
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .05, inherit.aes = FALSE) + # for gray
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE) + # inherit.aes makes this work
  scale_fill_gradientn(colors = max_dowy_scale, limits = c(100,225), oob = squish) + # max of color bar so it saturates
  labs(fill = "DOP (DOWY)") +
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
ggsave(max_dowy_plot,
       file = "./plots/max_dowy_test_v6.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/max_dowy_test_v6.png")



# cowplot test
full <-plot_grid(max_plot, max_dowy_plot, max_dowy_plot, max_plot,                 
                 labels = c("(a)", "(b)", "(c)", "(d)"),
                 ncol = 4,
                 nrow = 1,
                 align = "hv",
                 label_size = 22,
                 vjust =  2,
                 hjust = -.2,
                 rel_widths = c(1/3, 1/3, 1/3),
                 rel_heights = c(1/2, 1/2))
# test save
# make tighter together
ggsave(full,
       file = "./plots/full_test_v16.png",
       width = 18, 
       height = 8,
       dpi = 600)

system("open ./plots/full_test_v16.png")
  