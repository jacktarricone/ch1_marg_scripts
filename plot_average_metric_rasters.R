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
##### fm ####
##############

#### read in metrics

# wa
wa_v1 <-rast('./rasters/snow_metric_averages/wa_mean_mm.tif')
wa <-crop(wa_v1, ext(snsr))

# fwa
fwa_v1 <-rast('./rasters/snow_metric_averages/fwa_mean.tif')
fwa <-crop(fwa_v1, ext(snsr))

# fm
fm_v1 <-rast('./rasters/snow_metric_averages/fm_mean_f_25mm_27obs.tif')
fm <-crop(fm_v1, ext(snsr))

# max
max_mm <-rast('./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif')
max_m_v1 <-max_mm/1000
max_m <-crop(max_m_v1, ext(snsr))

# dom
dom_v1 <-rast('./rasters/snow_metric_averages/dom_mean_f_25mm_27obs.tif')
dom <-crop(dom_v1, ext(snsr))

# create na df
# make NA rast for plotting
na_v1 <-subst(wa, NA, -999)
values(na_v1)[values(na_v1) > -999] = NA
na <-mask(na_v1, snsr)

# convert to df for geom_raster
# fm_df <-as.data.frame(fm_cm, xy = TRUE, cells = TRUE)
wa_df <-as.data.frame(wa, xy = TRUE, cells = TRUE)
fwa_df <-as.data.frame(fwa, xy = TRUE, cells = TRUE)
max_df <-as.data.frame(max_m, xy = TRUE, cells = TRUE)
fm_df <-as.data.frame(fm, xy = TRUE, cells = TRUE)
dom_df <-as.data.frame(dom, xy = TRUE, cells = TRUE)
na_df <-as.data.frame(na, xy = TRUE, cells = TRUE)


######################
######################
######## wa #########
######################
######################

# set scale 
wa_scale <-c(viridis(30, option = "magma", direction = 1))

# plot
wa_plot <-ggplot(wa_df) +
  geom_tile(mapping = aes(x,y, fill = lyr.1)) +
  geom_tile(data = na_df, mapping = aes(x,y, fill = lyr.1), color = 'grey50') + # plot nan points as gray
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .05, inherit.aes = FALSE) + # 
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE) + # inherit.aes makes this work
  scale_fill_gradientn(colors = wa_scale, limits = c(0,150), oob = squish) + # wa of color bar so it saturates
  labs(fill = "WA (mm)") +
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
ggsave(wa_plot,
       file = "./plots/wa_snsr_v2.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/wa_snsr_v2.png")

######################
######################
######## fwa #########
######################
######################

# set scale 
fwa_scale <-c(viridis(30, option = "viridis", direction = 1))

# viridis# plot
fwa_plot <-ggplot(fwa_df) +
  geom_tile(mapping = aes(x,y, fill = lyr.1)) +
  geom_tile(data = na_df, mapping = aes(x,y, fill = lyr.1), color = 'grey50') + # plot nan points as gray
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .05, inherit.aes = FALSE) + # 
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE) + # inherit.aes makes this work
  scale_fill_gradientn(colors = fwa_scale, limits = c(0,.6), oob = squish) + # fwa of color bar so it saturates
  labs(fill = "FWA (-)") +
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
ggsave(fwa_plot,
       file = "./plots/fwa_snsr_v2.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/fwa_snsr_v2.png")



######################
######################
######## fm #########
######################
######################

# set scale 
fm_scale <-c(viridis(30, option = "C", direction = 1))

# plot
fm_plot <-ggplot(fm_df) +
  geom_tile(mapping = aes(x,y, fill = lyr.1)) +
  geom_tile(data = na_df, mapping = aes(x,y, fill = lyr.1), color = 'grey50') + # plot nan points as gray
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .05, inherit.aes = FALSE) + # 
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE) + # inherit.aes makes this work
  scale_fill_gradientn(colors = fm_scale, limits = c(0,1), oob = squish) + # fm of color bar so it saturates
  labs(fill = "FM") +
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
ggsave(fm_plot,
       file = "./plots/fm_snsr_v1.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/fm_snsr_v1.png")
hist(fm, breaks = 100)

######################
######################
######## max #########
######################
######################


# set scale 
max_scale <-brewer.pal(9, 'YlGnBu')

# plot
max_plot <-ggplot(max_df) +
  geom_tile(mapping = aes(x,y, fill = lyr.1)) +
  geom_tile(data = na_df, mapping = aes(x,y, fill = lyr.1), color = 'grey50') +
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
#####    dom.  #######
######################
######################

head(dom_df)
dom_df$apr1_anom <-dom_df$lyr.1-181
hist(dom_df$apr1_anom, breaks = 100)

# set scale 
trend_scale <-c(rev(brewer.pal(9, "Reds")), "white" , brewer.pal(9, "Blues"))
trend_scale

# plot
dom_plot <-ggplot(dom_df) +
  geom_tile(mapping = aes(x,y, fill = apr1_anom)) +
  geom_tile(data = na_df, mapping = aes(x,y, fill = lyr.1), color = 'grey50') +
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .05, inherit.aes = FALSE) + # for gray
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE) + # inherit.aes makes this work
  scale_fill_gradientn(colors = trend_scale, limits = c(-50,50), oob = squish) + # max of color bar so it saturates
  labs(fill = "DOM relative to April 1 (days)") +
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
ggsave(dom_plot,
       file = "./plots/dom_test_v9.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/dom_test_v9.png")



# cowplot test
full <-plot_grid(max_plot, dom_plot, fm_plot,                 
                 labels = c("(a)", "(b)", "(c)"),
                 ncol = 3,
                 nrow = 1,
                 align = "hv",
                 label_size = 22,
                 vjust =  2,
                 hjust = -.2,
                 rel_widths = c(1/3, 1/3, 1/3))
# test save
# make tighter together
ggsave(full,
       file = "./plots/max_dom_fm_v1.png",
       width = 13.5, 
       height = 8,
       dpi = 600)

system("open ./plots/max_dom_fm_v1.png")
  