### testing plotting mk results max
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

##############
##### max ####
##############

#### read in metrics
# trend
trend_v1 <-rast('./rasters/mk_results/max_slope_full.tif')
trend <-crop(trend_v1, ext(snsr))
plot(trend)
hist(trend, breaks = 200)

# sig trend
sig_trend_v1 <-rast('./rasters/mk_results/max_sig_slope.tif')
sig_trend_v2 <-crop(sig_trend_v1, ext(snsr))
sig_trend <-subst(sig_trend_v2, 0, NA)
values(sig_trend)[values(sig_trend) < 0] = -1 # covert all sig negative values to -1 for plotting
values(sig_trend)[values(sig_trend) > 0] = 1 # convert all sig posative values to 1 for plotting
plot(sig_trend)
hist(sig_trend, breaks = 200)

# convert to df for geom_raster
trend_df <-as.data.frame(trend, xy = TRUE, cells = TRUE)

# sig
sig_df <-as.data.frame(sig_trend, xy = TRUE, cells = TRUE)
sig_df$cat <-ifelse(sig_df$max_sig_slope == -1, "Decrease", -1)
sig_df$cat <-ifelse(sig_df$max_sig_slope == 1, "Increase", "Decrease")

######################
######################
######## trend #######
######################
######################

# set scale 
display.brewer.all()
trend_scale <-brewer.pal(9, 'RdBu')

# plot
trend_plot <-ggplot(trend_df) +
       geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
       geom_tile(mapping = aes(x,y, fill = max_slope_full)) +
       geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work
       scale_fill_gradientn(colors = trend_scale, limits = c(-15,15), na.value=max(trend_scale)) + # max of color bar so it saturates
       scale_x_continuous(expand = c(0, 0)) +
       scale_y_continuous(expand = c(0, 0)) +
       labs(fill = "Max SWE (mm/yr)") +
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
ggsave(trend_plot,
       file = "./plots/max_trend_test_v3.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/max_trend_test_v3.png")


######################
######################
######## sig #########
######################
######################
sig_colors <-c("#B2182B", "#2166AC")

# plot
sig_plot <-ggplot(sig_df) +
  geom_tile(mapping = aes(x,y, fill = cat)) +
  scale_fill_manual(name = "Significant Trends",
                    values = sig_colors, 
                    breaks = c("Decrease","Increase")) +
  geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .15, inherit.aes = FALSE) + # inherit.aes makes this work+
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
        legend.box.spacing = unit(0, "pt")) 

# save
ggsave(sig_plot,
       file = "./plots/sig_max_test_v4.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/sig_max_test_v4.png")


# cowplot test
full <-plot_grid(trend_plot,sig_plot,               
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
       file = "./plots/max_trend_test_v1.png",
       width = 9, 
       height = 8,
       dpi = 600)

system("open ./plots/max_trend_test_v1.png")
  