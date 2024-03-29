### testing plotting mk results max
# jack tarricone

library(terra)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)
library(scales)

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
##### max ####
##############

#### read in metrics
# trend
trend_v1 <-rast('./rasters/mk_results/old/max_dowy_slope.tif')
trend <-crop(trend_v1, ext(snsr))
plot(trend)
hist(trend, breaks = 200)

# make NA rast for plotting
trend_na_v1 <-subst(trend, NA, -999)
values(trend_na_v1)[values(trend_na_v1) > -999] = NA
trend_na <-mask(trend_na_v1, snsr)

# sig trend
sig_trend_v1 <-rast('./rasters/mk_results/old/max_dowy_sig_slope.tif')
sig_trend <-crop(sig_trend_v1, ext(snsr))
sig_trend_plot <-sig_trend

# convert for plotting
values(sig_trend_plot)[values(sig_trend_plot) < 0] = -1 # covert all sig negative values to -1 for plotting
values(sig_trend_plot)[values(sig_trend_plot) > 0] = 1 # convert all sig posative values to 1 for plotting
plot(sig_trend_plot)
hist(sig_trend_plot, breaks = 200)

# convert to df for geom_raster
trend_df <-as.data.frame(trend, xy = TRUE, cells = TRUE)
trend_na_df <-as.data.frame(trend_na, xy = TRUE, cells = TRUE)
trend_df$days_decade <-trend_df$lyr.1*10
head(trend_df)
head(trend_na_df$lyr.1)

# sig
sig_df <-as.data.frame(sig_trend, xy = TRUE, cells = TRUE)
sig_df$cat <-ifelse(sig_df$lyr.1 == -1, "Decrease", -1)
sig_df$cat <-ifelse(sig_df$lyr.1 == 1, "Increase", "Decrease")
head(sig_df)

######################
######################
######## trend #######
######################
######################

# set scale 
trend_scale <-c(rev(brewer.pal(9, "Reds")), "white" , brewer.pal(9, "Blues"))

# plot
trend_plot <-ggplot(trend_df) +
       geom_tile(mapping = aes(x,y, fill = days_decade), alpha = 1) +
       geom_tile(data = trend_na_df, mapping = aes(x,y, fill = lyr.1), color = 'grey50') +
       geom_sf(data = snsr_sf, fill = NA, color = "black", linewidth = .05, inherit.aes = FALSE) +
       geom_sf(data = snsr_basins_sf, fill = NA, color = "black",  linewidth = .2, inherit.aes = FALSE) + 
       scale_fill_gradientn(colors = trend_scale, limits = c(-10,10), oob = squish) + 
       scale_x_continuous(expand = c(0, 0)) +
       scale_y_continuous(expand = c(0, 0)) +
       labs(fill =(expression(Delta~"DOM (days/decade)"))) +
       theme(panel.border = element_rect(color = NA, fill=NA),
             axis.title.y = element_blank(),
             axis.title.x = element_blank(),
             axis.text.x = element_blank(),
             axis.text.y = element_blank(),
             axis.ticks = element_blank(),
             legend.position  = 'bottom',
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
       file = "./plots/max_dowy_trend_test_v13.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/max_dowy_trend_test_v13.png")


######################
######################
######## sig #########
######################
######################

sig_colors <-c("#CB181D", "#08306B")

# plot
sig_plot <-ggplot(sig_df) +
  geom_sf(data = snsr_sf, fill = "grey95", color = "black", linewidth = .05, inherit.aes = FALSE) +
  geom_tile(mapping = aes(x,y, fill = cat)) +
  scale_fill_manual(name = "Significant Trends",
                    values = sig_colors, 
                    breaks = c("Decrease","Increase")) +
  geom_sf(data = snsr_basins_sf, fill = NA, color = "black", linewidth = .2, inherit.aes = FALSE) + 
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
       file = "./plots/sig_max_dowy_sig_v2.png",
       width = 4.5, 
       height = 8,
       dpi = 600)

system("open ./plots/sig_max_dowy_sig_v2.png")


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
       file = "./plots/both_max_dowy_v1.png",
       width = 9, 
       height = 8,
       dpi = 600)

system("open ./plots/both_max_dowy_v1.png")
  