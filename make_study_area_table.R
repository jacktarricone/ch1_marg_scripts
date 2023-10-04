# fusion study area plot and table creation
# plot swe change sd rasters
# jack tarricone
# june 13th

library(terra)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(cowplot)
library(scales)
library(ggpubr)



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

setwd("~/ch1_margulis")

## bring in SNSR shape file
# with terra
snsr <-vect("./vectors/snsr_shp.gpkg")
snsr_basins <-vect("./vectors/ca_basins/snsr_all_basins.shp")

# with sf
snsr_v1 <-st_read("./vectors/snsr_shp.gpkg")
snsr_sf <-st_geometry(snsr_v1)
snsr_v <-vect(snsr_sf)

# ca state boundary
ca_v1 <-st_geometry(st_read("/Users/jacktarricone/ch1_jemez/vector_data/states/cb_2018_us_state_20m.shp"))
ca <-st_transform(ca_v1, "EPSG:4326")

# kern
kern_sf <-st_geometry(st_read("./vectors/ca_basins/kern.gpkg"))
kern_v <-vect(kern_sf)

# usj
usj_sf <-st_geometry(st_read("./vectors/ca_basins/usj.gpkg"))
usj_v <-vect(usj_sf)

# yuba
yuba_sf <-st_geometry(st_read("./vectors/ca_basins/yuba.gpkg"))
yuba_v <-vect(yuba_sf)

yuba_area <-expanse(yuba_v, "km")
kern_area <-expanse(kern_v, "km")

##############
##### dem ####
##############

# bring in mswe_mean
general_sa_v1 <-rast("./rasters/snow_metric_averages/max_mean_f_25mm_27obs.tif")
general_sa <-ifel(general_sa_v1 > 0, -99, general_sa_v1)
plot(general_sa)

# mask aspect
aspect_v1 <-rast("./rasters/categorized/aspect_thres_4_classes.tif")
aspect_v2 <-ifel(aspect_v1 == 4, NA, aspect_v1)
aspect_v3 <-ifel(aspect_v1 == 2, NA, aspect_v2)
aspect <-mask(aspect_v3, general_sa, maskvalue = NA)
plot(aspect)


# crop and mask
kern_sa <-mask(crop(aspect,ext(kern_v)), kern_v)
kern_north <-ifel(kern_sa == 3, NA, kern_sa)
kern_south <-ifel(kern_sa == 1, NA, kern_sa)
kern_ta <-mask(crop(general_sa,ext(kern_v)), kern_v)
k_total_area <-expanse(kern_ta, "km")
k_north_area <-expanse(kern_north, "km")
k_south_area <-expanse(kern_south, "km")

# crop and mask
usj_sa <-mask(crop(aspect,ext(usj_v)), usj_v)
usj_north <-ifel(usj_sa == 3, NA, usj_sa)
usj_south <-ifel(usj_sa == 1, NA, usj_sa)
usj_ta <-mask(crop(general_sa,ext(usj_v)), usj_v)
u_total_area <-expanse(usj_ta, "km")
u_north_area <-expanse(usj_north, "km")
u_south_area <-expanse(usj_south, "km")

# crop and mask
yuba_sa <-mask(crop(aspect,ext(yuba_v)), yuba_v)
yuba_north <-ifel(yuba_sa == 3, NA, yuba_sa)
yuba_south <-ifel(yuba_sa == 1, NA, yuba_sa)
yuba_ta <-mask(crop(general_sa,ext(yuba_v)), yuba_v)
y_total_area <-expanse(yuba_ta, "km")
y_north_area <-expanse(yuba_north, "km")
y_south_area <-expanse(yuba_south, "km")

#### read in metrics
dem_v1 <-rast('./rasters/static/SNSR_DEM.tif')
names(dem_v1) <-"dem"

cc_v1 <-rast("./rasters/nlcd_cc/cc_w0.tif")
names(cc_v1) <-"cc_percent"

# crop and mask
kern_dem <-mask(crop(dem_v1,ext(kern_v)), kern_v)
plot(kern_dem)
k_max_ele <-global(kern_dem, max, na.rm = TRUE)

# crop and mask
usj_dem <-mask(crop(dem_v1,ext(usj_v)), usj_v)
plot(usj_dem)
u_max_ele <-global(usj_dem, max, na.rm = TRUE)

# crop and mask
yuba_dem <-mask(crop(dem_v1,ext(yuba_v)), yuba_v)
plot(yuba_dem)
y_max_ele <-as.integer(global(yuba_dem, max, na.rm = TRUE))



# crop and mask
kern_cc <-mask(crop(cc_v1,ext(kern_v)), kern_v)
plot(kern_cc)
k_mean_cc <-global(kern_cc, mean, na.rm = TRUE)

# crop and mask
usj_cc <-mask(crop(cc_v1,ext(usj_v)), usj_v)
plot(usj_cc)
u_mean_cc <-global(usj_cc, mean, na.rm = TRUE)

# crop and mask
yuba_cc <-mask(crop(cc_v1,ext(yuba_v)), yuba_v)
plot(yuba_cc)
y_mean_cc <-global(yuba_cc, mean, na.rm = TRUE)



