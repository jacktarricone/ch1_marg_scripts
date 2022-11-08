# test analysis after meeting with adrian
# load in snow metric MK rast
# group the rast by physiographic variables
# 4 elevation bands, north/south

library(terra)
library(sf)
library(ggplot2)

########## static rasters
setwd("/Users/jacktarricone/ch1_margulis/")

# dem
dem <-rast("./static/rasters/SNSR_DEM.tif")
dem
plot(dem)

# aspect
aspect <-rast("./static/rasters/SNSR_aspect.tif")
aspect
plot(aspect)

# lat
lat <-rast("./static/rasters/SNSR_lat.tif")
lat
plot(lat)

# lon
lon <-rast("./static/rasters/SNSR_lon.tif")
lon
plot(lon)

# slope
slope <-rast("./static/rasters/SNSR_slope.tif")
slope
plot(slope)

# stack
static_stack <-c(dem,aspect,slope,lat,lon)

# load in american river shape file
american <-st_read("./american_test/american.shp")
american_shp <-st_geometry(american)
plot(dem)
plot(american_shp, add = TRUE)

# crop, mask, and plot again
stacic_crop <-crop(static_stack, ext(american))
static_mc <-mask(stacic_crop, american)
plot(static_mc[[2]])
plot(american_shp, add = TRUE)

# hist of american physiographic variables
hist(static_mc[[1]], breaks = 40)
hist(static_mc[[2]], breaks = 40)
hist(static_mc[[3]], breaks = 40)
hist(static_mc[[4]], breaks = 40)
hist(static_mc[[5]], breaks = 40)


dem_stats <-freq(static_mc[[1]], digits = 0)
ggplot(dem_stats) +
  geom_point(aes(x = value, y = count), size = .01)

# make 3 elevation bands
dem_1500_2000 <-static_mc[[1]]
dem_2000_2500 <-static_mc[[1]]
dem_2500_3000 <-static_mc[[1]]

## mask out the bands
# 1500-2000 m
values(dem_1500_2000)[values(dem_1500_2000) > 2000] <- NA
plot(dem_1500_2000)
plot(american_shp, add = TRUE)

# 2000-2500 m
values(dem_2000_2500)[values(dem_2000_2500) < 2000] <- NA
values(dem_2000_2500)[values(dem_2000_2500) > 2500] <- NA
plot(dem_2000_2500)
plot(american_shp, add = TRUE)

# 2000-2500 m
values(dem_2500_3000)[values(dem_2500_3000) < 2500] <- NA
plot(dem_2500_3000)
plot(american_shp, add = TRUE)
