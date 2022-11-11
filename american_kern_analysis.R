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

## scf rasters
scf_sig <-rast("./snow_metric_rasters/old_rasters_dont_use/scf_old/mk_results/sig_slope.tif")
scf_full <-rast("./snow_metric_rasters/old_rasters_dont_use/scf_old/mk_results/scf_slope_full.tif")
values(scf_full)[values(scf_full) == 0] <- NA
values(scf_sig)[values(scf_sig) == 0] <- NA
plot(scf_sig)
plot(scf_full)
crs(scf_full) <-crs(dem)
crs(scf_sig) <-crs(dem)

# stack
static_stack <-c(dem,aspect,slope,lat,lon,scf_full,scf_sig)
plot(static_stack)

# load in american river shape file
american <-st_read("./american_test/american.shp")
american_shp <-st_geometry(american)
plot(dem)
plot(american_shp, add = TRUE)

# crop, mask, and plot again
stacic_crop <-crop(static_stack, ext(american))
static_mc <-mask(stacic_crop, american)
plot(static_mc[[7]])
plot(american_shp, add = TRUE)

# hist of american physiographic variables
hist(static_mc[[1]], breaks = 40)
hist(static_mc[[2]], breaks = 40)
hist(static_mc[[3]], breaks = 40)
hist(static_mc[[4]], breaks = 40)
hist(static_mc[[5]], breaks = 40)
hist(static_mc[[6]], breaks = 40)
hist(static_mc[[7]], breaks = 40)


dem_stats <-freq(static_mc[[1]], digits = 0)
ggplot(dem_stats) +
  geom_point(aes(x = value, y = count), size = .01)

# make 3 elevation bands
dem_1500_2000 <-static_mc[[1]]
dem_2000_2500 <-static_mc[[1]]
dem_2500_3000 <-static_mc[[1]]

## mask out elevation bands
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

#### north south aspect
aspect_n <-static_mc[[2]]
aspect_s <-static_mc[[2]]

# north facing
values(aspect_n)[values(aspect_n) < 180] <- NA
plot(aspect_n)
plot(american_shp, add = TRUE)

# north facing
values(aspect_s)[values(aspect_s) > 180] <- NA
plot(aspect_s)
plot(american_shp, add = TRUE)

# quick hist
hist(aspect_s)
hist(aspect_n)

# mask for north
dem_1500_2000_n <-mask(dem_1500_2000, aspect_n)
plot(dem_1500_2000_n)

dem_2000_2500_n <-mask(dem_2000_2500, aspect_n)
plot(dem_2000_2500_n)

dem_2500_3000_n <-mask(dem_2500_3000, aspect_n)
plot(dem_2500_3000_n)

# mask for south
dem_1500_2000_s <-mask(dem_1500_2000, aspect_s)
plot(dem_1500_2000_s)

dem_2000_2500_s <-mask(dem_2000_2500, aspect_s)
plot(dem_2000_2500_s)

dem_2500_3000_s <-mask(dem_2500_3000, aspect_s)
plot(dem_2500_3000_s)

# now mask the scf stats
# south
scf_1500_2000_s <-mask(static_mc[[7]], dem_1500_2000_s)
scf_2000_2500_s <-mask(static_mc[[7]], dem_2000_2500_s)
scf_2500_3000_s <-mask(static_mc[[7]], dem_2500_3000_s)
plot(scf_1500_2000_s)
plot(american_shp, add = TRUE)
freq(scf_1500_2000_s)

# north
scf_1500_2000_n <-mask(static_mc[[7]], dem_1500_2000_n)
scf_2000_2500_n <-mask(static_mc[[7]], dem_2000_2500_n)
scf_2500_3000_n <-mask(static_mc[[7]], dem_2500_3000_n)
plot(scf_1500_2000_n)
plot(american_shp, add = TRUE)
freq(scf_1500_2000_n)

(global(scf_1500_2000_n, mean, na.rm = TRUE))*100
(global(scf_2000_2500_n, mean, na.rm = TRUE))*100
(global(scf_2500_3000_n, mean, na.rm = TRUE))*100
(global(scf_1500_2000_s, mean, na.rm = TRUE))*100
(global(scf_2000_2500_s, mean, na.rm = TRUE))*100
(global(scf_2500_3000_s, mean, na.rm = TRUE))*100


# count pixels for north facing 
n_df <-freq(dem_1500_2000_n)
pixels_1500_2000_n <-sum(n_df$count)
pixels_1500_2000_n

n_df2 <-freq(dem_2000_2500_n)
pixels_2000_2500_n <-sum(n_df2$count)
pixels_2000_2500_n

n_df3 <-freq(dem_2500_3000_n)
pixels_2500_3000_n <-sum(n_df3$count)
pixels_2500_3000_n

# count pixels for south facing 
s_df <-freq(dem_1500_2000_s)
pixels_1500_2000_s <-sum(n_df$count)
pixels_1500_2000_s

s_df2 <-freq(dem_2000_2500_s)
pixels_2000_2500_s <-sum(s_df2$count)
pixels_2000_2500_s

s_df3 <-freq(dem_2500_3000_s)
pixels_2500_3000_s <-sum(s_df3$count)
pixels_2500_3000_s

