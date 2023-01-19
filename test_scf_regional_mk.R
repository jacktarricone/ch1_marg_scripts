# scf regional mann-kendall test script start
# jack tarricone
# january 18 2023

library(terra)
library(tidyverse);theme_set(theme_classic(12))
library(rkt)

# set working dir
setwd("/Users/jacktarricone/ch1_margulis/")

# read in stack american stack
scf_stack <-rast("./snow_metric_rasters/terra_rasters/scf/scf_american.tif")
scf_stack

# bring in american shape file
american_v1 <-vect("./american_test/american.shp")
american <-project(american_v1, crs(scf_stack))

# test plot
plot(scf_stack[[6]])
plot(american, add = TRUE)

# bring in aspect raster
aspect_v1 <-rast("/Users/jacktarricone/ch1_margulis/static/rasters/SNSR_aspect.tif")
aspect <-project(aspect_v1, crs(scf_stack))

# bin into 4 categories north, south, east, west
aspect_classes <-matrix(c(315,360,1, 00,45,1, # 1 = north
                          135,225,2,          # 2 = south
                          45,135,3,           # 3 = east 
                          225,315,4),         # 4 = west
                        ncol=3, byrow=TRUE)

aspect_classes # inspect

# classify using matrix
aspect_cat <-classify(aspect, rcl = aspect_classes)
# writeRaster(aspect_cat, "./static/aspect_cat.tif")

##### format for analysis
# crop and mask
aspect_cat_american_v1 <-crop(aspect_cat, american)
aspect_cat_american <-mask(aspect_cat_american_v1, american)
# writeRaster(aspect_cat_american)

# convert to df
scf_df <-as.data.frame(scf_stack, xy = TRUE, cells = TRUE, na.rm = TRUE)
ac_df <-as.data.frame(aspect_cat_american, xy = TRUE, cells = TRUE, na.rm = TRUE)

# filter down to same cell numbers for each df
scf_filt <-subset(scf_df, cell %in% ac_df$cell)
ac_filt <-subset(ac_df, cell %in% scf_df$cell)

# check if the same
identical(scf_filt$cell, ac_filt$cell) # yes

# years seq
years <-seq(1985,2016,1)

# rename columns, which are the annual rasters, with the correct year name
colnames(scf_filt)[4:ncol(scf_filt)] <-years
tail(scf_filt)
tail(ac_filt)

# join ascpect categorical and scf dataframes
joined <-right_join(ac_filt,scf_filt,by = c("cell","x","y"))
joined$SNSR_aspect <-ac_filt$SNSR_aspect # fix aspect col
head(joined)

# pivot longer for test
# creates "year" col and "scf_percent" col while preserving meta data info
mk_test_df <-as.data.frame(joined %>%
                             pivot_longer(-c(cell,x,y,SNSR_aspect), names_to = "year", values_to = "scf_percent"))

# convert to int for test
mk_test_df$year <-as.integer(mk_test_df$year)
mk_df <-mk_test_df[order(mk_test_df$year),] # sort by year
head(mk_df) # looks good!

# crop to just north facing slopes
north_crop <-filter(mk_df, SNSR_aspect == 1)
tail(north_crop)

# using rkt package run regional kendall by aspect category
north_rkt_results <-rkt(north_crop$year,         # time vector of years
                        north_crop$scf_percent,  # scf_percent data 
                        north_crop$SNSR_aspect,
                        correct = TRUE,
                        rep = "m") # block aka aspect (numbers 1:4)

print(north_results)

# crop to just north facing slopes
south_crop <-filter(mk_df, SNSR_aspect == 1)
tail(south_crop)

# using rkt package run regional kendall by aspect category
south_rkt_results <-rkt(south_crop$year,         # time vector of years
                         south_crop$scf_percent,  # scf_percent data 
                         south_crop$SNSR_aspect,
                         correct = TRUE,
                         rep = "m") # block aka aspect (numbers 1:4)

print(south_rkt_results)
















#######################
###### brain storm code
#################################

# cell_numbers <-mk_df$cell[1:800]
# mk_crop <-filter(mk_df, cell %in% cell_numbers) # filter for certain cells


### test plot
# one cell
north <-filter(mk_df, SNSR_aspect == 1)

# quick test plot
library(scattermore)
ggplot(north, aes(x = year, y = scf_percent)) +
  geom_scattermore()

data(pie1)
pie1
ex<-rkt(pie1$Year,pie1$SO4,pie1$Month, correct = TRUE)
print(ex)
