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

# bring in aspect 4
aspect_4 <-rast("./static/aspect_cat.tif")
plot(aspect_4)

# crop and mask
aspect_4_c <-crop(aspect_4, american)
aspect_am <-mask(aspect_4_c, american)
plot(aspect_am)

# convert to df
scf_df <-as.data.frame(scf_stack, xy = TRUE, cells = TRUE, na.rm = TRUE)
aspect_df <-as.data.frame(aspect_am, xy = TRUE, cells = TRUE, na.rm = TRUE)

# filter down to same cell numbers for each df
scf_filt <-subset(scf_df, cell %in% aspect_df$cell)
aspect_filt <-subset(aspect_df, cell %in% scf_df$cell)

# check if the same
identical(scf_filt$cell, aspect_filt$cell) # yes

# years seq
years <-seq(1985,2016,1)

# rename columns, which are the annual rasters, with the correct year name
colnames(scf_filt)[4:ncol(scf_filt)] <-years
tail(scf_filt)
tail(ac_filt)

# join ascpect categorical and scf dataframes
scf_joined <-right_join(aspect_filt,scf_filt,by = c("cell","x","y"))
scf_joined$SNSR_aspect <-aspect_filt$SNSR_aspect # fix aspect col
head(scf_joined)

# pivot longer for test
# creates "year" col and "scf_percent" col while preserving meta data info
scf_mk_test_df <-as.data.frame(scf_joined %>%
                             pivot_longer(-c(cell,x,y,SNSR_aspect), names_to = "year", values_to = "scf_percent"))

# convert to int for test
scf_mk_test_df$year <-as.integer(scf_mk_test_df$year)
scf_mk_df <-scf_mk_test_df[order(scf_mk_test_df$year),] # sort by year
head(scf_mk_df) # looks good!

# crop to just north facing slopes
north_crop <-filter(scf_mk_df, SNSR_aspect == 1)
south_crop <-filter(scf_mk_df, SNSR_aspect == 2)
east_crop <-filter(scf_mk_df, SNSR_aspect == 3)
west_crop <-filter(scf_mk_df, SNSR_aspect == 4)

# using rkt package run regional kendall by aspect category
north_rkt_results <-rkt(north_crop$year,         # time vector of years
                        north_crop$scf_percent,  # scf_percent data 
                        north_crop$SNSR_aspect,  # 'block' variable
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
