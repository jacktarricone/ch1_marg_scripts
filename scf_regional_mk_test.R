# scf regional mann-kendall test script start
# jack tarricone
# january 18 2023

library(terra)
library(lubridate)
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
plot(aspect)
hist(aspect, breaks = 100)

# bin into 4 categories north, south, east, west
aspect_classes_4 <-matrix(c(315,360,1, 00,45,1, # 1 = north
                          135,225,2,          # 2 = south
                          45,135,3,           # 3 = east 
                          225,315,4),         # 4 = west
                          ncol=3, byrow=TRUE)

aspect_classes_2 <-matrix(c(270,360,1, 0,90,1, # 1 = north
                            90,270,2),          # 2 = south
                            ncol=3, byrow=TRUE)

aspect_classes_2

# classify using matrix
aspect_cat <-classify(aspect, rcl = aspect_classes_4)
plot(aspect_cat)
# writeRaster(aspect_cat, "./static/aspect_cat.tif")

aspect_cat_ns <-classify(aspect, rcl = aspect_classes_2)
plot(aspect_cat_ns)
hist(aspect_cat_ns)
# writeRaster(aspect_cat_ns, "./static/aspect_cat_ns.tif")

##### format for analysis
# crop and mask
aspect_cat_american_v1 <-crop(aspect_cat_ns, american)
aspect_cat_american <-mask(aspect_cat_american_v1, american)
plot(aspect_cat_american)
hist(aspect_cat_american)
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

# using rkt package run regional kendall by aspect category
# rkt_results <-system.time(rkt(mk_test_df$year, # time vector of years
#                   mk_test_df$scf_percent,      # scf_percent data 
#                   mk_test_df$SNSR_aspect))      # block aka aspect (numbers 1:4)
# 
# print(rkt_results)

#########################
#### make time series box plot
##################################

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
theme_set(theme_classic(12))

# crop to 800 cells
cell_numbers <-mk_df$cell[1:800]
mk_crop <-filter(mk_df, cell %in% cell_numbers) # filter for certain cells

# starting plot
ggplot(mk_df, mapping = aes(x = as.factor(year), y = scf_percent, color = as.factor(SNSR_aspect))) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))
  # scale_x_date(date_labels = "%Y",
  #              date_breaks = "1 year",
  #              expand = c(0,1),
  #              limits = ymd_hms(c("1985-01-01", "2016-01-01"), tz = "MST")) 
  
  # geom_boxplot(south_plot, mapping = aes(x = year_date, y = scf_percent, group = year), posistion = "dodge", alpha = .01, color = "firebrick") 
  #  geom_point(south_plot, mapping = aes(x = year, y = scf_percent), alpha = .01, color = "red")

