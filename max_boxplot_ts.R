# max box plot time series
# jack tarricone
# january 18, 2023

library(terra)
library(lubridate)
library(tidyverse)

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

# set working dir
setwd("/Users/jacktarricone/ch1_margulis/")

# read in stack american stack
max_stack <-rast("./snow_metric_rasters/terra_rasters/max_swe/max_american.tif")
max_stack

# bring in american shape file
american_v1 <-vect("./american_test/american.shp")
american <-project(american_v1, crs(max_stack))

# test plot
plot(max_stack[[6]])
plot(american, add = TRUE)

# read in north/south raster
aspect_ns <-rast("./static/aspect_cat_ns.tif")

##### format for analysis
# crop and mask
aspect_ns_american_v1 <-crop(aspect_ns, american)
aspect_ns_american <-mask(aspect_ns_american_v1, american)
plot(aspect_ns_american)

# convert to df
max_df <-as.data.frame(max_stack, xy = TRUE, cells = TRUE, na.rm = TRUE)
max_ac_df <-as.data.frame(aspect_ns_american, xy = TRUE, cells = TRUE, na.rm = TRUE)

# filter down to same cell numbers for each df
max_filt <-subset(max_df, cell %in% ac_df$cell)
max_ac_filt <-subset(max_ac_df, cell %in% max_df$cell)

# check if the same
identical(max_filt$cell, max_ac_filt$cell) # yes

# years seq
years <-seq(1985,2016,1)

# rename columns, which are the annual rasters, with the correct year name
colnames(max_filt)[4:ncol(max_filt)] <-years

# join ascpect categorical and max dataframes
max_joined <-right_join(max_ac_filt,max_filt,by = c("cell","x","y"))
max_joined$SNSR_aspect <-max_ac_filt$SNSR_aspect # fix aspect col
head(max_joined)

# pivot longer for test
# creates "year" col and "max_percent" col while preserving meta data info
max_mk_test_df <-as.data.frame(max_joined %>%
 pivot_longer(-c(cell,x,y,SNSR_aspect), names_to = "year", values_to = "max_swe_mm"))

# convert to int for test
max_mk_test_df$year <-as.integer(max_mk_test_df$year)
max_mk_test_df$max_swe_m <-max_mk_test_df$max_swe_mm * (1/1000) # add meters col
max_mk_df <-max_mk_test_df[order(max_mk_test_df$year),] # sort by year
head(max_mk_df) # looks good!

###################################
#### make time series box plot ####
###################################

# starting plot
max <-ggplot(max_mk_df, mapping = aes(x = as.factor(year), y = max_swe_m, fill = as.factor(SNSR_aspect))) +
  geom_boxplot(linewidth = .5, width = .4, outlier.size = .01, outlier.shape = 1) +
  scale_fill_manual(name = "Aspect",
                     values = c('1' = 'goldenrod', '2' = 'cornflowerblue'),
                     labels = c('North Facing', 'South Facing'))+
  xlab("Year") + ylab("Max SWE (m)") +
  scale_y_continuous(limits = c(0,3)) +
  theme_classic(11) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.text.x = element_text(angle = 75, hjust = 1),
        legend.position = "none")

# pdf
ggsave("./plots/max_boxplot_test_v2.pdf",
       width = 9, 
       height = 3,
       units = "in",
       dpi = 500)

# png
ggsave("./plots/max_boxplot_test_v2.png",
       width = 9, 
       height = 3,
       units = "in",
       dpi = 500)
