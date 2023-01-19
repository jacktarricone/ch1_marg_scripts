# scf box plot time series
# jack tarricone
# january 18 2023

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
scf_stack <-rast("./snow_metric_rasters/terra_rasters/scf/scf_american.tif")
scf_stack

# bring in american shape file
american_v1 <-vect("./american_test/american.shp")
american <-project(american_v1, crs(scf_stack))

# test plot
plot(scf_stack[[6]])
plot(american, add = TRUE)

# bring in ns aspect
aspect_ns <-rast("./static/aspect_cat_ns.tif")

##### format for analysis
# crop and mask
aspect_ns_american_v1 <-crop(aspect_ns, american)
aspect_ns_american <-mask(aspect_ns_american_v1, american)
# writeRaster(aspect_cat_american)

# convert to df
scf_df <-as.data.frame(scf_stack, xy = TRUE, cells = TRUE, na.rm = TRUE)
ac_df <-as.data.frame(aspect_ns_american, xy = TRUE, cells = TRUE, na.rm = TRUE)

# filter down to same cell numbers for each df
scf_filt <-subset(scf_df, cell %in% ac_df$cell)
scf_ac_filt <-subset(ac_df, cell %in% scf_df$cell)

# check if the same
identical(scf_filt$cell, scf_ac_filt$cell) # yes

# years seq
years <-seq(1985,2016,1)

# rename columns, which are the annual rasters, with the correct year name
colnames(scf_filt)[4:ncol(scf_filt)] <-years

# join ascpect categorical and scf dataframes
scf_joined <-right_join(scf_ac_filt,scf_filt,by = c("cell","x","y"))
scf_joined$SNSR_aspect <-scf_ac_filt$SNSR_aspect # fix aspect col
head(scf_joined)

# pivot longer for test
# creates "year" col and "scf_percent" col while preserving meta data info
scf_mk_test_df <-as.data.frame(scf_joined %>%
 pivot_longer(-c(cell,x,y,SNSR_aspect), names_to = "year", values_to = "scf_percent"))

# convert to int for test
scf_mk_test_df$year <-as.integer(scf_mk_test_df$year)
scf_mk_test_df$scf_percent_100 <-scf_mk_test_df$scf_percent*100
scf_mk_df <-scf_mk_test_df[order(scf_mk_test_df$year),] # sort by year
head(scf_mk_df) # looks good!

##################################
#### make time series box plot ###
##################################

# starting plot
ggplot(scf_mk_df, mapping = aes(x = as.factor(year), y = scf_percent_100, fill = as.factor(SNSR_aspect))) +
  geom_boxplot(linewidth = .5, width = .4, outlier.size = .01, outlier.shape = 1) +
  scale_fill_manual(name = "Aspect",
                    values = c('1' = 'goldenrod', '2' = 'cornflowerblue'),
                    labels = c('North Facing', 'South Facing'))+
  xlab("Year") + ylab("SCF (%)") +
  scale_y_continuous(limits = c(0,100)) +
  theme_classic(11) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.text.x = element_text(angle = 75, hjust = 1),
        legend.position = "none")

ggsave("./plots/scf_boxplot_test_v2.pdf",
       width = 9, 
       height = 3,
       units = "in",
       dpi = 500)

ggsave("./plots/scf_boxplot_test_v2.pnd",
       width = 9, 
       height = 3,
       units = "in",
       dpi = 500)

??ggplot2::geom_boxplot

