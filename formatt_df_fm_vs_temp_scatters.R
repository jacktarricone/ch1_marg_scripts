# fm box plot time series
# jack tarricone
# june 8th, 2023

library(terra)
library(lubridate)
library(tidyverse)
library(cowplot)
library(data.table)

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
setwd("~/ch1_margulis")

# read in stack american stack
fm_list <-list.files("./rasters/snow_metrics/fm_apr1", full.names = TRUE)
fm_stack <-rast(fm_list[33])
fm_stack

# read in stack american stack
temp_list <-list.files("./rasters/daymet/tmean", full.names = TRUE)
temp_stack <-rast(temp_list[1:32])
temp_stack

# bring in ns aspect
dem_6b <-rast("./rasters/categorized/dem_6zb.tif")
plot(dem_6b)

# bring in ns aspect
aspect_v1 <-rast("./rasters/categorized/aspect_thres_4_classes.tif")
aspect_ns <-subst(aspect_v1,c(2,4),NA)
plot(aspect_ns)

# test plot
plot(fm_stack[[6]])
plot(temp_stack[[6]])

# stack with dem bins and aspect
full_stack_v1 <-c(dem_6b,aspect_ns,fm_stack)
full_stack_v2 <-c(dem_6b,aspect_ns,temp_stack)

# convert to df
df_fm_v1 <-as.data.frame(full_stack_v1, xy = TRUE, cells = TRUE, na.rm = TRUE)
df_temp_v1 <-as.data.frame(full_stack_v1, xy = TRUE, cells = TRUE, na.rm = TRUE)
head(df_fm_v1)
head(df_temp_v1)

# years seq
years <-seq(1985,2016,1)

# rename columns, which are the annual rasters, with the correct year name
colnames(df)[6:ncol(df)] <-years
colnames(df)[4] <-"ele_bin"


# pivot longer for test
# creates "year" col and "fm_percent" col while preserving meta data info
long_df <-as.data.frame((df) %>%
                          pivot_longer(-c(cell,x,y,ele_bin,aspect), names_to = "year", values_to = "frac_melt"))

head(long_df)
# fwrite(long_df, "./csvs/fm_eb_ns_csv_v1.csv")