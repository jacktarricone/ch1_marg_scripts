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

# bring in ns aspect
dem_6b <-rast("./rasters/categorized/dem_6zb.tif")
plot(dem_6b)

# bring in ns aspect
aspect_v1 <-rast("./rasters/categorized/aspect_thres_4_classes.tif")
aspect_ns <-subst(aspect_v1,c(2,4),NA)
plot(aspect_ns)

# test plot
plot(fm_stack[[6]])

# stack with dem bins and aspect
stack <-c(dem_6b,aspect_ns,fm_stack)

# convert to df
df <-as.data.frame(stack, xy = TRUE, cells = TRUE, na.rm = TRUE)
head(df)

# years seq
years <-seq(1985,2016,1)

# rename columns, which are the annual rasters, with the correct year name
colnames(df)[6:ncol(df)] <-years
colnames(df)[4] <-"ele_bin"

# join ascpect categorical and fm dataframes
fm_joined <-right_join(fm_ac_filt,fm_filt,by = c("cell","x","y"))
fm_joined$SNSR_aspect <-fm_ac_filt$SNSR_aspect # fix aspect col
head(fm_joined)

# pivot longer for test
# creates "year" col and "fm_percent" col while preserving meta data info
fm_mk_test_df <-as.data.frame(fm_joined %>%
 pivot_longer(-c(cell,x,y,SNSR_aspect), names_to = "year", values_to = "fm_percent"))

# convert to int for test
fm_mk_test_df$year <-as.integer(fm_mk_test_df$year)
fm_mk_test_df$fm_percent_100 <-fm_mk_test_df$fm_percent*100
fm_mk_df <-fm_mk_test_df[order(fm_mk_test_df$year),] # sort by year
head(fm_mk_df) # looks good!

##################################
#### make time series box plot ###
##################################

# starting plot
fm <-ggplot(fm_mk_df, mapping = aes(x = as.factor(year), y = fm_percent_100, fill = as.factor(SNSR_aspect))) +
  geom_boxplot(linewidth = .5, width = .4, outlier.size = .01, outlier.shape = 1) +
  scale_fill_manual(name = "Aspect",
                    values = c('1' = 'goldenrod', '2' = 'cornflowerblue'),
                    labels = c('North Facing', 'South Facing'))+
  xlab("Year") + ylab("fm (%)") +
  scale_y_continuous(limits = c(0,100)) +
  theme_classic(11) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size = 1),
        axis.text.x = element_text(angle = 75, hjust = 1),
        legend.position = "none")

print(fm)

# stack plots
plot_grid(fm, max,
          labels = c("(a)","(b)","(c)","(d)"),
          align = "v", 
          nrow = 2, 
          rel_heights = c(1/2, 1/2),
          label_y = .92)

ggsave("./plots/fm_max_boxplot_test.pdf",
       width = 9, 
       height = 6,
       units = "in",
       dpi = 500)

ggsave("./plots/fm_boxplot_test_v2.png",
       width = 9, 
       height = 6,
       units = "in",
       dpi = 500)

??ggplot2::geom_boxplot

