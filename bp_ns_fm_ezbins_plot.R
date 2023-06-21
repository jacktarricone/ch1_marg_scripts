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

theme_set(theme_classic(14))

# set working dir
setwd("~/ch1_margulis")

# # read in stack american stack
# fm_list <-list.files("./rasters/snow_metrics/fm_apr1", full.names = TRUE)
# fm_stack <-rast(fm_list[33])
# fm_stack
# 
# # read in stack american stack
# max_list <-list.files("./rasters/snow_metrics/max_swe", full.names = TRUE)
# max_stack <-rast(max_list[2])
# max_stack
# 
# # bring in ns aspect
# dem_6b <-rast("./rasters/categorized/dem_6zb.tif")
# plot(dem_6b)
# 
# # bring in ns aspect
# aspect_v1 <-rast("./rasters/categorized/aspect_thres_4_classes.tif")
# aspect_ns <-subst(aspect_v1,c(2,4),NA)
# plot(aspect_ns)
# 
# # stack with dem bins and aspect
# fm_stack <-c(dem_6b,aspect_ns,fm_stack)
# max_stack <-c(dem_6b,aspect_ns,max_stack)
# 
# # convert to df
# fm_df <-as.data.frame(fm_stack, xy = TRUE, cells = TRUE, na.rm = TRUE)
# max_df <-as.data.frame(max_stack, xy = TRUE, cells = TRUE, na.rm = TRUE)
# 
# # years seq
# years <-seq(1985,2016,1)
# 
# # rename columns, which are the annual rasters, with the correct year name
# colnames(max_df)[6:ncol(max_df)] <-years
# colnames(max_df)[3:5] <-c("y","ez","aspect")
# 
# colnames(fm_df)[6:ncol(fm_df)] <-years
# colnames(fm_df)[3:5] <-c("y","ez","aspect")
# 
# # pivot longer for test
# # creates "year" col and "fm_percent" col while preserving meta data info
# fm_long_df <-as.data.frame((fm_df) %>%
#  pivot_longer(-c(cell,x,y,ez,aspect), names_to = "year", values_to = "frac_melt"))
# 
# head(fm_long_df)
# 
# max_long_df <-as.data.frame((max_df) %>%
#                              pivot_longer(-c(cell,x,y,ez,aspect), names_to = "year", values_to = "max_swe"))
# head(max_long_df)
# 
# fwrite(fm_long_df, "./csvs/fm_boxplot_df_v2.csv")
# fwrite(max_long_df, "./csvs/max_boxplot_df_v2.csv")

##############################################
yuba_df <-fread("./csvs/gridmet_dfs/yuba_full_stats.csv_v2.csv")
usj_df <-fread("./csvs/gridmet_dfs/usj_full_stats.csv_v2.csv")
kern_df <-fread("./csvs/gridmet_dfs/kern_full_stats.csv_v2.csv")

# join
joined_df_v1 <-as.data.table(bind_rows(yuba_df, usj_df, kern_df))
joined_df <-dplyr::filter(joined_df_v1, aspect != 2 & aspect != 4)

# read back in using data.table
df <-joined_df[sample(.N, 500000)]
head(df)

# rename
df$bin_name <-ifelse(df$ez == 1, "1500-1900 m", df$ez)
df$bin_name <-ifelse(df$ez == 2, "1900-2300 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 3, "2300-2700 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 4, "2700-3100 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 5, "3100-3500 m", df$bin_name)
df$bin_name <-ifelse(df$ez == 6, "3500-4361 m", df$bin_name)

head(df)

# test hists
hist(joined_df$frac_melt, breaks = 50)
hist(df$frac_melt, breaks = 50)
mean(joined_df$frac_melt, na.rm = TRUE)
mean(df$frac_melt, na.rm = TRUE)

##################################
#### make time series box plot ###
##################################

# starting plot
fm <-ggplot(df, mapping = aes(x = as.factor(bin_name), y = frac_melt, fill = as.factor(aspect))) +
  geom_boxplot(linewidth = .3, width = .4, outlier.size = .01, outlier.shape = 1) +
  # geom_text(data = meds, aes(y = frac_med, label = round(frac_med, 2)),
  #           size = 2, vjust = -0.5, hjust = -.5) +
  scale_fill_manual(name = "Aspect",
                    values = c('1' = 'cornflowerblue', '3' = 'darkorange'),
                    labels = c('North Facing', 'South Facing'))+
  xlab(NULL) + ylab("FM") +
  scale_y_continuous(limits = c(0,1)) +
  theme_classic(11) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        plot.margin = unit(c(.25,.25,0,.25), "cm"),
        legend.position = c(.88,.86),
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        legend.margin=margin(-5,1,1,1),
        legend.box.background = element_rect(colour = "black"))

fm

# test save
ggsave(fm,
       file = "./plots/fm_ez_ns_boxplot_test_v4.png",
       width = 7, 
       height = 3,
       units = "in",
       dpi = 300) 

system("open ./plots/fm_ez_ns_boxplot_test_v4.png")

# starting plot
max_p <-ggplot(df, mapping = aes(x = as.factor(bin_name), y = mswe_mm/10, fill = interaction(basin_name,aspect))) +
  geom_boxplot(linewidth = .3, width = .4, outlier.size = .01, outlier.shape = 1, position = 'dodge') +
  # geom_text(data = meds, aes(y = frac_med, label = round(frac_med, 2)),
  #           size = 2, vjust = -0.5, hjust = -.5) +
  # scale_fill_manual(name = "Aspect and Basin Name",
  #                   values = c('kern.1' = 'cornflowerblue', 'kern.3' = 'red',
  #                              'yuba.1' = 'firebrick','yuba.3' = 'green',
  #                              'usj.1' = 'darkorange', 'usj.3' = 'violet'),
  #                   labels = c('Kern N', 'Kern S', 
  #                              'Yuba N', 'Yuba S',
  #                              'USJ N',  'USJ S')) +
  xlab("Elevation Zone") + ylab("MSWE (cm)") +
  scale_y_continuous(limits = c(0,300)) +
  theme_classic(11) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = c(.15,.8),
        plot.margin = unit(c(.25,.25,.25,.25), "cm"))

max_p

# test save
ggsave(max_p,
       file = "./plots/max_boxplot_v1.png",
       width = 6, 
       height = 3,
       units = "in",
       dpi = 300) 

system("open ./plots/max_boxplot_v1.png")

# cowplot test
cow <-plot_grid(fm, max_p,
                labels = c("(a)", "(b)"),
                nrow = 2, 
                align = "v",
                axis = "b",
                label_size = 14,
                vjust =  2.2,
                hjust = -2.8,
                rel_heights = c(.47, .53))

plot(cow)

ggsave(cow,
       file = "./plots/fm_max_boxplot_v2.png",
       width = 6, 
       height = 5,
       units = "in",
       dpi = 300) 

system("open ./plots/fm_max_boxplot_v2.png")




