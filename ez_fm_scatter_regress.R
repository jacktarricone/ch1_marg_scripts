# scatter plot for each of the 6 elevation zones for FM
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

# read back in using data.table
df_v1 <-fread("./csvs/fm_temp_eb_ns_csv_v6.csv")

# sample down to a milli
df <-df_v1[sample(.N, 100000)]
head(df)

# test hists
hist(df_v1$temp_c, breaks = 50)
hist(df$temp_c, breaks = 50)
mean(df_v1$temp_c)
mean(df$temp_c)

##################################
#### make time series box plot ###
##################################

bin_stats <-as.data.frame((df) %>%
  group_by(bin_name, aspect_name) %>%
  summarise(mean_frac_melt = round(mean(frac_melt),2),
            mean_temp = round(mean(temp_c),2)))

# print
bin_stats

# stat plot
ggplot(data = df, aes(y = frac_melt, x = temp_c)) +
  geom_point(color = "steelblue", size = .1, alpha = .1) +
  scale_y_continuous(limits = c(0,1))+
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))+
  facet_wrap(~ aspect_name + bin_name, ncol = 6) 


# starting plot
fm <-ggplot(long_df, mapping = aes(x = as.factor(bin_name), y = frac_melt, fill = as.factor(aspect))) +
  geom_boxplot(linewidth = .3, width = .4, outlier.size = .01, outlier.shape = 1) +
  # geom_text(data = meds, aes(y = frac_med, label = round(frac_med, 2)),
  #           size = 2, vjust = -0.5, hjust = -.5) +
  scale_fill_manual(name = "Aspect",
                    values = c('1' = 'cornflowerblue', '3' = 'darkorange'),
                    labels = c('North Facing', 'South Facing'))+
  xlab("Elevation Zone") + ylab("FM") +
  scale_y_continuous(limits = c(0,1)) +
  theme_classic(11) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = c(.88,.86),
        legend.title = element_blank(),
        legend.margin=margin(-5,1,1,1),
        legend.box.background = element_rect(colour = "black"))

# test save
ggsave(fm,
       file = "./plots/fm_ez_ns_boxplot_test_v4.png",
       width = 7, 
       height = 3,
       units = "in",
       dpi = 300) 

system("open ./plots/fm_ez_ns_boxplot_test_v4.png")



