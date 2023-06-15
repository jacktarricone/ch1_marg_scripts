# make spearman table
# jack tarricone
# june 11th, 2023

library(tidyverse)
library(data.table)
library(cowplot)
library(viridisLite)
library(scales)
library(RColorBrewer)

# set wd
setwd("~/ch1_margulis")

# set custom plot theme
theme_classic <-function(base_size = 11, base_family = "",
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

theme_set(theme_classic(15))

# read in df
df <-fread("./csvs/spearman_fm_temp_results/all_basins_spearman_results.csv")
colnames(df)[13] <-"Basin"
head(df)

# write function which calculates percentage of bin that is significant
results_v1 <-df %>%
  group_by(Basin, zone_name, aspect_name) %>%
  summarise(percent_sig     = round((length(which(p_val < .05))/length(p_val))*100, 0),
            mean_ez_temp_c  = mean(mean_temp_c))

head(results_v1)

# filter for north and south facing
north_results <-filter(results_v1, aspect_name == "North")
colnames(north_results)[4] <-"north_percent_sig"

# remove zone 4 feather, like 4 pixels
south_results_v1 <-filter(results_v1, aspect_name == "South")
south_results <-filter(south_results_v1, Basin != "Feather" | zone_name != "2700-3100 m")
colnames(south_results)[4] <-"south_percent_sig"
south_results
identical(south_results$Basin, north_results$Basin)

# calc difference
# diff <-north_results$percent_sig - south_results$percent_sig


# make differnce df
diff_results <-as.data.frame(full_join(north_results, south_results, 
                                       by = c("Basin","zone_name")))
diff_results
diff_results$diff <-diff_results$north_percent_sig - diff_results$south_percent_sig 

# # pivot wider for plotting
# test <-as.data.frame(results_v1 %>%
#   pivot_wider(names_from = zone_name, values_from = percent_sig))

# plot
mycolors <-rev(brewer.pal(9, "Spectral"))

#### north facing
north_p <-ggplot(north_results, aes(y=Basin, x=zone_name, fill= north_percent_sig)) + 
  geom_tile()+
  geom_text(aes(label=north_percent_sig)) +
  scale_fill_gradientn(colors = mycolors, limits = c(0,100), oob = squish) +
  labs(x = "EZ", fill = "Area Significant (%)", title = "North Facing") +
  scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        panel.background = element_rect(fill = 'gray'),
        aspect.ratio = 1,
        legend.position  = 'bottom',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(.25,.1,.1,.1), "cm"),
        legend.box.spacing = unit(0, "pt")) +
  guides(fill = guide_colorbar(direction = "horizontal",
                                label.position = 'top',
                                title.position = 'bottom',
                                title.hjust = .5,
                                barwidth = 27,
                                barheight = 1,
                                frame.colour = "black", 
                                ticks.colour = "black"))
north_p

ggsave(north_p,
       file = "./plots/spearman_heat_north_v2.png",
       width = 8, 
       height = 8,
       dpi = 600)

system("open ./plots/spearman_heat_north_v2.png")

#### south facing
south_p <-ggplot(south_results, aes(y=Basin, x=zone_name, fill= south_percent_sig)) + 
  geom_tile()+
  geom_text(aes(label=south_percent_sig)) +
  scale_fill_gradientn(colors = mycolors, limits = c(0,100), oob = squish) +
  labs(fill = "Area Significant (%)", title = "South Facing") +
  scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        panel.background = element_rect(fill = 'gray'),
        aspect.ratio = 1,
        legend.position  = 'bottom',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(.25,.1,.1,.1), "cm"),
        legend.box.spacing = unit(0, "pt")) +
  guides(fill = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position = 'bottom',
                               title.hjust = .5,
                               barwidth = 27,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black"))
south_p

ggsave(south_p,
       file = "./plots/spearman_heat_south_v2.png",
       width = 8, 
       height = 8,
       dpi = 600)

system("open ./plots/spearman_heat_south_v2.png")


# set scale 
diff_colors <-brewer.pal(9, "RdBu")
head(diff_results)

#### difference
diff_p <-ggplot(diff_results, aes(y=Basin, x=zone_name, fill= diff)) + 
  geom_tile()+
  geom_text(aes(label=diff)) +
  scale_fill_gradientn(colors = diff_colors, limits = c(-50,50), oob = squish) +
  labs(x = "EZ", fill = "Difference (%)", title = "North - South Difference") +
  scale_x_discrete(expand = c(0, 0))+
  scale_y_discrete(expand = c(0, 0))+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        panel.background = element_rect(fill = 'gray'),
        aspect.ratio = 1,
        legend.position  = 'bottom',
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.margin = unit(c(.25,.1,.1,.1), "cm"),
        legend.box.spacing = unit(0, "pt")) +
  guides(fill = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               title.position = 'bottom',
                               title.hjust = .5,
                               barwidth = 27,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black"))
diff_p

ggsave(diff_p,
       file = "./plots/spearman_heatmap_v3.png",
       width = 8, 
       height = 8,
       dpi = 600)

system("open ./plots/spearman_heatmap_v3.png")

### cowing
# cowplot test
full <-plot_grid(north_p, south_p, diff_p,
                 labels = c("(a)", "(b)", "(c)"),
                 ncol = 3, 
                 align = "hv",
                 label_size = 24,
                 vjust =  3,
                 hjust = -1,
                 rel_widths = c(1/3, 1/3, 1/3))
# test save
# make tighter together
ggsave(full,
       file = "./plots/heatmap_all3_v4.png",
       width = 23, 
       height = 8,
       dpi = 600)

system("open ./plots/heatmap_all3_v4.png")


