
library(lubridate)
library(tidyverse)
library(cowplot)
library(data.table)
library(ggsignif)


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

theme_set(theme_classic(13))

# set working dir
setwd("~/ch1_margulis")

##############################################
df <-fread("./csvs/full_la_stats_v1.csv")

# Summarize max_vol_m3 and wa_vol_m3 by bin_name and basin_name
df_summary <- df %>%
  group_by(basin_name,bin_name,aspect_name) %>%
  summarise(
    land_area_km2 = sum(area_m3, na.rm = TRUE)/10^6,
  )

# View the result
df_summary <-na.omit(df_summary)
print(df_summary)
cols <-scale_color_viridis(option = "turbo",discrete = T)

# plot
la_plot <-ggplot(df_summary, 
                 mapping = aes(x = as.factor(bin_name), y = land_area_km2, fill = aspect_name))+
  facet_wrap( ~ basin_name, nrow = 3)+
  geom_bar(stat = "identity", position = "dodge", width = .7) +
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(expand = c(.004,.004), limits = c(0,600))+
  labs(x = "Elevation Bin", y = expression(Area~(km^2)), fill = "Aspect") +
  guides(fill = guide_legend(ncol = 4, override.aes = list(order = c(1,2,3,4)))) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        axis.text.y = element_text(color = "black"),
        axis.text.x = element_text(color = "black"),
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))

ggsave(la_plot,
       file = "./plots/la_bar_graph_v1.png",
       width = 8, 
       height = 8,
       units = "in",
       dpi = 300) 

system("open ./plots/la_bar_graph_v1.png")
