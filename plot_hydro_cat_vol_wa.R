# updated snow metric box plots
# split by hydro cat means


library(terra)
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
df <-fread("./csvs/wa_max_all_years_v1.csv")
head(df)
hist(df$)

# sum by basin, aspect, bin_name, wy, and hydro_cat
df_summary <- df %>%
  group_by(basin,aspect,bin_name,wy,hydro_cat) %>%
  summarise(
    total_wa_swe_km3 = sum(wa_swe_m3, na.rm = TRUE)*1e-9,
    total_max_swe_km3 = sum(max_swe_m3, na.rm = TRUE)*1e-9,
  )

# check
head(df_summary)
yuba <-filter(df_summary, basin == "Yuba")

hist(yuba$total_max_swe_km3, breaks = 100)
hist(yuba$total_wa_swe_km3, breaks = 100)

# calc mean and SD by hydro_cat
df_summary2 <- df_summary %>%
  group_by(basin,aspect,bin_name,hydro_cat) %>%
  summarise(
    mean_wa_swe_km3 = mean(total_wa_swe_km3, na.rm = TRUE),
    mean_max_swe_km3 = mean(total_max_swe_km3, na.rm = TRUE),
    sd_wa_swe_km3 = sd(total_wa_swe_km3, na.rm = TRUE),
    sd_max_swe_km3 = sd(total_max_swe_km3, na.rm = TRUE),
  )

df_summary2

# add grouped col
df_summary2$aspect_basin <-paste0(df_summary2$aspect,".",df_summary2$basin)
# df_summary2$aspect_basin <-factor(df_summary2$aspect_basin, levels=c('1.Kern', '3.Kern', 
                                                   '1.USJ', '3.USJ',
                                                   '1.Yuba','3.Yuba'))

head(df_summary2)
unique(df_summary2$aspect_basin)

# View the result
# plotting_df <-na.omit(df_summary2)
# print(plotting_df)

hist(df_summary2$sd_max_swe_km3, breaks = 20)
hist(df_summary2$sd_wa_swe_km3, breaks = 20)

# plot
vol_plot <-ggplot(df_summary2, mapping = aes(x = as.factor(bin_name), y = mean_max_swe_km3, fill = aspect_basin))+
  facet_wrap( ~ hydro_cat, nrow = 4) +
  geom_bar(stat = "identity", position = "dodge", width = .7) +
  # scale_fill_manual(name = "Basin and Aspect",
  #                   values = c('1.Kern' = 'azure4', '3.Kern' = 'azure2', 
  #                              '1.USJ' = 'tomato4', '3.USJ' = 'tomato1',
  #                              '1.Yuba' = 'chartreuse4', '3.Yuba' = 'lightgreen'),
  #                   labels = c('Kern N','Kern S',  
  #                              'USJ N', 'USJ S',
  #                              'Yuba N','Yuba S')) +
  labs(x = "Elevation Bin", y = "Mean Max SWE (km³)", fill = "Basin Name") +
  guides(fill = guide_legend(ncol = 6, override.aes = list(order = c(1,2,3,4,5,6)))) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))

vol_plot

ggsave(vol_plot,
       file = "./plots/max_vol_hydro_cat_v3.png",
       width = 8, 
       height = 8,
       units = "in",
       dpi = 300) 

system("open ./plots/max_vol_hydro_cat_v3.png")


# plot
wa_vol_plot <-ggplot(df_summary2, mapping = aes(x = as.factor(bin_name), y = mean_wa_swe_km3, fill = aspect_basin))+
  facet_wrap( ~ hydro_cat, nrow = 4)+
  geom_bar(stat = "identity", position = "dodge", width = .7) +
  # scale_fill_manual(name = "Basin and Aspect",
  #                   values = c('1.Kern' = 'azure4', '3.Kern' = 'azure2', 
  #                              '1.USJ' = 'tomato4', '3.USJ' = 'tomato1',
  #                              '1.Yuba' = 'chartreuse4', '3.Yuba' = 'lightgreen'),
  #                   labels = c('Kern N','Kern S',  
  #                              'USJ N', 'USJ S',
  #                              'Yuba N','Yuba S')) +
  labs(x = "Elevation Bin", y = "Mean WA (km³)", fill = "Basin Name") +
  guides(fill = guide_legend(ncol = 6, override.aes = list(order = c(1,2,3,4,5,6)))) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))

ggsave(wa_vol_plot,
       file = "./plots/wa_vol_hydro_cat_v2.png",
       width = 8, 
       height = 8,
       units = "in",
       dpi = 300) 

system("open ./plots/wa_vol_hydro_cat_v2.png")



# Summarize max_vol_m3 and wa_vol_m3 by bin_name and basin_name
df_summary3 <- df %>%
  group_by(basin,bin_name,hydro_cat) %>%
  summarise(
    total_max_vol_km3 = sum(max_swe_m3, na.rm = TRUE)/10^9,
    total_wa_vol_km3 = sum(wa_swe_m3, na.rm = TRUE)/10^9
  )

head(df_summary3)

df_piv <- df_summary3 %>%
  pivot_longer(cols = c(total_max_vol_km3, total_wa_vol_km3),
               names_to = "Metric",
               values_to = "swe_km3") %>%
  mutate(Metric = recode(Metric,
                          "total_max_vol_km3" = "Max",
                          "total_wa_vol_km3" = "WA"))
df_piv
df_wa <-dplyr::filter(df_piv, Metric == "WA")


# plot
basin_wa_vol_plot <-ggplot(df_wa, 
                     mapping = aes(x = as.factor(bin_name), 
                                   y = swe_km3,
                                   fill = basin))+
  facet_wrap( ~ hydro_cat, nrow = 4)+
  geom_bar(stat = "identity", position = "dodge", width = .7) +
  scale_fill_manual(name = "Basin",
                    values = c('Kern' = 'azure4', 'USJ' = 'tomato4', 'Yuba' = 'chartreuse4'),
                    labels = c('Kern','USJ','Yuba')) +
  labs(x = "Elevation Bin", y = "Mean WA (km³)", fill = "Basin Name") +
  guides(fill = guide_legend(ncol = 3, override.aes = list(order = c(1,2,3)))) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'top',
        legend.direction = 'horizontal',
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))

basin_wa_vol_plot

ggsave(basin_wa_vol_plot,
       file = "./plots/basin_wa_vol_hydro_cat_v2.png",
       width = 8, 
       height = 8,
       units = "in",
       dpi = 300) 

system("open ./plots/basin_wa_vol_hydro_cat_v2.png")
