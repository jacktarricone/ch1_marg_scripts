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
df1 <-fread("./csvs/wa_max_all_years_v1.csv")
head(df1)

df<-as.data.frame(full_join(df1, static_summary))
head(df)

# sum by basin, aspect, bin_name, wy, and hydro_cat
df_summary <- df %>%
  group_by(basin,aspect,bin_name,ez,wy,hydro_cat) %>%
  summarise(
    total_wa_swe_km3 = sum(wa_swe_m3, na.rm = TRUE)*1e-9,
    total_max_swe_km3 = sum(max_swe_m3, na.rm = TRUE)*1e-9,
    total_area_km3 = sum(total_area_km3, na.rm = TRUE)*1e-6
  )

# check
# head(df_summary)
# yuba <-filter(df_summary, basin == "Kern")
# yuba2 <-filter(yuba, bin_name == "2700-3100 m")
# yuba3 <-filter(yuba2, hydro_cat == "HW")
# yuba4 <-filter(yuba3, aspect == "3")
# 
# yuba4
# mean_wa <-as.numeric(mean(yuba4$total_wa_swe_km3))
# sd_wa <-as.numeric(sd(yuba4$total_wa_swe_km3))
# 
# hist(yuba4$total_max_swe_km3, breaks = 100)
# hist(yuba$total_wa_swe_km3, breaks = 100)

# calc mean and SD by hydro_cat
df_summary3 <- df_summary2 %>%
  group_by(basin,aspect,bin_name,ez,hydro_cat) %>%
  summarise(
    mean_wa_swe_km3 = mean(total_wa_swe_km3, na.rm = TRUE),
    mean_max_swe_km3 = mean(total_max_swe_km3, na.rm = TRUE),
    sd_wa_swe_km3 = sd(total_wa_swe_km3, na.rm = TRUE),
    sd_max_swe_km3 = sd(total_max_swe_km3, na.rm = TRUE))
  )

df_summary3


## filter for just north and south
df_summary4 <-as.data.frame(filter(df_summary3, aspect == 2 | 4))
static_filt <-as.data.frame(filter(static_summary, aspect == 2 | 4))
df_summary4 <-as.data.frame(full_join(df_summary4, static_filt))
head(df_summary4)

# add grouped col
df_summary4$aspect_basin <-paste0(df_summary4$aspect,".",df_summary3$basin)
df_summary4$aspect_basin <-factor(df_summary4$aspect_basin, levels=c('1.Kern', '3.Kern', 
                                                   '1.USJ', '3.USJ',
                                                   '1.Yuba','3.Yuba'))
# check
head(df_summary4)
unique(df_summary4$aspect_basin)

df_summary5 <- df_summary4 %>%
  group_by(basin,ez,aspect) %>%
  summarise(
    norm_max_swe = mean_max_swe_km3/total_area_km3
)

df_summary5

## add bin area rast
# list of paths to shape files
basin_paths_v1 <-list.files("./vectors/ca_basins", full.names = TRUE, pattern = "\\.gpkg$")
basin_paths <-basin_paths_v1[c(6,19,21)]
kern_shp <-vect(basin_paths[[1]])
usj_shp <-vect(basin_paths[[2]])
yuba_shp <-vect(basin_paths[[3]])

# load in pa
pa <-rast("./rasters/static/SNSR_pixel_area.tif")
names(pa) <-"area_m3"
ez <-rast("./rasters/categorized/dem_6zb.tif")
names(ez) <-"ez"
aspect <- rast("./rasters/categorized/aspect_thres_4_classes.tif")
static <-c(pa,ez,aspect)
static

# pull out bains
yuba_static <-mask(static,yuba_shp)
usj_static <-mask(static,usj_shp)
kern_static <-mask(static,kern_shp)

# make stack
static_basins <-c(kern_static,usj_static,yuba_static)

# make df
kern_df1 <-as.data.frame(kern_static, xy = T)
kern_df2 <-drop_na(kern_df1)
kern_df2$basin <-rep("Kern",nrow(kern_df2))
head(kern_df2)

usj_df1 <-as.data.frame(usj_static, xy = T)
usj_df2 <-drop_na(usj_df1)
usj_df2$basin <-rep("USJ",nrow(usj_df2))

yuba_df1 <-as.data.frame(yuba_static, xy = T)
yuba_df2 <-drop_na(yuba_df1)
yuba_df2$basin <-rep("Yuba",nrow(yuba_df2))

basin_static <-rbind(yuba_df2,usj_df2,yuba_df2)
head(basin_static)

# calc mean and SD by hydro_cat
static_summary <- basin_static %>%
  group_by(basin,aspect,ez) %>%
  summarise(
    total_area_km3 = sum(area_m3, na.rm = TRUE)*1e-6,
  )
static_summary


test <-as.data.frame(full_join(df_summary4, static_summary))
test
df_summary4


# labls
f_labels <- data.frame(
  label = c("CD", "CW" ,"HD", "HW"),
  hydro_cat = c("CD", "CW" ,"HD", "HW"),
  bin_name = c('1500-1900 m','1500-1900 m','1500-1900 m','1500-1900 m'))

# plot
vol_plot <-ggplot(df_summary4, mapping = aes(x = as.factor(bin_name), y = mean_max_swe_km3, fill = aspect_basin))+
  geom_text(data = f_labels, aes(y = .2, label = label, fill = label), size = 11) +
  facet_wrap( ~ hydro_cat, scales = "fixed", nrow = 4) +
  geom_bar(stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(position = "dodge", 
                aes(ymin = mean_max_swe_km3 - sd_max_swe_km3, ymax = mean_max_swe_km3 + sd_max_swe_km3),
                width = .8, linewidth = .3, alpha = 1) +
  scale_fill_manual(name = "Basin and Aspect",
                    values = c('1.Kern' = 'azure4', '3.Kern' = 'azure2',
                               '1.USJ' = 'tomato4', '3.USJ' = 'tomato1',
                               '1.Yuba' = 'chartreuse4', '3.Yuba' = 'lightgreen'),
                    labels = c('Kern N','Kern S',
                               'USJ N', 'USJ S',
                               'Yuba N','Yuba S')) +
  labs(x = "Elevation Bin", y = "Max SWE (km³)", fill = "Basin Name") +
  scale_y_continuous(expand = c(.01,0), limits = c(0,.28), breaks = seq(0,.25,.05))+
  guides(fill = guide_legend(ncol = 6, override.aes = list(order = c(1,2,3,4,5,6)))) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        plot.title=element_text(margin=margin(t=40,b=-30)),
        legend.position = 'top',
        legend.direction = 'horizontal',
        strip.text = element_blank(),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))

vol_plot

ggsave(vol_plot,
       file = "./plots/max_vol_hydro_cat_v6.pdf",
       width = 8, 
       height = 8,
       units = "in") 

system("open ./plots/max_vol_hydro_cat_v6.pdf")


# plot
wa_vol_plot <-ggplot(df_summary4, mapping = aes(x = as.factor(bin_name), y = mean_wa_swe_km3, fill = aspect_basin))+
  geom_text(data = f_labels, aes(y = .037, label = label, fill = label), size = 11) +
  facet_wrap( ~ hydro_cat, nrow = 4)+
  geom_bar(stat = "identity", position = "dodge", width = .8) +
  geom_errorbar(stat = "identity", position = "dodge", 
                aes(ymin = mean_wa_swe_km3 - sd_wa_swe_km3, ymax = mean_wa_swe_km3 + sd_wa_swe_km3),
                width = .8, linewidth = .3, alpha = 1) +
  scale_fill_manual(name = "Basin and Aspect",
                    values = c('1.Kern' = 'azure4', '3.Kern' = 'azure2',
                               '1.USJ' = 'tomato4', '3.USJ' = 'tomato1',
                               '1.Yuba' = 'chartreuse4', '3.Yuba' = 'lightgreen'),
                    labels = c('Kern N','Kern S',
                               'USJ N', 'USJ S',
                               'Yuba N','Yuba S')) +
  labs(x = "Elevation Bin", y = "WA (km³)", fill = "Basin Name") +
  #scale_y_continuous(expand = c(0.01, 0), limits = c(0,.05)) +
  guides(fill = guide_legend(ncol = 6, override.aes = list(order = c(1,2,3,4,5,6)))) +
  theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
        legend.position = 'top',
        legend.direction = 'horizontal',
        strip.text = element_blank(),
        legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
        plot.margin = unit(c(.25,.25, 0,.25), "cm"))

ggsave(wa_vol_plot,
       file = "./plots/wa_vol_hydro_cat_v5.pdf",
       width = 8, 
       height = 8,
       units = "in",
       dpi = 300) 

system("open ./plots/wa_vol_hydro_cat_v5.pdf")



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
