# rank years on basin wide max swe vs. winter temp anomly

library(terra)
library(tidyverse)

# set working dir
setwd("~/ch1_margulis")

# load in shape files
yuba <-vect('./vectors/ca_basins/yuba.gpkg')
usj <-vect('./vectors/ca_basins/usj.gpkg')
kern <-vect('./vectors/ca_basins/kern.gpkg')
snsr <-vect('./vectors/snsr_shp.gpkg')

# max stack
max_stack <-rast("./rasters/snow_metrics/max_swe/max_stack_f_25mm_27obs.tif")

# years list
names1 <-seq(1985,2016,1)
year_names <-paste0("WY",names1)

# make basin stacks
yuba_max <-crop(mask(max_stack, yuba),yuba)
names(yuba_max) <-year_names
usj_max <-crop(mask(max_stack, usj),usj)
names(usj_max) <-year_names
kern_max <-crop(mask(max_stack, kern),kern)
names(kern_max) <-year_names

# calculate time series scene wide mean
yuba_max_years <-global(yuba_max, mean, na.rm = T)
yuba_ts_mean <-mean(yuba_max_years$mean)

usj_max_years <-global(usj_max, mean, na.rm = T)
usj_ts_mean <-mean(usj_max_years$mean)

kern_max_years <-global(kern_max, mean, na.rm = T)
kern_ts_mean <-mean(kern_max_years$mean)

# calculate max anom
yuba_max_years$anom <-(yuba_max_years$mean)-yuba_ts_mean
usj_max_years$anom <-(usj_max_years$mean)-usj_ts_mean
kern_max_years$anom <-(kern_max_years$mean)-kern_ts_mean

# continue formatting
colnames(yuba_max_years)[1:2] <-c("max_mean","max_anom")
yuba_max_years$years <-seq(1985,2016,1)
yuba_max_years$basin <-rep("Yuba", nrow(yuba_max_years))

colnames(usj_max_years)[1:2] <-c("max_mean","max_anom")
usj_max_years$years <-seq(1985,2016,1)
usj_max_years$basin <-rep("USJ", nrow(usj_max_years))

colnames(kern_max_years)[1:2] <-c("max_mean","max_anom")
kern_max_years$years <-seq(1985,2016,1)
kern_max_years$basin <-rep("Kern", nrow(kern_max_years))
kern_max_years

max_years1 <-bind_rows(kern_max_years,usj_max_years)
max_years <-bind_rows(max_years1, yuba_max_years)
max_years

#  stack
tmean_stack <-rast("./rasters/gridmet/tmean/tmean_stack.tif")

# make basin stacks
yuba_tmean <-crop(mask(tmean_stack, yuba),yuba)
names(yuba_tmean) <-year_names
usj_tmean <-crop(mask(tmean_stack, usj),usj)
names(usj_tmean) <-year_names
kern_tmean <-crop(mask(tmean_stack, kern),kern)
names(kern_tmean) <-year_names

# calculate time series scene wide mean
yuba_tmean_years <-global(yuba_tmean, mean, na.rm = T)
yuba_ts_mean <-mean(yuba_tmean_years$mean)

usj_tmean_years <-global(usj_tmean, mean, na.rm = T)
usj_ts_mean <-mean(usj_tmean_years$mean)

kern_tmean_years <-global(kern_tmean, mean, na.rm = T)
kern_ts_mean <-mean(kern_tmean_years$mean)

# calculate tmean anom
yuba_tmean_years$anom <-(yuba_tmean_years$mean)-yuba_ts_mean
usj_tmean_years$anom <-(usj_tmean_years$mean)-usj_ts_mean
kern_tmean_years$anom <-(kern_tmean_years$mean)-kern_ts_mean

# continue formatting
colnames(yuba_tmean_years)[1:2] <-c("tmean_mean","tmean_anom")
yuba_tmean_years$years <-seq(1985,2016,1)
yuba_tmean_years$basin <-rep("Yuba", nrow(yuba_tmean_years))

colnames(usj_tmean_years)[1:2] <-c("tmean_mean","tmean_anom")
usj_tmean_years$years <-seq(1985,2016,1)
usj_tmean_years$basin <-rep("USJ", nrow(usj_tmean_years))

colnames(kern_tmean_years)[1:2] <-c("tmean_mean","tmean_anom")
kern_tmean_years$years <-seq(1985,2016,1)
kern_tmean_years$basin <-rep("Kern", nrow(kern_tmean_years))

tmean_years1 <-bind_rows(kern_tmean_years,usj_tmean_years)
tmean_years <-bind_rows(tmean_years1, yuba_tmean_years)
tmean_years

# fulll df
plotting_df <-full_join(max_years, tmean_years, c("years","basin"))
plotting_df

library(viridis)
scale <-viridis(32, alpha = 1, option = "H")

# plot
ggplot(plotting_df)+
  geom_point(aes(x = tmean_anom, y = max_anom, shape = basin, color = years)) +
  scale_color_gradientn(colors = scale) +
  geom_vline(xintercept = 0, linetype=2, col = "gray70", alpha = 1) +
  geom_hline(yintercept = 0, linetype=2, col = "gray70", alpha = 1) +
  ylab("Max SWE Anomaly (mm)")+
  xlab("ONDJFM Temperature Anomaly (°C)") +
  scale_x_continuous(limits = c(-4,4), breaks = seq(-3,3,1))+
  scale_y_continuous(limits = c(-750,750), breaks = seq(-750,750,250))+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth  = 1))+
  theme(legend.position = c(.9,.69),
        aspect.ratio = 1,
        panel.border = element_rect(colour = "black", fill=NA, linewidth  = 1),
        legend.text=element_text(size=6),
        legend.box.spacing = unit(0, "pt"),
        legend.title=element_blank())+
  
  guides(color = guide_colorbar(barwidth = .5,
                               barheight = 4,
                               ticks.colour = "black"))


ggsave("~/ch1_margulis/plots/wet_dry_years_basins_v1.pdf",
       width = 5,
       height = 5,
       units = "in")

system("open ~/ch1_margulis/plots/wet_dry_years_basins_v1.pdf")
  
# bains# bainscolorConverter()
years_mean <- plotting_df %>%
  group_by(years) %>%
  summarize(year_max_mean = mean(max_anom),
            year_tmean_mean = mean(tmean_anom))
years_mean

years_mean$hydro_cat <-NA
years_mean$hydro_cat <-ifelse(years_mean$year_tmean_mean > 0 & years_mean$year_max_mean > 0, "hw", years_mean$hydro_cat)
years_mean$hydro_cat <-ifelse(years_mean$year_tmean_mean < 0 & years_mean$year_max_mean > 0, "cw", years_mean$hydro_cat)
years_mean$hydro_cat <-ifelse(years_mean$year_tmean_mean < 0 & years_mean$year_max_mean < 0, "cd", years_mean$hydro_cat)
years_mean$hydro_cat <-ifelse(years_mean$year_tmean_mean > 0 & years_mean$year_max_mean < 0, "hd", years_mean$hydro_cat)
as.data.frame(years_mean)
write.csv(years_mean, "~/ch1_margulis/csvs/hydro_cat_years.csv")

ggplot(year_means, aes(x = year_tmean_mean, y = year_max_mean))+
  geom_vline(xintercept = 0, linetype=2, col = "gray70", alpha = 1) +
  geom_hline(yintercept = 0, linetype=2, col = "gray70", alpha = 1) +
  geom_point(shape = 3) +
  geom_text(aes(label = years),  hjust = 0.5,  vjust = -1, size = 2, color = "gray50") +
  ylab("Max SWE Anomaly (mm)")+
  xlab("ONDJFM Temperature Anomaly (°C)") +
  scale_x_continuous(limits = c(-3,3), breaks = seq(-3,3,1))+
  scale_y_continuous(limits = c(-600,600), breaks = seq(-600,600,200))+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth  = 1))+
  theme(legend.position = c(.85,.79),
        aspect.ratio = 1,
        panel.border = element_rect(colour = "black", fill=NA, linewidth  = 1),
        legend.title=element_blank())+
  annotate("text", x = -2.3, y = 530, label = "Cold/Wet", color = "darkblue")+
  annotate("text", x = -2.3, y = -530, label = "Cold/Dry", color = "grey") +
  annotate("text", x = 2.3, y = 530, label = "Warm/Wet", color = "grey") +
  annotate("text", x = 2.3, y = -530, label = "Warm/Dry", color = "darkred") 

ggsave("~/ch1_margulis/plots/wet_dry_years_v1.pdf",
       width = 5.5,
       height = 5.5,
       units = "in")

system("open ~/ch1_margulis/plots/wet_dry_years_v1.pdf")

plotting_df$hydro_cat <-NA
plotting_df$hydro_cat <-ifelse(plotting_df$tmean_anom > 0 & plotting_df$max_anom > 0, "hw", plotting_df$hydro_cat)
plotting_df$hydro_cat <-ifelse(plotting_df$tmean_anom < 0 & plotting_df$max_anom > 0, "cw", plotting_df$hydro_cat)
plotting_df$hydro_cat <-ifelse(plotting_df$tmean_anom < 0 & plotting_df$max_anom < 0, "cd", plotting_df$hydro_cat)
plotting_df$hydro_cat <-ifelse(plotting_df$tmean_anom > 0 & plotting_df$max_anom < 0, "hd", plotting_df$hydro_cat)
plotting_df

