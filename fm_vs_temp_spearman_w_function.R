# annual temp vs fm and short wave
# jack tarricone
# may 15th, 2023

library(terra)
library(tidyverse)
library(scales)
library(cowplot)
library(viridis)
library(data.table)

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

theme_set(theme_classic(14))

# bring vectors
files <-list.files("./vectors/ca_basins", full.names = TRUE, pattern = ".gpkg")
shp_list <-lapply(files, vect)
shp_list

# single rasters: insol, temp normal, dem
insol_american <-mask(crop(rast("./rasters/insolation/snsr_dem_insol_watts_masked_v1.tif"), ext(american)), american)
names(insol_american) <-"insol_watts"
plot(insol_american)

# temp
temp_american <-mask(crop(rast("./rasters/daymet/tmean_normal_1985_2016.tif"),ext(american)), american)
names(temp_american) <-"mean_temp_c"
plot(temp_american)

# dem
dem_american <-mask(crop(rast("./rasters/static/SNSR_DEM.tif"),ext(american)), american)
names(dem_american) <-"elevation"
plot(dem_american)

### stacks
# fm load and format
fm_list <- list.files("./rasters/snow_metrics/fm_apr1", full.names = TRUE)
fm_stack_american <-mask(crop(rast(fm_list[1:32]),ext(american)), american)
plot(fm_stack_american[[32]])

# temp load and format
tmean_list <-list.files("./rasters/daymet/tmean", full.names = TRUE)
tmean_stack_american <-mask(crop(rast(tmean_list[1:32]),ext(american)), american)
plot(tmean_stack_american[[32]])

# rename layers
wy_names <-seq(1985,2016,1)
names(tmean_stack_american) <-wy_names
names(fm_stack_american) <-wy_names
tmean_stack_american

# calculate number of non na obs per pixel
fm_n_obs <-app(fm_stack_american, function(x) sum(!is.na(x)))
n_obs_v2 <-subst(fm_n_obs, 0, NA)
plot(n_obs_v2)

# convert all values below 10 to NA
masking_value <-subst(n_obs_v2, 0:27, NA)
plot(masking_value)

# if there are less than 10 observations per pixel, make NA
fm_v3 <-mask(fm_stack_american, masking_value, maskvalues = NA)

# stack and join
# fm
fm_stack <-c(dem_american, temp_american, insol_american, fm_v3)
fm_df_v1 <-as.data.frame(fm_stack, xy = TRUE, cell = TRUE)
fm_df <-as.data.frame(tidyr::pivot_longer(fm_df_v1 ,7:38, names_to = 'wy', values_to = 'frac_melt'))
head(fm_df)

# tmean
tmean_stack <-c(dem_american, temp_american, insol_american, tmean_stack_american)
tmean_df_v1 <-as.data.frame(tmean_stack, xy = TRUE, cell = TRUE)
tmean_df <-as.data.frame(tidyr::pivot_longer(tmean_df_v1 ,7:38, names_to = 'wy', values_to = 'ondjfm_temp_c'))
head(tmean_df)

# full join
analysis_df <-full_join(fm_df, tmean_df)
analysis_df <-analysis_df %>% drop_na()
tail(analysis_df)
head(analysis_df)

# # quick test df with two cells
# test_df <-filter(analysis_df, cell == 841193)
# head(test_df)

# # test ft
# fit <-cor.test(test_df$frac_melt, test_df$ondjfm_temp_c, method = 'spearman', exact=FALSE)
# fit

# covert to data table and set key
DT <- as.data.table(analysis_df)
setkey(DT, cell)

# run much faster, 
results_v1 <-DT[,list(corr = cor.test(frac_melt,ondjfm_temp_c,method = 'spearman', exact = FALSE)), by = cell]
results_raw <-as.data.frame(unlist(results_v1$corr))
head(results_raw)

# extract results from unlist/unorganized df
# p_val
p_val <-as.data.frame(as.numeric(results_raw[seq(2, nrow(results_raw), 7), ]))
names(p_val)[1] <-"p_val"
head(p_val)

# rho
rho_val <-as.data.frame(as.numeric(results_raw[seq(3, nrow(results_raw), 7), ]))
names(rho_val)[1] <-"rho_val"
head(rho_val)

# s_stat
s_stat <-as.data.frame(as.numeric(results_raw[seq(1, nrow(results_raw), 7), ]))
names(s_stat)[1] <-"s_stat"
head(s_stat)

# format static values for binding
single_cell_df <-as.data.frame(analysis_df %>% group_by(cell) %>%
  slice(which.min(x)) %>%
  select(-c(frac_melt,ondjfm_temp_c,wy)))

# make spearman df
cell <-unique(results_v1$cell)
spearman_df <-data.frame(cell,p_val,rho_val,s_stat)
head(spearman_df)

# combine both for full df
results_df <-full_join(single_cell_df,spearman_df)
head(results_df)
fwrite(results_df, "./csvs/usj_spearman_results_v1.csv", row.names = FALSE)

# test hists
hist(results_df$rho_val, breaks = 100)
hist(results_df$p_val, breaks = 100)

# filter for sig
sig_df <-filter(results_df, p_val < .05)
not_sig_df <-filter(results_df, p_val > .05)

percent_sig <-(nrow(sig_df)/nrow(results_df))*100
percent_no_sig <-(nrow(not_sig_df)/nrow(results_df))*100

# create plotting function
plot_rho_vs_elevation <-function(not_sig, sig, scale, title){
  
  plot <-ggplot() +
    geom_point(data = not_sig, aes(y = rho_val, x = elevation, color = mean_temp_c), alpha = .05, size = .1, shape = 4) +
    geom_point(data = sig, aes(y = rho_val, x = elevation, color = mean_temp_c), alpha = .3, size = .4, shape = 19) +
    scale_color_gradientn(colors = scale, name = "Mean Temperature (°C)") +
    scale_x_continuous(limits = c(min(results_df$elevation),max(results_df$elevation)), expand = (c(0,0))) +
    scale_y_continuous(limits = c(-.2,.8),expand = (c(0,0))) +
    labs(x = "Elevation (m)", y = "Spearman's Rho")+
    annotate(geom="text", x = 3500, y = -.1, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          aspect.ratio = 1,
          legend.position  = 'top',
          plot.margin = unit(c(.25,.1,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(color = guide_colorbar(direction = "horizontal",
                                  label.position = 'bottom',
                                  title.position = 'top',
                                  title.hjust = .5,
                                  barwidth = 20,
                                  barheight = 1,
                                  frame.colour = "black", 
                                  ticks.colour = "black"))
  return(plot)
}

## set color
scale2 <-c(viridis(30, option = "A", direction = 1))

# plot
american_rho_ele_plot <-plot_rho_vs_elevation(not_sig = not_sig_df, 
                                              sig = sig_df,
                                              scale = scale2,
                                              title = "USJ") 
# save
ggsave(american_rho_ele_plot,
       file = "./plots/usj_rho_ele_temp_spearman_test_v7.png",
       width = 5, 
       height = 5.8,
       dpi = 600)

system("open ./plots/usj_rho_ele_temp_spearman_test_v7.png")




# create plotting function
plot_rho_ele_sw <-function(not_sig, sig, scale, title){
  
  plot <-ggplot() +
    geom_point(data = not_sig, aes(y = rho_val, x = elevation, color = insol_watts), alpha = .05, size = .1, shape = 4) +
    geom_point(data = sig, aes(y = rho_val, x = elevation, color = insol_watts), alpha = .3, size = .4, shape = 19) +
    scale_color_gradientn(colors = scale, name = expression("Insolation",paste(~'(W m'^{"-2"},')'))) +
    scale_x_continuous(limits = c(min(results_df$elevation),max(results_df$elevation)), expand = (c(0,0))) +
    scale_y_continuous(limits = c(-.2,.8),expand = (c(0,0))) +
    labs(x = "Elevation (m)", y = "Spearman's Rho")+
    annotate(geom="text", x = 3500, y = -.1, label= title, size = 8, fontface = "bold")+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          aspect.ratio = 1,
          legend.position  = 'top',
          plot.margin = unit(c(.25,.1,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(color = guide_colorbar(direction = "horizontal",
                                  label.position = 'bottom',
                                  title.position = 'top',
                                  title.hjust = .5,
                                  barwidth = 20,
                                  barheight = 1,
                                  frame.colour = "black", 
                                  ticks.colour = "black"))
  return(plot)
}

## set color
scale2 <-c(viridis(30, option = "D", direction = 1))

# plot
american_rho_ele_sw <-plot_rho_ele_sw(not_sig = not_sig_df, 
                                              sig = sig_df,
                                              scale = scale2,
                                              title = "USJ") 
# save
ggsave(american_rho_ele_sw,
       file = "./plots/usj_rho_ele_swe_spearman_test_v2.png",
       width = 5, 
       height = 5.8,
       dpi = 600)

system("open ./plots/usj_rho_ele_swe_spearman_test_v2.png")


mean(results_df$rho_val)
hist(results_df$rho_val, breaks = 100)
mean(results_df$p_val)





