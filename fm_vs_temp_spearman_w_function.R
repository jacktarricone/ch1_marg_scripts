# generate spearman df by basin
# jack tarricone
# june 11th, 2023

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

# list of paths to shape files
basin_paths <-list.files("./vectors/ca_basins", full.names = TRUE, pattern = "\\.gpkg$")

# head_df for col referencing
head_df <-fread("./csvs/gridmet_dfs/head_df.csv")
head(head_df)

# read in full df
system.time(analysis_df <-fread("./csvs/gridmet_dfs/full_df.csv", nThread=14))

# Assuming you have a data.table object called 'dt' with a grouping variable 'group_var'
# Split the data.table by the grouping variable
grouped_dts <- split(analysis_df, by = "basin_name")
names(grouped_dts) <-basin_names
# rm(analyis_df)

setwd("~/ch1_margulis/csvs/gridmet_dfs/")

# Save individual data.frames as separate files
for (i in seq_along(grouped_dts)) {
  name <-names(grouped_dts)[[i]]
  fwrite(grouped_dts[[i]], file = paste0(name, "_full_stats.csv"))
}

# generate spearman results df by basin
generate_spearman_df <-function(basin_paths_list){

  # # full join
  # analysis_df <-full_join(fm_df, tmean_df)
  # analysis_df <-analysis_df %>% drop_na()
  # 
  # # covert to data table and set key
  # DT <-as.data.table(analysis_df)
  # setkey(analysis_df, cell)

  # run using data.table -- super fast
  results_v1 <-DT[,list(corr = cor.test(frac_melt,temp_mean_c,
                                      method = 'spearman', exact = FALSE)), by = cell]

  # convert back to data frame
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
  # head(spearman_df)

  # combine both for full df
  results_df <-full_join(single_cell_df,spearman_df)
  
  # add basin name col
  results_df_v2 <-cbind(results_df, rep(basin_name, nrow(results_df)))
  colnames(results_df_v2)[13] <-"basin_name"
  
  # save
  saving_name <-paste0(basin_name,"_spearman_results.csv")
  fwrite(results_df_v2, paste0("./csvs/spearman_fm_temp_results/",saving_name))
  print(paste0(basin_name, " is done!"))
  
}

# apply function to list of basin shape files
lapply(basin_paths, generate_spearman_df)


# subset by sig
sig_df <-filter(results_df, p_val < .05 & ez == 1)
not_sig_df <-filter(results_df, p_val > .05 & ez == 1)
rows <-rbind(sig_df,not_sig_df)

percent_sig <-(nrow(sig_df)/nrow(rows))*100
percent_no_sig <-(nrow(not_sig_df)/nrow(rows))*100

# plot_test <-filter(analysis_df, cell == 233366 | cell == 233369 | cell == 233370 | cell == 233373)

# set scale
topo_table <-read.csv("./gis/topo_colors.csv")
topo_colors <-c(topo_table$colors)

# test plot
p1 <-ggplot() +
  geom_point(sig_df, mapping = aes(y = mean_fm, x = mean_temp_c, color = elevation),
             alpha = (40/100), size = (1.5), shape = 10)+
  scale_color_gradientn(colors = topo_colors, limits = c(1500,4000)) +
  scale_y_continuous(limits = c(0,1), expand = (c(0,0))) +
  scale_x_continuous(limits = c(-7,7), expand = (c(0,0))) +
  labs(y = "Mean FM", x = expression('ONDJFM ' ~T["Mean"] ~ '(°C)')) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1), 
        aspect.ratio = 1,
        legend.position  = 'right',
        legend.title = element_blank(),
        plot.margin = unit(c(.25,.1,.1,.1), "cm"),
        legend.box.spacing = unit(0, "pt")) +
  guides(color = guide_colorbar(direction = "vertical",
                               label.position = 'right',
                               title.hjust = .5,
                               barwidth = 1,
                               barheight = 17,
                               frame.colour = "black", 
                               ticks.colour = "black"))
p1

p2 <-p1 + geom_point(not_sig_df, mapping = aes(y = mean_fm, x = mean_temp_c), 
                     color = "black", alpha = (3/100), size = (1), shape = 4)

p2









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





