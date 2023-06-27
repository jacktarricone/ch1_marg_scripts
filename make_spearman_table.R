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

theme_set(theme_classic(14))

# read in df

df_list <-list.files("./csvs/spearman_results", full.names = TRUE, pattern = "KUY")
df_list
df_path <-df_list[[1]]
# hist(ah$p_val, breaks = 100)
# hist(ah$mean_rh, breaks = 100)
# hist(ah$mean_ah, breaks = 100)
# hist(ah$mean_fm, breaks = 100)

make_3panel_heatmap <-function(df_path, twovars){
  
  df <-fread(df_path)
  name_v1 <-basename(df_path)
  name_v2 <- substr(name_v1, 1, nchar(name_v1) - 30)

  # elevations zones
  df$zone_name <-ifelse(df$ez== 1, "1500-1900 m", df$ez)
  df$zone_name <-ifelse(df$ez == 2, "1900-2300 m", df$zone_name)
  df$zone_name <-ifelse(df$ez == 3, "2300-2700 m", df$zone_name)
  df$zone_name <-ifelse(df$ez == 4, "2700-3100 m", df$zone_name)
  df$zone_name <-ifelse(df$ez == 5, "3100-4361 m", df$zone_name)
  df$zone_name <-ifelse(df$ez == 6, "3100-4361 m", df$zone_name)

  # elevations zones
  df$aspect_name <-ifelse(df$aspect == 1, "North", df$aspect)
  df$aspect_name <-ifelse(df$aspect == 2, "East", df$aspect_name)
  df$aspect_name <-ifelse(df$aspect == 3, "South", df$aspect_name)
  df$aspect_name <-ifelse(df$aspect == 4, "West", df$aspect_name)

  # remane basin col
  colnames(df)[1] <-"Basin"
  colnames(df)[6] <-"mean_rh"
  
  # write function which calculates percentage of bin that is significant
  results_v1 <-df %>%
    group_by(Basin, zone_name, aspect_name) %>%
    summarise(percent_sig     = round((length(which(p_val < .05))/length(p_val))*100, 0),
              rho_mean        = mean(rho_val),
              mean_ez_temp_c  = mean(mean_temp_c),
              mean_ez_fm      = mean(mean_fm),
              mean_ez_rh      = mean(mean_rh))
  
  results_v1
  
  # filter for north and south facing
  north_results <-filter(results_v1, aspect_name == "North")
  colnames(north_results)[4] <-"north_percent_sig"
  
  # remove zone 4 feather, like 4 pixels
  south_results <-filter(results_v1, aspect_name == "South")
  colnames(south_results)[4] <-"south_percent_sig"
  south_results
  identical(south_results$Basin, north_results$Basin)
  
  # make differnce df
  diff_results <-as.data.frame(full_join(north_results, south_results, 
                                         by = c("Basin","zone_name")))
  
  diff_results$diff <-diff_results$north_percent_sig - diff_results$south_percent_sig 
  diff_results
  
  # plot
  mycolors <-rev(brewer.pal(9, "Spectral"))

  #### north facing
  north_p <-ggplot(north_results, aes(y=Basin, x=zone_name, fill= north_percent_sig)) + 
    geom_tile()+
    geom_text(aes(label=north_percent_sig)) +
    scale_fill_gradientn(colors = mycolors, limits = c(0,100), oob = squish) +
    labs(x = "EZ", fill = "Area Significant (%)", title = paste0(twovars, " North Facing")) +
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          panel.background = element_rect(fill = 'gray'),
          aspect.ratio = .25,
          legend.position  = 'bottom',
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(.25,.1,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(fill = guide_colorbar(direction = "horizontal",
                                  label.position = 'top',
                                  title.position = 'bottom',
                                  title.hjust = .5,
                                  barwidth = 15,
                                  barheight = 1,
                                  frame.colour = "black", 
                                  ticks.colour = "black"))
  
  #### south facing
  south_p <-ggplot(south_results, aes(y=Basin, x=zone_name, fill= south_percent_sig)) + 
    geom_tile()+
    geom_text(aes(label=south_percent_sig)) +
    scale_fill_gradientn(colors = mycolors, limits = c(0,100), oob = squish) +
    labs(fill = "Area Significant (%)", title = paste0(twovars, " South Facing")) +
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          panel.background = element_rect(fill = 'gray'),
          aspect.ratio = .25,
          legend.position  = 'bottom',
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(.25,.1,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(fill = guide_colorbar(direction = "horizontal",
                                 label.position = 'top',
                                 title.position = 'bottom',
                                 title.hjust = .5,
                                 barwidth = 15,
                                 barheight = 1,
                                 frame.colour = "black", 
                                 ticks.colour = "black"))
  south_p
  
  
  # set scale 
  diff_colors <-brewer.pal(9, "RdBu")
  head(diff_results)
  
  #### difference
  diff_p <-ggplot(diff_results, aes(y=Basin, x=zone_name, fill= diff)) + 
    geom_tile()+
    geom_text(aes(label=diff)) +
    scale_fill_gradientn(colors = diff_colors, limits = c(-50,50), oob = squish) +
    labs(x = "EZ", fill = "Difference (%)", title = paste0(twovars, " North - South Difference")) +
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          panel.background = element_rect(fill = 'gray'),
          aspect.ratio = .25,
          legend.position  = 'bottom',
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(.25,.1,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(fill = guide_colorbar(direction = "horizontal",
                                 label.position = 'top',
                                 title.position = 'bottom',
                                 title.hjust = .5,
                                 barwidth = 15,
                                 barheight = 1,
                                 frame.colour = "black", 
                                 ticks.colour = "black"))
  diff_p
  
  ### cowing
  # cowplot test
  full <-plot_grid(north_p, south_p, diff_p,
                   # labels = c("(a)", "(b)", "(c)"),
                   ncol = 3, 
                   align = "hv",
                   # label_size = 24,
                   # vjust =  3,
                  #  hjust = -1,
                   rel_widths = c(1/3, 1/3, 1/3))

  # test save
  # make tighter together

  saving_path <-paste0("./plots/kuy_",name_v2,"_heatmap_v1.png")
  
  ggsave(full,
         file = saving_path,
         width = 16, 
         height = 3,
         dpi = 600)
  
  return(full)
}
make_3panel_heatmap_ns <-function(df_path, twovars){
  
  df <-fread(df_path)
  name_v1 <-basename(df_path)
  name_v2 <- substr(name_v1, 1, nchar(name_v1) - 30)
  
  # elevations zones
  df$zone_name <-ifelse(df$ez== 1, "1500-1900 m", df$ez)
  df$zone_name <-ifelse(df$ez == 2, "1900-2300 m", df$zone_name)
  df$zone_name <-ifelse(df$ez == 3, "2300-2700 m", df$zone_name)
  df$zone_name <-ifelse(df$ez == 4, "2700-3100 m", df$zone_name)
  df$zone_name <-ifelse(df$ez == 5, "3100-4361 m", df$zone_name)
  df$zone_name <-ifelse(df$ez == 6, "3100-4361 m", df$zone_name)
  
  # elevations zones
  df$aspect_name <-ifelse(df$aspect == 1, "North", df$aspect)
  df$aspect_name <-ifelse(df$aspect == 2, "East", df$aspect_name)
  df$aspect_name <-ifelse(df$aspect == 3, "South", df$aspect_name)
  df$aspect_name <-ifelse(df$aspect == 4, "West", df$aspect_name)
  
  # remane basin col
  colnames(df)[1] <-"Basin"
  colnames(df)[6] <-"mean_rh"
  
  # write function which calculates percentage of bin that is significant
  results_v1 <-df %>%
    group_by(Basin, zone_name, aspect_name) %>%
    summarise(percent_sig     = round((length(which(p_val < .05))/length(p_val))*100, 0),
              rho_mean        = mean(rho_val),
              mean_ez_temp_c  = mean(mean_temp_c),
              mean_ez_fm      = mean(mean_fm),
              mean_ez_rh      = mean(mean_rh))
  
  results_v1
  
  # filter for north and south facing
  north_results <-filter(results_v1, aspect_name == "North")
  colnames(north_results)[4] <-"north_percent_sig"
  
  # remove zone 4 feather, like 4 pixels
  south_results <-filter(results_v1, aspect_name == "South")
  colnames(south_results)[4] <-"south_percent_sig"
  south_results
  identical(south_results$Basin, north_results$Basin)
  
  # make differnce df
  diff_results <-as.data.frame(full_join(north_results, south_results, 
                                         by = c("Basin","zone_name")))
  
  diff_results$diff <-diff_results$north_percent_sig - diff_results$south_percent_sig 
  diff_results
  
  # plot
  mycolors <-rev(brewer.pal(9, "Spectral"))
  
  #### north facing
  north_p <-ggplot(north_results, aes(y=Basin, x=zone_name, fill= north_percent_sig)) + 
    geom_tile()+
    geom_text(aes(label=north_percent_sig)) +
    scale_fill_gradientn(colors = mycolors, limits = c(0,100), oob = squish) +
    labs(x = "EZ", fill = "Area Significant (%)", title = paste0(twovars, " North Facing")) +
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          panel.background = element_rect(fill = 'gray'),
          aspect.ratio = .25,
          legend.position = 'none',
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(.25,.1,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(fill = guide_colorbar(direction = "horizontal",
                                 label.position = 'top',
                                 title.position = 'bottom',
                                 title.hjust = .5,
                                 barwidth = 10,
                                 barheight = 1,
                                 frame.colour = "black", 
                                 ticks.colour = "black"))
  
  #### south facing
  south_p <-ggplot(south_results, aes(y=Basin, x=zone_name, fill= south_percent_sig)) + 
    geom_tile()+
    geom_text(aes(label=south_percent_sig)) +
    scale_fill_gradientn(colors = mycolors, limits = c(0,100), oob = squish) +
    labs(fill = "Area Significant (%)", title = paste0(twovars, " South Facing")) +
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          panel.background = element_rect(fill = 'gray'),
          legend.position = 'none',
          aspect.ratio = .25,
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(.25,.1,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(fill = guide_colorbar(direction = "horizontal",
                                 label.position = 'top',
                                 title.position = 'bottom',
                                 title.hjust = .5,
                                 barwidth = 10,
                                 barheight = 1,
                                 frame.colour = "black", 
                                 ticks.colour = "black"))
  south_p
  
  
  # set scale 
  diff_colors <-brewer.pal(9, "RdBu")
  head(diff_results)
  
  #### difference
  diff_p <-ggplot(diff_results, aes(y=Basin, x=zone_name, fill= diff)) + 
    geom_tile()+
    geom_text(aes(label=diff)) +
    scale_fill_gradientn(colors = diff_colors, limits = c(-50,50), oob = squish) +
    labs(x = "EZ", fill = "Difference (%)", title = paste0(twovars, " North - South Difference")) +
    scale_x_discrete(expand = c(0, 0))+
    scale_y_discrete(expand = c(0, 0))+
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
          panel.background = element_rect(fill = 'gray'),
          aspect.ratio = .25,
          legend.position  = 'none',
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          plot.margin = unit(c(.25,.1,.1,.1), "cm"),
          legend.box.spacing = unit(0, "pt")) +
    guides(fill = guide_colorbar(direction = "horizontal",
                                 label.position = 'top',
                                 title.position = 'bottom',
                                 title.hjust = .5,
                                 barwidth = 10,
                                 barheight = 1,
                                 frame.colour = "black", 
                                 ticks.colour = "black"))
  diff_p
  
  ### cowing
  # cowplot test
  full <-plot_grid(north_p, south_p, diff_p,
                   # labels = c("(a)", "(b)", "(c)"),
                   ncol = 3, 
                   align = "hv",
                   # label_size = 24,
                   # vjust =  3,
                   #  hjust = -1,
                   rel_widths = c(1/3, 1/3, 1/3))
  
  # test save
  # make tighter together
  
  saving_path <-paste0("./plots/kuy_",name_v2,"_heatmap_v1.png")
  
  ggsave(full,
         file = saving_path,
         width = 16, 
         height = 3,
         dpi = 600)
  
  return(full)
}

ah_test <-make_3panel_heatmap(df = df_list[[1]], twovars = "AH vs. FM")
rh_test <-make_3panel_heatmap_ns(df = df_list[[3]], twovars = "RH vs. FM")
srad_test <-make_3panel_heatmap_ns(df = df_list[[4]], twovars = "Srad vs. FM")
tmean_test <-make_3panel_heatmap_ns(df = df_list[[5]], twovars = "Tmean vs. FM")


full <-plot_grid(tmean_test, rh_test, srad_test, ah_test,
                 labels = c("(a)", "(b)", "(c)","(d)"),
                 nrow = 4, 
                 align = "hv",
                 label_size = 18,
                 vjust =  1.7,
                 hjust = -.2,
                 rel_heights = c(.9, .9, .9, 1.35))
# full
ggsave(full,
       file = "./plots/metvars_fm_heatmaps_v3.png",
       width = 17,
       height = 9,
       units = "in",
       dpi = 300)

system("open ./plots/metvars_fm_heatmaps_v3.png")
  