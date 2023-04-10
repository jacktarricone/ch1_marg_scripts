# comparing max swe to SNOTELS

library(terra)
library(dplyr)
library(ggplot2)
library(snotelr)
library(lubridate)
library(Metrics)
library(cowplot)

setwd('~/ch1_margulis')

# load in functions
# this file has all the snow metric functions and the raster creation one
url <-"https://raw.githubusercontent.com/jacktarricone/ch1_marg_scripts/main/snow_metric_functions.R"
devtools::source_url(url)

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


#########################
#########################
## read in snotel data ##
#########################
#########################

snotel_df <-read.csv("./csvs/snotel_df.csv")
snotel_df <-subset(snotel_df, select=-c(X, network)) # move bad one
head(snotel_df)

#########################
#########################
###  read in SNSR data ##
#########################
#########################

# read in csvs
snsr_snotel_list <-sort(list.files("./csvs/rename_snsr_snotel_data", full.names = TRUE))
snsr_snotel_data <-lapply(snsr_snotel_list, read.csv)

# bind together
snsr_df <-bind_rows(snsr_snotel_data) # make df
snsr_df <-subset(snsr_df, select=-c(station_id)) # move bad one
colnames(snsr_df)[c(1,4,5)] <-c('site_name_v2','lat','lon')
head(snsr_df)
unique(snsr_df$site_name_v2)
unique(snotel_df$site_name)

# bind together
swe_df <-cbind(snotel_df, snsr_df)
head(swe_df)

# test to see if wy match
year_match_test <-ifelse(swe_df$wy == swe_df$waterYear, TRUE, FALSE)
unique(year_match_test)

#########################
#########################
##     max swe     ##
#########################
#########################

# calc metric 
max_df <-as.data.frame(swe_df %>%
                       group_by(site_name, waterYear) %>%
                       summarise(max_snotel_0 = max_swe(snotel_swe_mm, swe_thres = 0)/1000,
                                 max_snsr_0   = max_swe(snsr_swe_mm, swe_thres = 0)/1000,
                                 max_snotel_25.4 = max_swe(snotel_swe_mm, swe_thres = 25.4)/1000,
                                 max_snsr_25.4   = max_swe(snsr_swe_mm, swe_thres = 25.4)/1000,
                                 max_snotel_50.8 = max_swe(snotel_swe_mm, swe_thres = 50.8)/1000,
                                 max_snsr_50.8   = max_swe(snsr_swe_mm, swe_thres = 50.8)/1000))


# calculate metric to report
# R
max_corr <-round(cor(max_df$max_snotel_25.4, max_df$max_snsr_25.4, 
                        use = "complete.obs", method = 'pearson'), digits = 2)
max_corr_lab <-paste0('R = ', max_correlation)

# rmse and mae
max_rmse <-round(hydroGOF::rmse(max_df$max_snsr_25.4, max_df$max_snotel_25.4, na.rm = TRUE), digits = 2)
max_mae <-round(hydroGOF::mae(max_df$max_snsr_25.4, max_df$max_snotel_25.4, na.rm = TRUE), digits = 2)
max_rmse_lab <-paste0("RMSE = ",max_rmse," (m)")
max_mae_lab <-paste0("MAE = ",max_mae," (m)") 


# plot
max_25 <-ggplot(max_df) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(aes(x = max_snotel_25.4, y = max_snsr_25.4), shape = 3, size = .5, color = "darkred") +
  geom_label(x = .8, y = 2.8, label = max_corr_lab, label.size = NA, fontface = "bold") +
  geom_label(x = .8, y = 2.6, label = max_rmse_lab, label.size = NA, fontface = "bold") +
  geom_label(x = .8, y = 2.4, label = max_mae_lab, label.size = NA, fontface = "bold") +
  scale_y_continuous(limits = c(0,3),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,3),expand = (c(0,0))) +
  xlab("SNOTEL Max (mm)") + ylab("SNSR Max SWE (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

# save
ggsave( "./plots/max_metric_compare_v4.pdf",
       max_25,
       width = 4.5,
       height = 4.5,
       units = "in")

system("open ./plots/max_metric_compare_v4.pdf")


#########################
#########################
##     max_dowy swe    ##
#########################
#########################

# calc metric 
max_dowy_df <-as.data.frame(swe_df %>%
                         group_by(site_name, waterYear) %>%
                         summarise(max_dowy_snotel_0 = max_swe_dowy(snotel_swe_mm, swe_thres = 0),
                                   max_dowy_snsr_0   = max_swe_dowy(snsr_swe_mm, swe_thres = 0),
                                   max_dowy_snotel_25.4 = max_swe_dowy(snotel_swe_mm, swe_thres = 25.4),
                                   max_dowy_snsr_25.4   = max_swe_dowy(snsr_swe_mm, swe_thres = 25.4),
                                   max_dowy_snotel_50.8 = max_swe_dowy(snotel_swe_mm, swe_thres = 50.8),
                                   max_dowy_snsr_50.8   = max_swe_dowy(snsr_swe_mm, swe_thres = 50.8)))


# calculate metric to report
# R
max_dowy_corr <-round(cor(max_dowy_df$max_dowy_snotel_25.4, max_dowy_df$max_dowy_snsr_25.4, 
                        use = "complete.obs", method = 'pearson'), digits = 2)

max_dowy_corr_lab <-paste0("R = ",max_dowy_corr) 

# rmse and mae
max_dowy_rmse <-round(hydroGOF::rmse(max_dowy_df$max_dowy_snsr_25.4, max_dowy_df$max_dowy_snotel_25.4, na.rm = TRUE), digits = 2)
max_dowy_mae <-round(hydroGOF::mae(max_dowy_df$max_dowy_snsr_25.4, max_dowy_df$max_dowy_snotel_25.4, na.rm = TRUE), digits = 2)
max_dowy_rmse_lab <-paste0("RMSE = ",max_dowy_rmse," (days)")
max_dowy_mae_lab <-paste0("MAE = ",max_dowy_mae," (days)") 

# plot
max_dowy_25 <-ggplot(max_dowy_df) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(aes(x = max_dowy_snotel_25.4, y = max_dowy_snsr_25.4), shape = 3, size = .5, color = "darkviolet") +
  geom_label(x = 120, y = 290, label = max_dowy_corr_lab, label.size = NA, fontface = "bold") +
  geom_label(x = 120, y = 275, label = max_dowy_rmse_lab, label.size = NA, fontface = "bold") +
  geom_label(x = 120, y = 260, label = max_dowy_mae_lab, label.size = NA, fontface = "bold") +
  scale_y_continuous(limits = c(50,300),expand = (c(0,0))) +
  scale_x_continuous(limits = c(50,300),expand = (c(0,.1))) +
  xlab("SNOTEL Max DOWY") + ylab("SNSR Max DOWY") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

# save
ggsave( "./plots/max_dowy_metric_compare_v3.pdf",
       max_dowy_25,
       width = 4.5,
       height = 4.5,
       units = "in")

system("open ./plots/max_dowy_metric_compare_v3.pdf")

#########################
#########################
##     sdd swe     ##
#########################
#########################

# calc metric 
sdd_df <-as.data.frame(swe_df %>%
                       group_by(site_name, waterYear) %>%
                       summarise(sdd_snotel_0 = sdd(snotel_swe_mm, swe_thres = 0),
                                 sdd_snsr_0   = sdd(snsr_swe_mm, swe_thres = 0),
                                 sdd_snotel_25.4 = sdd(snotel_swe_mm, swe_thres = 25.4),
                                 sdd_snsr_25.4   = sdd(snsr_swe_mm, swe_thres = 25.4),
                                 sdd_snotel_50.8 = sdd(snotel_swe_mm, swe_thres = 50.8),
                                 sdd_snsr_50.8   = sdd(snsr_swe_mm, swe_thres = 50.8)))


# calculate metric to report
# R
sdd_corr <-round(cor(sdd_df$sdd_snotel_25.4, sdd_df$sdd_snsr_25.4, 
                                 use = "complete.obs", method = 'pearson'), digits = 2)
sdd_corr_lab <-paste0("R = ", sdd_corr)

# rmse and mae
sdd_rmse <-round(hydroGOF::rmse(sdd_df$sdd_snsr_25.4, sdd_df$sdd_snotel_25.4, na.rm = TRUE), digits = 2)
sdd_mae <-round(hydroGOF::mae(sdd_df$sdd_snsr_25.4, sdd_df$sdd_snotel_25.4, na.rm = TRUE), digits = 2)
sdd_rmse_lab <-paste0("RMSE = ",sdd_rmse," (days)")
sdd_mae_lab <-paste0("MAE = ",sdd_mae," (days)") 

# plot
sdd_25 <-ggplot(sdd_df) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(aes(x = sdd_snotel_25.4, y = sdd_snsr_25.4), shape = 3, size = .5, color = "darkgreen") +
  geom_label(x = 120, y = 350, label = sdd_corr_lab, label.size = NA, fontface = "bold") +
  geom_label(x = 120, y = 335, label = sdd_rmse_lab, label.size = NA, fontface = "bold") +
  geom_label(x = 120, y = 320, label = sdd_mae_lab, label.size = NA, fontface = "bold") +
  scale_y_continuous(limits = c(50,366),expand = (c(0,0))) +
  scale_x_continuous(limits = c(50,366),expand = (c(0,0))) +
  xlab("SNOTEL SDD (DOWY)") + ylab("SNSR SSD (DOWY)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

# save
ggsave( "./plots/sdd_metric_compare_v1.pdf",
        sdd_25,
        width = 4.5,
        height = 4.5,
        units = "in")

system("open ./plots/sdd_metric_compare_v1.pdf")
