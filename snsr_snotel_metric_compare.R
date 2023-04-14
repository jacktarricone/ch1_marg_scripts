# comparing SNSR and SNOTEL snow metrics
# jack tarricone

library(dplyr)
library(ggplot2)
library(lubridate)
library(hydroGOF)
library(cowplot)
library(viridis)
library(ggpointdensity)


# set sd
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

# snotel_df_v1 <-read.csv("./csvs/snotel_df_v2.csv")
# snotel_df_v1 <-subset(snotel_df_v1, select=-c(X, network)) # move bad one
# head(snotel_df_v1)
# 
# # add NaN rows becaues horse meadow only has 357 days in 2004
# # makes df exact same size
# 
# missing_days <-snotel_df_v1[rep(119430, 9),]
# snotel_df <-rbind(snotel_df_v1[1:119430,], missing_days, snotel_df_v1[119431:nrow(snotel_df_v1),])
# write.csv(snotel_df, "./csvs/snotel_df_v3.csv")

snotel_df_v1 <-read.csv("./csvs/snotel_df_v3.csv")
snotel_df_v1 <-subset(snotel_df_v1, select=-c(X)) # move bad one
head(snotel_df_v1)

#########################
#########################
###  read in SNSR data ##
#########################
#########################

# read in csvs
snsr_snotel_list <-sort(list.files("./csvs/snsr_snotel_data", full.names = TRUE))
snsr_snotel_data <-lapply(snsr_snotel_list, read.csv)

# bind together
snsr_df <-bind_rows(snsr_snotel_data) # make df
snsr_df <-subset(snsr_df, select=-c(station_id)) # move bad one
colnames(snsr_df)[c(1,4,5)] <-c('site_name_v2','lat','lon')

# bind together
swe_df <-cbind(snotel_df, snsr_df)
head(swe_df)


#########################
#########################
##     max swe     ######
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


max_25 <-ggplot(data = max_df, mapping = aes(x = max_snotel_25.4, y = max_snsr_25.4)) +
  geom_pointdensity(adjust = .3, size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_color_viridis(option = "H") +
  scale_y_continuous(limits = c(0,3),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,3),expand = (c(0,0))) +
  xlab("SNOTEL Max SWE (m)") + ylab("SNSR Max SWE (m)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        legend.position = "none")

plot(max_25)

# save
ggsave( "./plots/max_metric_compare_v7.pdf",
       max_25,
       width = 4.5,
       height = 4.5,
       units = "in")

system("open ./plots/max_metric_compare_v7.pdf")


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


# plot
max_dowy_25 <-ggplot(data = max_dowy_df, mapping = aes(x = max_dowy_snotel_25.4, y = max_dowy_snsr_25.4)) +
  geom_pointdensity(adjust = 10, size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_color_viridis(option = "B") +
  scale_y_continuous(limits = c(50,300),expand = (c(0,3))) +
  scale_x_continuous(limits = c(50,300),expand = (c(0,3))) +
  xlab("SNOTEL Max DOWY") + ylab("SNSR Max DOWY") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        legend.position = "none")

plot(max_dowy_25)

# save
ggsave( "./plots/max_dowy_metric_compare_v6.pdf",
       max_dowy_25,
       width = 4.5,
       height = 4.5,
       units = "in")

system("open ./plots/max_dowy_metric_compare_v6.pdf")




#########################
#########################
######     sdd      #####
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

sdd_df[sapply(sdd_df, is.infinite)] <- NA

# plot
sdd_25 <-ggplot(sdd_df, aes(x = sdd_snotel_25.4, y = sdd_snsr_25.4)) +
  geom_pointdensity(adjust = 6, size = 1) +
  scale_color_viridis(option = "A") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_y_continuous(limits = c(50,366),expand = (c(0,0))) +
  scale_x_continuous(limits = c(50,366),expand = (c(0,0))) +
  xlab("SNOTEL SDD (DOWY)") + ylab("SNSR SDD (DOWY)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        legend.position = "none")

plot(sdd_25)

# save
ggsave( "./plots/sdd_metric_compare_v5.pdf",
        sdd_25,
        width = 4.5,
        height = 4.5,
        units = "in")

system("open ./plots/sdd_metric_compare_v5.pdf")



# #########################
# #########################
# ####  melt_rate_50  #####
# #########################
# #########################
# 
# # calc metric 
# melt_rate_df <-as.data.frame(swe_df %>%
#                          group_by(site_name, waterYear) %>%
#                          summarise(melt_rate_snotel_0 = melt_rate(snotel_swe_mm, swe_thres = 0),
#                                    melt_rate_snsr_0   = melt_rate(snsr_swe_mm, swe_thres = 0),
#                                    melt_rate_snotel_25.4 = melt_rate(snotel_swe_mm, swe_thres = 25.4),
#                                    melt_rate_snsr_25.4   = melt_rate(snsr_swe_mm, swe_thres = 25.4),
#                                    melt_rate_snotel_50.8 = melt_rate(snotel_swe_mm, swe_thres = 50.8),
#                                    melt_rate_snsr_50.8   = melt_rate(snsr_swe_mm, swe_thres = 50.8)))
# 
# melt_rate_df[sapply(melt_rate_df, is.infinite)] <- NA
# 
# # calc metric 
# melt_rate_50_df <-as.data.frame(swe_df %>%
#                                group_by(site_name, waterYear) %>%
#                                summarise(melt_rate_snotel_0 = melt_rate_50(snotel_swe_mm, swe_thres = 0),
#                                          melt_rate_snsr_0   = melt_rate_50(snsr_swe_mm, swe_thres = 0),
#                                          melt_rate_snotel_25.4 = melt_rate_50(snotel_swe_mm, swe_thres = 25.4),
#                                          melt_rate_snsr_25.4   = melt_rate_50(snsr_swe_mm, swe_thres = 25.4),
#                                          melt_rate_snotel_50.8 = melt_rate_50(snotel_swe_mm, swe_thres = 50.8),
#                                          melt_rate_snsr_50.8   = melt_rate_50(snsr_swe_mm, swe_thres = 50.8)))
# 
# melt_rate_50_df[sapply(melt_rate_50_df, is.infinite)] <- NA
# 
# 
# # plot
# melt_rate_50 <-ggplot(melt_rate_50_df) +
#   geom_abline(intercept = 0, slope = 1, linetype = 2) +
#   geom_point(aes(x = melt_rate_snotel_25.4, y = melt_rate_snsr_25.4), shape = 3, alpha = .7, size = 1.4, color = "deeppink4") +
#   scale_y_continuous(limits = c(0,40),expand = (c(0,0))) +
#   scale_x_continuous(limits = c(0,40),expand = (c(0,0))) +
#   xlab("SNOTEL Melt Rate 50 (mm/day)") + ylab("SNSR Melt Rate 50 (mm/day)") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))
# 
# # save
# ggsave( "./plots/melt_rate_50_metric_compare_v1.pdf",
#         melt_rate_50,
#         width = 4.5,
#         height = 4.5,
#         units = "in")
# 
# system("open ./plots/melt_rate_50_metric_compare_v1.pdf")


# #########################
# #########################
# ####  melt_rate_25  #####
# #########################
# #########################
# 
# # calc metric 
# melt_rate_25_df <-as.data.frame(swe_df %>%
#                                   group_by(site_name, waterYear) %>%
#                                   summarise(melt_rate_snotel_0 = melt_rate_25(snotel_swe_mm, swe_thres = 0),
#                                             melt_rate_snsr_0   = melt_rate_25(snsr_swe_mm, swe_thres = 0),
#                                             melt_rate_snotel_25.4 = melt_rate_25(snotel_swe_mm, swe_thres = 25.4),
#                                             melt_rate_snsr_25.4   = melt_rate_25(snsr_swe_mm, swe_thres = 25.4),
#                                             melt_rate_snotel_50.8 = melt_rate_25(snotel_swe_mm, swe_thres = 50.8),
#                                             melt_rate_snsr_50.8   = melt_rate_25(snsr_swe_mm, swe_thres = 50.8)))
# 
# melt_rate_25_df[sapply(melt_rate_25_df, is.infinite)] <- NA
# 
# 
# # plot
# melt_rate_25 <-ggplot(melt_rate_25_df) +
#   geom_abline(intercept = 0, slope = 1, linetype = 2) +
#   geom_point(aes(x = melt_rate_snotel_25.4, y = melt_rate_snsr_25.4), shape = 3, alpha = .7, size = 1.4, color = "darkviolet") +
#   scale_y_continuous(limits = c(0,40),expand = (c(0,0))) +
#   scale_x_continuous(limits = c(0,40),expand = (c(0,0))) +
#   xlab("SNOTEL Melt Rate 25 (mm/day)") + ylab("SNSR Melt Rate 25 (mm/day)") +
#   theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))
# 
# # save
# ggsave( "./plots/melt_rate_25_metric_compare_v1.pdf",
#         melt_rate_25,
#         width = 4.5,
#         height = 4.5,
#         units = "in")
# 
# system("open ./plots/melt_rate_25_metric_compare_v1.pdf")


#########################
#########################
####  melt_rate_33  #####
#########################
#########################

# calc metric 
melt_rate_33_df <-as.data.frame(swe_df %>%
                                  group_by(site_name, waterYear) %>%
                                  summarise(melt_rate_snotel_0 = melt_rate_33(snotel_swe_mm, swe_thres = 0),
                                            melt_rate_snsr_0   = melt_rate_33(snsr_swe_mm, swe_thres = 0),
                                            melt_rate_snotel_25.4 = melt_rate_33(snotel_swe_mm, swe_thres = 25.4),
                                            melt_rate_snsr_25.4   = melt_rate_33(snsr_swe_mm, swe_thres = 25.4),
                                            melt_rate_snotel_50.8 = melt_rate_33(snotel_swe_mm, swe_thres = 50.8),
                                            melt_rate_snsr_50.8   = melt_rate_33(snsr_swe_mm, swe_thres = 50.8)))

melt_rate_33_df[sapply(melt_rate_33_df, is.infinite)] <- NA

# plot
melt_rate_33 <-ggplot(melt_rate_33_df, aes(x = melt_rate_snotel_25.4, y = melt_rate_snsr_25.4)) +
  geom_pointdensity(adjust = 2, size = 1) +
  scale_color_viridis(option = "H") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  scale_y_continuous(limits = c(0,40),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,40),expand = (c(0,0))) +
  xlab("SNOTEL Melt Rate (mm/day)") + ylab("SNSR Melt Rate (mm/day)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        legend.position = "none")

plot(melt_rate_33)


# save
ggsave( "./plots/melt_rate_33_metric_compare_v2.pdf",
        melt_rate_33,
        width = 4.5,
        height = 4.5,
        units = "in")

system("open ./plots/melt_rate_33_metric_compare_v2.pdf")



##########################
##########################
##     mwa_djfm_total   ##
##########################
##########################

# calc metric 
mwa_djfm_df <-as.data.frame(swe_df %>%
                       group_by(site_name, waterYear) %>%
                       summarise(mwa_djfm_snotel_0 = mwa_djfm_total(snotel_swe_mm, swe_thres = 0),
                                 mwa_djfm_snsr_0   = mwa_djfm_total(snsr_swe_mm, swe_thres = 0),
                                 mwa_djfm_snotel_25.4 = mwa_djfm_total(snotel_swe_mm, swe_thres = 25.4),
                                 mwa_djfm_snsr_25.4   = mwa_djfm_total(snsr_swe_mm, swe_thres = 25.4),
                                 mwa_djfm_snotel_50.8 = mwa_djfm_total(snotel_swe_mm, swe_thres = 50.8),
                                 mwa_djfm_snsr_50.8   = mwa_djfm_total(snsr_swe_mm, swe_thres = 50.8)))

# plot
mwa_djfm_25 <-ggplot(mwa_djfm_df, aes(x = mwa_djfm_snotel_25.4, y = mwa_djfm_snsr_25.4)) +
  geom_pointdensity(adjust = 15, size = 1) +
  scale_color_viridis(option = "H") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_y_continuous(limits = c(0,500),expand = (c(0,5))) +
  scale_x_continuous(limits = c(0,500),expand = (c(0,5))) +
  xlab("SNOTEL MWA (mm)") + ylab("SNSR MWA (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        legend.position = "none")

# save
ggsave( "./plots/mwa_djfm_metric_compare_v6.pdf",
        mwa_djfm_25,
        width = 4.5,
        height = 4.5,
        units = "in")

system("open ./plots/mwa_djfm_metric_compare_v6.pdf")




##########################
##########################
##     mwa_djfm_days    ##
##########################
##########################

# calc metric 
mwa_djfm_days_df <-as.data.frame(swe_df %>%
                              group_by(site_name, waterYear) %>%
                              summarise(mwa_djfm_days_snotel_0 = mwa_djfm_days(snotel_swe_mm, swe_thres = 0),
                                        mwa_djfm_days_snsr_0   = mwa_djfm_days(snsr_swe_mm, swe_thres = 0),
                                        mwa_djfm_days_snotel_25.4 = mwa_djfm_days(snotel_swe_mm, swe_thres = 25.4),
                                        mwa_djfm_days_snsr_25.4   = mwa_djfm_days(snsr_swe_mm, swe_thres = 25.4),
                                        mwa_djfm_days_snotel_50.8 = mwa_djfm_days(snotel_swe_mm, swe_thres = 50.8),
                                        mwa_djfm_days_snsr_50.8   = mwa_djfm_days(snsr_swe_mm, swe_thres = 50.8)))



# plot
mwa_djfm_days_25 <-ggplot(mwa_djfm_days_df, aes(x = mwa_djfm_days_snotel_25.4, y = mwa_djfm_days_snsr_25.4)) +
  geom_pointdensity(adjust = 5, size = 1) +
  scale_color_viridis(option = "H") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_y_continuous(limits = c(0,70),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,70),expand = (c(0,0))) +
  xlab("SNOTEL MWA (# days)") + ylab("SNSR MWA (# days)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        legend.position = "none")

# save
ggsave( "./plots/mwa_djfm_days_metric_compare_v3.pdf",
        mwa_djfm_days_25,
        width = 4.5,
        height = 4.5,
        units = "in")

system("open ./plots/mwa_djfm_days_metric_compare_v3.pdf")



# stack with cow plot
plot_grid(max_25,max_dowy_25, sdd_25, melt_rate_33, mwa_djfm_25, mwa_djfm_days_25,
          labels = c("(a)","(b)","(c)","(d)","(e)","(f)"),
          align = "hv",
          vjust =  2,
          hjust = -.2,
          ncol = 3, 
          rel_widths = c(1/3, 1/3, 1/3))

ggsave("./plots/snsr_snotel_metric_compare_v6.pdf",
       width = 11, 
       height = 7,
       units = "in",
       dpi = 500)

system("open ./plots/snsr_snotel_metric_compare_v6.pdf")


#########################################
### calculate goodness of it metrics ####
#########################################

# define function that creates vector of metrics
gof_func <-function(snsr, snotel, metric_name){
  
  # calculate the 5 metrics
  corr <-round(hydroGOF::rPearson(snsr, snotel, na.rm = TRUE), digits = 2)
  r2 <-round(corr^2, digits = 2)
  rmse <-round(hydroGOF::rmse(snsr, snotel, na.rm = TRUE), digits = 2)
  mae <-round(hydroGOF::mae(snsr, snotel, na.rm = TRUE), digits = 2)
  me <-round(hydroGOF::me(snsr, snotel,na.rm = TRUE), digits = 2)
  pb <-round(hydroGOF::pbias(snsr, snotel, na.rm = TRUE), digits = 2)
  
  # bind
  row <-c(metric_name,corr,r2,rmse,mae,me,pb)
  return(row)
}



# create rows
metric_names <-c("Snow Metric", "R", "R^2", "RMSE", "MAE", "ME", "PB (%)")
max_stats <-gof_func(max_df$max_snsr_25.4, max_df$max_snotel_25.4, "Max SWE (m)")
max_dowy_stats <-gof_func(max_dowy_df$max_dowy_snsr_25.4, max_dowy_df$max_dowy_snotel_25.4, "Max SWE (DOWY)")
sdd_stats <-gof_func(sdd_df$sdd_snsr_25.4, sdd_df$sdd_snotel_25.4, "SDD (DOWY)")
melt_rate_stats <-gof_func(melt_rate_df$melt_rate_snsr_25.4, melt_rate_df$melt_rate_snotel_25.4, "Melt Rate (mm/day)")
melt_rate_50_stats <-gof_func(melt_rate_50_df$melt_rate_snsr_25.4, melt_rate_50_df$melt_rate_snotel_25.4, "Melt Rate 50 (mm/day)")
melt_rate_33_stats <-gof_func(melt_rate_33_df$melt_rate_snsr_25.4, melt_rate_33_df$melt_rate_snotel_25.4, "Melt Rate 33 (mm/day)")
melt_rate_25_stats <-gof_func(melt_rate_25_df$melt_rate_snsr_25.4, melt_rate_25_df$melt_rate_snotel_25.4, "Melt Rate 25 (mm/day)")
mwa_stats <-gof_func(mwa_djfm_df$mwa_djfm_snsr_25.4, mwa_djfm_df$mwa_djfm_snotel_25.4, "MWA (mm)")
mwa_days_stats <-gof_func(mwa_djfm_days_df$mwa_djfm_days_snsr_25.4, mwa_djfm_days_df$mwa_djfm_days_snotel_25.4, "MWA (# days)")

# make df 
table <-as.data.frame(rbind(metric_names, max_stats, max_dowy_stats, 
              sdd_stats, melt_rate_stats, melt_rate_50_stats, melt_rate_33_stats, 
              melt_rate_25_stats, mwa_stats, mwa_days_stats))

# rename cols
colnames(table)[1:7] <-table[1,]
table <-table[-1,]
table

# save
write.csv(table, "./csvs/snow_metric_error_metric_v2_melt_rate.csv", row.names = FALSE)
