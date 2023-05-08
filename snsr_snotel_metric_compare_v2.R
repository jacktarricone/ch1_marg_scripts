# comparing SNSR and SNOTEL snow metrics
# jack tarricone

library(dplyr)
library(ggplot2)
library(lubridate)
library(hydroGOF)
library(cowplot)
library(viridis)
library(ggpointdensity)

# set wd
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

snotel_df <-read.csv("./csvs/snotel_df_v3.csv")
snotel_df <-subset(snotel_df, select=-c(X)) # move bad one
head(snotel_df)

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
  geom_pointdensity(adjust = .1, size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_color_viridis(option = "H") +
  scale_y_continuous(limits = c(0,3),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,3),expand = (c(0,0))) +
  xlab("SNOTEL Max SWE (m)") + ylab("SNSR Max SWE (m)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        aspect.ratio = 1,
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
  geom_pointdensity(adjust = 5, size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_color_viridis(option = "H") +
  scale_y_continuous(limits = c(50,300),expand = (c(0,3))) +
  scale_x_continuous(limits = c(50,300),expand = (c(0,3))) +
  xlab("SNOTEL DOM (DOWY)") + ylab("SNSR DOM (DOWY)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        aspect.ratio = 1,
        legend.position = "none")

plot(max_dowy_25)

# save
ggsave( "./plots/max_dowy_metric_compare_v6.pdf",
       max_dowy_25,
       width = 4.5,
       height = 4.5,
       units = "in")

system("open ./plots/max_dowy_metric_compare_v6.pdf")

##########################
##########################
##     fm   ##
##########################
##########################

# calc metric 
fm_df <-as.data.frame(swe_df %>%
                       group_by(site_name, waterYear) %>%
                       summarise(fm_snotel_0 = fm_apr1(snotel_swe_mm, swe_thres = 0),
                                 fm_snsr_0   = fm_apr1(snsr_swe_mm, swe_thres = 0),
                                 fm_snotel_25.4 = fm_apr1(snotel_swe_mm, swe_thres = 25.4),
                                 fm_snsr_25.4   = fm_apr1(snsr_swe_mm, swe_thres = 25.4),
                                 fm_snotel_50.8 = fm_apr1(snotel_swe_mm, swe_thres = 50.8),
                                 fm_snsr_50.8   = fm_apr1(snsr_swe_mm, swe_thres = 50.8)))

# plot
fm_25 <-ggplot(fm_df, aes(x = fm_snotel_25.4, y = fm_snsr_25.4)) +
  geom_pointdensity(adjust = 1, size = 1) +
  scale_color_viridis(option = "H") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,1),expand = (c(0,.02))) +
  xlab("SNOTEL FM") + ylab("SNSR FM") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        aspect.ratio = 1,
        legend.position = "none")

plot(fm_25)

# save
ggsave( "./plots/fm_metric_compare_v1.pdf",
        fm_25,
        width = 4.5,
        height = 4.5,
        units = "in")

system("open ./plots/fm_metric_compare_v1.pdf")



# stack with cow plot
plot_grid(max_25,max_dowy_25, fm_25,
          labels = c("(a)","(b)","(c)"),
          align = "hv",
          vjust =  2,
          hjust = -.2,
          ncol = 3, 
          rel_widths = c(1/3, 1/3, 1/3))

ggsave("./plots/snsr_snotel_metric_compare_v10.pdf",
       width = 13.5, 
       height = 4.5,
       units = "in",
       dpi = 500)

system("open ./plots/snsr_snotel_metric_compare_v10.pdf")


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
fm_stats <-gof_func(fm_df$fm_snsr_25.4, fm_df$fm_snotel_25.4, "FM")

# make df 
table <-as.data.frame(rbind(metric_names, max_stats, max_dowy_stats, fm_stats))

# rename cols
colnames(table)[1:7] <-table[1,]
table <-table[-1,]
table

# save
write.csv(table, "./csvs/snow_metric_error_metric_v4.csv", row.names = FALSE)
