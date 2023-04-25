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

snotel_df_v1 <-read.csv("./csvs/snotel_df_v3.csv")
snotel_df <-subset(snotel_df_v1, select=-c(X)) # move bad one
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
  xlab("SNOTEL DJFM MWA (mm)") + ylab("SNSR MWA DJFM (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        legend.position = "none")

# save
ggsave( "./plots/mwa_djfm_metric_compare_v7.pdf",
        mwa_djfm_25,
        width = 4.5,
        height = 4.5,
        units = "in")

system("open ./plots/mwa_djfm_metric_compare_v7.pdf")



##########################
##########################
#######     mwa    #######
##########################
##########################

# calc metric 
mwa_df <-as.data.frame(swe_df %>%
                              group_by(site_name, waterYear) %>%
                              summarise(mwa_snotel_0 = mwa(snotel_swe_mm, swe_thres = 0),
                                        mwa_snsr_0   = mwa(snsr_swe_mm, swe_thres = 0),
                                        mwa_snotel_25.4 = mwa(snotel_swe_mm, swe_thres = 25.4),
                                        mwa_snsr_25.4   = mwa(snsr_swe_mm, swe_thres = 25.4),
                                        mwa_snotel_50.8 = mwa(snotel_swe_mm, swe_thres = 50.8),
                                        mwa_snsr_50.8   = mwa(snsr_swe_mm, swe_thres = 50.8)))

# plot
mwa_25 <-ggplot(mwa_df, aes(x = mwa_snotel_25.4, y = mwa_snsr_25.4)) +
  geom_pointdensity(adjust = 15, size = 1) +
  scale_color_viridis(option = "H") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_y_continuous(limits = c(0,500),expand = (c(0,5))) +
  scale_x_continuous(limits = c(0,500),expand = (c(0,5))) +
  xlab("SNOTEL MWA (mm)") + ylab("SNSR MWA (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        legend.position = "none")

# save
ggsave( "./plots/mwa_metric_compare_v1.pdf",
        mwa_25,
        width = 4.5,
        height = 4.5,
        units = "in")

system("open ./plots/mwa_metric_compare_v1.pdf")




# stack with cow plot
plot_grid(mwa_djfm_25, mwa_25,
          labels = c("(a)","(b)"),
          align = "hv",
          vjust =  2,
          hjust = -.2,
          ncol = 2, 
          rel_widths = c(1/2, 1/2))

ggsave("./plots/mwa_metric_compare_v1.pdf",
       width = 9, 
       height = 4.5,
       units = "in",
       dpi = 600)

system("open ./plots/mwa_metric_compare_v1.pdf")


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
mwa_djfm_stats <-gof_func(mwa_djfm_df$mwa_djfm_snsr_25.4, mwa_djfm_df$mwa_djfm_snotel_25.4, "MWA DJFM (mm)")
mwa_stats <-gof_func(mwa_df$mwa_snsr_25.4, mwa_df$mwa_snotel_25.4, "MWA (mm)")


# make df 
table <-as.data.frame(rbind(metric_names, mwa_djfm_stats, mwa_stats))
table

# rename cols
colnames(table)[1:7] <-table[1,]
table <-table[-1,]
table

# save
write.csv(table, "./csvs/snow_metric_error_metric_v2_melt_rate.csv", row.names = FALSE)
