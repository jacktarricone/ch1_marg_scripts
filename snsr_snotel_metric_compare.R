# comparing max swe to SNOTELS

library(terra)
library(dplyr)
library(ggplot2)
library(snotelr)
library(lubridate)

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
snsr_df <-bind_rows(snsr_snotel_data)
colnames(snsr_df)[1] <-'site_name_v2'

# test to see if ID numbers are the same
true <-ifelse(snotel_df$site_id == snotel_df$site_id, TRUE, FALSE)

#########################
#########################
##     max swe     ##
#########################
#########################

# calc snotel max 
max_snotel_df <-as.data.frame(snotel_df %>%
                              group_by(site_name, waterYear) %>%
                              summarise(max_snotel_swe_mm = max_swe(snotel_swe_mm, swe_thres = 25.4)))

# calc snotel max 
max_snsr_df <-as.data.frame(snsr_df %>%
                            group_by(site_name_v2, wy) %>%
                            summarise(max_snsr_swe_mm = max_swe(snsr_swe_mm, swe_thres = 25.4)))

# bind for plotting
max_df <-cbind(max_snotel_df, max_snsr_df)
head(max_df)

# plot
ggplot(max_df) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(aes(x = max_snotel_swe_mm, y = max_snsr_swe_mm), size = .9, color = "darkred") +
  scale_y_continuous(limits = c(0,3000),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,3000),expand = (c(0,0))) +
  xlab("SNOTEL Max (mm)") + ylab("SNSR Max SWE (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

# calc correlation
cor(max_df$max_snotel_swe_mm, max_df$max_snsr_swe_mm, use = "complete.obs")

#########################
#########################
##     max dowy swe    ##
#########################
#########################

# calc snotel 
max_dowy_snotel_df <-as.data.frame(snotel_df %>%
                                group_by(site_name, waterYear) %>%
                                summarise(max_dowy_snotel = max_swe_dowy(snotel_swe_mm, swe_thres = 0)))

# calc snsr 
max_dowy_snsr_df <-as.data.frame(snsr_df %>%
                              group_by(site_name_v2, wy) %>%
                              summarise(max_dowy_snsr = max_swe_dowy(snsr_swe_mm, swe_thres = 0)))

# bind for plotting
max_dowy_df <-cbind(max_dowy_snotel_df, max_dowy_snsr_df)
head(max_dowy_df)

# plot
ggplot(max_dowy_df) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(aes(x = max_dowy_snotel, y = max_dowy_snsr), shape = 3, size = .9, color = "darkviolet") +
  scale_y_continuous(limits = c(50,300),expand = (c(0,0))) +
  scale_x_continuous(limits = c(50,300),expand = (c(0,0))) +
  xlab("SNOTEL Max DOWY") + ylab("SNSR Max DOWY") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

# calc correlation
cor(max_dowy_df$max_dowy_snotel, max_dowy_df$max_dowy_snsr, use = "complete.obs")


#########################
#########################
##        sdd          ##
#########################
#########################

# calc snotel sdd 
sdd_snotel_df <-as.data.frame(snotel_df %>%
                                group_by(site_name, waterYear) %>%
                                summarise(sdd_snotel = sdd(snotel_swe_mm, swe_thres = 100)))

# calc snotel sdd 
sdd_snsr_df <-as.data.frame(snsr_df %>%
                              group_by(site_name_v2, wy) %>%
                              summarise(sdd_snsr = sdd(snsr_swe_mm, swe_thres = 100)))

# bind for plotting
sdd_df <-cbind(sdd_snotel_df, sdd_snsr_df)
head(sdd_df)

# plot
ggplot(sdd_df) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(aes(x = sdd_snotel, y = sdd_snsr), shape =3, size = .9, color = "black") +
  scale_y_continuous(limits = c(100,365),expand = (c(0,0))) +
  scale_x_continuous(limits = c(100,365),expand = (c(0,0))) +
  xlab("SNOTEL SDD (dowy)") + ylab("SNSR SDD SWE (dowy)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

# calc correlation
cor(sdd_df$sdd_snotel, sdd_df$sdd_snsr, use = "complete.obs")

#########################
#########################
##     melt_rate      ##
#########################
#########################

# calc snotel melt_rate 
melt_rate_snotel_df <-as.data.frame(snotel_df %>%
                                group_by(site_name, waterYear) %>%
                                summarise(melt_rate_snotel_swe_mm = melt_rate(snotel_swe_mm, swe_thres = 0)))

# calc snotel melt_rate 
melt_rate_snsr_df <-as.data.frame(snsr_df %>%
                              group_by(site_name_v2, wy) %>%
                              summarise(melt_rate_snsr_swe_mm = melt_rate(snsr_swe_mm, swe_thres = 25.4)))

# bind for plotting
melt_rate_df <-cbind(melt_rate_snotel_df, melt_rate_snsr_df)
head(melt_rate_df)

# plot
ggplot(melt_rate_df) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(aes(x = melt_rate_snotel_swe_mm, y = melt_rate_snsr_swe_mm), shape =3, size = .9, color = "darkred") +
  scale_y_continuous(limits = c(0,20),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,20),expand = (c(0,0))) +
  xlab("SNOTEL Melt Rate (mm/day)") + ylab("SNSR Melt Rate (mm/day)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

# calc correlation
cor(melt_rate_df$melt_rate_snotel_swe_mm, melt_rate_df$melt_rate_snsr_swe_mm, use = "complete.obs")
