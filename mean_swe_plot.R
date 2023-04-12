# plot mean snotel vs. snsr

library(dplyr)
library(ggplot2)
library(lubridate)
library(cowplot)

setwd('~/ch1_margulis')

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

snotel_df_v1 <-read.csv("./csvs/snotel_df_v2.csv")
snotel_df_v1 <-subset(snotel_df_v1, select=-c(X, network)) # move bad one
head(snotel_df_v1)

# add NaN rows becaues horse meadow only has 357 days in 2004
# makes df exact same size

missing_days <-snotel_df_v1[rep(119430, 9),]
snotel_df <-rbind(snotel_df_v1[1:119430,], missing_days, snotel_df_v1[119431:nrow(snotel_df_v1),])


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
swe_df$Date <-ymd(swe_df$Date)
head(swe_df)

# create dowy col
get_waterYearDay <- function(x, wyMonth=10L) {
  
  doy <- lubridate::yday(x)  # day of year series
  year <- as.integer(lubridate::year(x))  # actual year series - this must be integer for the subtraction
  pyear <- year-1  # previous year series
  wy_date <- as.Date(paste(year,wyMonth,"01", sep = "-"))  # series with the date of the water year in the current year
  pwy_date <- as.Date(paste(pyear,wyMonth,"01", sep = "-"))  # series with the date of the water year in the previous year
  
  # the day of the year after the beginning of the water year can be calculated by subtracting dates:
  # if the current date is before the current water year date:
  #   year_day is the days between current date and the previous water year + 1 day
  # else (current date is after the current water year date):
  #   year_day is the days between current date and the actual water year + 1 day
  year_day <- ifelse(wy_date > x, x-pwy_date+1, x-wy_date+1)
  
  return(year_day)
  
}
swe_df$dowy <-get_waterYearDay(swe_df$Date)
tail(swe_df)

# calc metric 
snotel <-as.data.frame(swe_df %>%
                         group_by(dowy) %>%
                         summarise(snotel_mean = as.integer(mean(snotel_swe_mm))))

snsr <-as.data.frame(swe_df %>%
                         group_by(dowy) %>%
                         summarise(snsr_mean = as.integer(mean(snsr_swe_mm))))

mean_df <-left_join(snotel,snsr, by = "dowy")
head(mean_df)
mean_df

ggplot(mean_df) +
  geom_line(aes(x = dowy, y = snotel_mean, col = "SNOTEL Mean"), linewidth = 1) +
  geom_line(aes(x = dowy, y = snsr_mean, col = "SNSR Mean"), linewidth = 1) +
  scale_y_continuous(limits = c(0,600),expand = (c(0,10))) +
  scale_x_continuous(limits = c(0,366),expand = (c(0,0))) +
  scale_colour_manual(values=c("SNOTEL Mean"="midnightblue",'SNSR Mean'="darkorange"))+
  xlab("DOWY") + ylab("SWE (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1), 
        legend.position = c(.83,.80),
        legend.title = element_blank())

ggsave( "./plots/mean_swe_plot_v2.pdf",
        width = 6,
        height = 4,
        units = "in")

system("open ./plots/mean_swe_plot_v2.pdf")

ggplot(mean_df) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(aes(y = snsr_mean, x = snotel_mean, color = dowy), alpha = 2, size = 1.4) +
  scale_color_viridis_c() +
  scale_y_continuous(limits = c(0,600),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,600),expand = (c(0,0))) +
  xlab("SNOTEL SWE (mm)") + ylab("SNSR SWE (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        legend.position = c(.15,.8))

ggsave( "./plots/mean_swe_scatter_v1.pdf",
        width = 5,
        height = 5,
        units = "in")

system("open ./plots/mean_swe_scatter_v1.pdf")




