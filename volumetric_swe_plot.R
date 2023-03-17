# plot SNSR volumetric SWE
# jack tarricone

library(dplyr)
library(ggplot2)

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

# list csvs
daily_vol_swe <-read.csv("./volume_results/SNSR_daily_vol_SWE.csv")
daily_vol_swe$date <-as.Date(daily_vol_swe$date)
daily_vol_swe

### calc stats
# mean
mean_vol_swe <-daily_vol_swe %>% 
               group_by(doWY) %>%   
               summarise(mean_vol_swe = mean(swe_vol_km3), .groups = 'drop')

# max
mean_max <-round(max(mean_vol_swe$mean_vol_swe), digits = 1)
mean_max

# max
max_vol_swe <-daily_vol_swe %>% 
              group_by(doWY) %>%   
              summarise(max_vol_swe = max(swe_vol_km3), .groups = 'drop')

# max
max_max <-round(max(max_vol_swe$max_vol_swe), digits = 1)
max_max

# min
min_vol_swe <-daily_vol_swe %>% 
              group_by(doWY) %>%   
              summarise(min_vol_swe = min(swe_vol_km3), .groups = 'drop')

# min
min_max <-round(max(min_vol_swe$min_vol_swe), digits = 1)
min_max

# sd
sd_vol_swe <-daily_vol_swe %>% 
  group_by(doWY) %>%   
  summarise(sd_vol_swe = sd(swe_vol_km3), .groups = 'drop')

# set ymin by band for sd
ymin_v1 <-mean_vol_swe$mean_vol_swe -(sd_vol_swe$sd_vol_swe)
ymin <-ifelse(ymin_v1 < 0, ymin_v1 == 0, ymin_v1)
ymin

# # 1993 test
# wy_93 <-filter(daily_vol_swe, wy == "1993")
# wy_15 <-filter(daily_vol_swe, wy == "2015")

# make labels
mean <-paste0(expression('Mean (16 '~(km^{"3"})))

# rough plot
ggplot(daily_vol_swe) +
  geom_line(aes(x= doWY, y = swe_vol_km3, group = wy), color = "gray40", alpha = .4, linewidth = .2) +
  # geom_line(data = wy_15, aes(x= doWY, y = swe_vol_km3), color = "black", linewidth = .9) +
  # geom_line(data = wy_93, aes(x= doWY, y = swe_vol_km3), color = "black", linewidth = .9) +
  geom_ribbon(data = mean_vol_swe, aes(x= doWY, y = mean_vol_swe,
                                       ymin = ymin, 
                                       ymax = mean_vol_swe + (sd_vol_swe$sd_vol_swe)), color = "grey80", alpha=.3) +
  geom_line(data = mean_vol_swe, aes(x= doWY, y = mean_vol_swe, color = "Mean"), linewidth = 1) +
  geom_line(data = max_vol_swe, aes(x= doWY, y = max_vol_swe, color = "Max"), linewidth = .8) +
  geom_line(data = min_vol_swe, aes(x= doWY, y = min_vol_swe, color = "Min"), linewidth = .8) +
  scale_colour_manual(values=c(Mean = "black","Min" = "red", "Max" = "darkblue"))+
  scale_y_continuous(limits = c(0,40),breaks = c(seq(0,40,5)), expand = (c(0,.2))) +
  scale_x_continuous(breaks = c(seq(0,350,50)), expand = (c(0,0))) +
  labs(y=(expression('Volumetric SWE'~(km^{"3"}))), x="Day of Water Year") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1), 
        legend.position = c(.83,.80),
        legend.title = element_blank())

ggsave("./plots/vol_swe_v2.pdf",
       width = 6,
       height = 4,
       units = "in")

system("open ./plots/vol_swe_v2.pdf")

  geom_ribbon(aes(ymin = mean-(sd), ymax = mean+(sd)), alpha=.3)+
  geom_line(aes(doWY,mean_06_16,color="06-16 Mean"),size = .9) +
  geom_line(aes(doWY,mean_95_05,color="95-05 Mean"),size = .9) +
  geom_line(aes(doWY,mean_85_94,color="85-94 Mean"),size = .9) +
  geom_line(aes(doWY,WY2015,color="2015 (Min)"),size = .9, linetype = "dashed") +
  geom_line(aes(doWY,WY1993,color="1993 (Max)"), size = .9, linetype = "dashed") +
  geom_line(aes(doWY,median,color="Median"), size = .9) +
  geom_line(aes(doWY,mean, color = "Mean"), size = 1.3) +
  scale_colour_manual(values=c(Mean="black",'2015 (Min)'="red",'1993 (Max)'="darkblue",
                               '06-16 Mean' = "turquoise3", '95-05 Mean' = "darkseagreen1",
                               Median = "grey45", '85-94 Mean' = "deeppink3"))+
  scale_x_continuous(breaks = seq(0,366,50)) +
  scale_y_continuous(breaks = seq(0,40,2)) +
  labs(title="CA Sierra Nevada Volumetric SWE (1985-2016)",
       y=(expression('Volumetric SWE'~(km^{"3"}))), x="Day of Water Year", color = "Water Year") +
  theme(
    legend.direction = "vertical",
    legend.position = c(.85,.75),
    legend.key = element_rect(size = 10),
    legend.key.size = unit(.5, 'lines'))


####max value test in 1993 (30ish km^3)
#read in lists of matrixes
dowy_155<-as.matrix(fread("/Volumes/john_tarricone/UNR_summer_20/margulis/swe/daily_data/WY_1993/c2/swe_1993_doWY_155.txt", header=T))

#try for 155th doWY 1993
max_rast <- raster(dowy_155, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42,) #set lat lon cords
crs(max_rast) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs" 
max_rast #inspect raster
image(max_rast) #print raster
writeRaster(max_rast, "/Volumes/john_tarricone/UNR_summer_20/margulis/volume_results/plots/max3.tif")

#2014 max doWY
max_14<-results %>%
  dplyr::select(c(WY_2014, doWY)) %>%
  slice(which.max(WY_2014))

#2009 max doWY
max_09<-results %>%
  dplyr::select(c(WY_2009, doWY)) %>%
  slice(which.max(WY_2009))

#2005 max doWY
max_05<-results %>%
  dplyr::select(c(WY_2005, doWY)) %>%
  slice(which.max(WY_2005))

max<-cbind(max_14,max_09,max_05)

april_1<-results %>% filter(doWY == 182) %>% dplyr::select(c(doWY, WY_2014, WY_2009, WY_2005))


#######color
#set theme first with this so you can edit it later
theme_set(theme_light(base_size =11))
y<-ggplot(results)+  
  geom_line(aes(doWY,WY_2016,color="2016")) +
  geom_line(aes(doWY,WY_2015,color="2015")) +
  geom_line(aes(doWY,WY_2014,color="2014")) +
  geom_line(aes(doWY,WY_2013,color="2013")) +
  geom_line(aes(doWY,WY_2012,color="2012")) +
  geom_line(aes(doWY,WY_2011,color="2011"))+
  geom_line(aes(doWY,WY_2010,color="2010")) +
  geom_line(aes(doWY,WY_2009,color="2009")) +
  geom_line(aes(doWY,WY_2008,color="2008")) +
  geom_line(aes(doWY,WY_2007,color="2007")) +
  geom_line(aes(doWY,WY_2006,color="2006")) +
  geom_line(aes(doWY,WY_2005,color="2005")) +
  geom_line(aes(doWY,WY_2004,color="2004")) +
  geom_line(aes(doWY,WY_2003,color="2003")) +
  geom_line(aes(doWY,WY_2002,color="2002")) +
  geom_line(aes(doWY,WY_2001,color="2001")) +
  geom_line(aes(doWY,mean, color = "Mean"), size = 1.5) +
  scale_colour_manual(values=c(Mean="black",'2016' ="coral3",'2015'="brown4",
                               '2014'="darkviolet",'2013'="darkslategray1",'2012'="darkseagreen1",
                               '2011'="deeppink3",'2010'="pink",'2009'="yellow",
                               '2008'="lawngreen",'2007'="tomato",'2006'="tan",
                               '2005'="lightblue",'2006'="darkblue",'2005'="red3",
                               '2004'="snow4",'2003'="darkcyan",'2002'="darkred",
                               '2001'="deepskyblue"))+
  scale_x_continuous(breaks = seq(0,366,50)) +
  scale_y_continuous(breaks = seq(0,30,2)) +
  labs(title="CA Sierra Nevada Volumetric SWE (2016-2001)", 
       y=(expression('Volumetric SWE'~(km^{"3"}))), x="Day of Water Year", color = "Water Year") +
  theme(
    legend.direction = "vertical", 
    legend.position = c(.85,.75),
    legend.key = element_rect(size = 10),
    legend.key.size = unit(.5, 'lines'))

print(y)




# #read in lists of matrixes   
# system.time(yearly_df_list <- lapply(filelist, function(x) fread(x, header=T))) 
# 
# #pull out single matrix
# results<-as.data.frame(yearly_df_list[[3]])
# 
# #full measn
# mean<-rowMeans(results[-1], na.rm = TRUE)
# results_2<-cbind(results,mean) #bind
# 
# #mean_06_16
# mean_06_16 <- rowMeans(results[2:12], na.rm = TRUE)
# mean_95_05 <- rowMeans(results[13:23], na.rm = TRUE)
# mean_85_94 <- rowMeans(results[24:33], na.rm = TRUE)
# sd<-apply(results[,2:33], 1, sd)
# median<-apply(results[,2:33],1, median)
# 
# results_final <-cbind(results_2, mean_85_94, mean_95_05, mean_06_16, median, sd) 
# 
# 
# fwrite(results_final, "/Volumes/jt/margulis/volume_results/SNSR_year_compare.csv")
# 
# #######grey
# #set theme first with this so you can edit it later
# theme_set(theme_light(base_size =11))
# y<-ggplot(results)+  
#   geom_line(aes(doWY,WY2016),color="snow4") +
#   geom_line(aes(doWY,WY2014),color="snow4") +
#   geom_line(aes(doWY,WY2013),color="snow4") +
#   geom_line(aes(doWY,WY2012),color="snow4") +
#   geom_line(aes(doWY,WY2011),color="snow4") +
#   geom_line(aes(doWY,WY2010),color="snow4") +
#   geom_line(aes(doWY,WY2009),color="snow4") +
#   geom_line(aes(doWY,WY2008),color="snow4") +
#   geom_line(aes(doWY,WY2007),color="snow4") +
#   geom_line(aes(doWY,WY2006),color="snow4") +
#   geom_line(aes(doWY,WY2005),color="snow4") +
#   geom_line(aes(doWY,WY2004),color="snow4") +
#   geom_line(aes(doWY,WY2003),color="snow4") +
#   geom_line(aes(doWY,WY2002),color="snow4") +
#   geom_line(aes(doWY,WY2001),color="snow4") +
#   geom_line(aes(doWY,WY2000),color="snow4") +
#   geom_line(aes(doWY,WY1999),color="snow4") +
#   geom_line(aes(doWY,WY1998),color="snow4") +
#   geom_line(aes(doWY,WY1997),color="snow4") +
#   geom_line(aes(doWY,WY1996),color="snow4") +
#   geom_line(aes(doWY,WY1995),color="snow4") +
#   geom_line(aes(doWY,WY1994),color="snow4") +
#   geom_line(aes(doWY,WY1992),color="snow4") +
#   geom_line(aes(doWY,WY1991),color="snow4") +
#   geom_line(aes(doWY,WY1990),color="snow4") +
#   geom_line(aes(doWY,WY1995),color="snow4") +
#   geom_line(aes(doWY,WY1989),color="snow4") +
#   geom_line(aes(doWY,WY1988),color="snow4") +
#   geom_line(aes(doWY,WY1987),color="snow4") +
#   geom_line(aes(doWY,WY1986),color="snow4") +
#   geom_line(aes(doWY,WY1985),color="snow4") +
#   geom_line(aes(doWY,mean_06_16,color="06-16 Mean"),size = .5) +
#   geom_line(aes(doWY,mean_95_05,color="95-05 Mean"),size = .5) +
#   geom_line(aes(doWY,mean_85_94,color="85-94 Mean"),size = .5) +
#   geom_line(aes(doWY,WY2015,color="2015 (Min)"),size = .9) +
#   geom_line(aes(doWY,WY2015,color="2015 (Min)"),size = .9) +
#   geom_line(aes(doWY,WY1993,color="1993 (Max)"), size = .9) +
#   geom_line(aes(doWY,mean, color = "Mean"), size = 1.3) +
#   geom_line(aes(doWY,median, color = "Median"), size = 1.3) +
#   scale_colour_manual(values=c(Mean="black",'2015 (Min)'="red",'1993 (Max)'="darkblue",
#                                '06-16 Mean' = "green", '95-05 Mean' = "yellow", 
#                                Median = "cyan",'85-94 Mean' = "purple"))+
#   scale_x_continuous(breaks = seq(0,366,50)) +
#   scale_y_continuous(breaks = seq(0,40,2)) +
#   labs(title="CA Sierra Nevada Volumetric SWE (1985-2016)", 
#        y=(expression('Volumetric SWE'~(km^{"3"}))), x="Day of Water Year", color = "Water Year") +
#   theme(
#     legend.direction = "vertical", 
#     legend.position = c(.85,.75),
#     legend.key = element_rect(size = 10),
#     legend.key.size = unit(.5, 'lines'))
# 
# print(y)
# 
# 
# print(y)
