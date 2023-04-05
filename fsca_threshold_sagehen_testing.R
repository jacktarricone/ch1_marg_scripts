# testing making metric for
# date at which fsca goes below certain (15%?) threshold

library(rhdf5)
library(terra)
library(ggplot2)

#######################################################
##### code for bringing block to test functions on
#######################################################

# reset wd
setwd("/Volumes/jack_t/projects/ch1_margulis/") 

# metric creating with this script
snow_metric <-"fsca_thres"

# list sca files
sca_list <-list.files("./sca", full.names = TRUE)
print(sca_list) # static and wy2015 SCA

# set pth to biggest year and smallest year
path_1993 <-sca_list[9]
path_2015 <-sca_list[31]

# read in a block around sagehen for water year 1993
# pulled from QGIS using the cell number rasters generated
sagehen_fsca_wy93 <-h5read(path_1993, "/SCA", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent
sagehen_fsca_wy15 <-h5read(path_2015, "/SCA", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent
sagehen_fsca_wy16 <-h5read(sca_list[32], "/SCA", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent

# get swe data
# list sca files
swe_list <-list.files("~/ch1_margulis/swe/hdf", full.names = TRUE)
sagehen_swe_wy93 <-h5read(swe_list[9], "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent
sagehen_swe_wy15 <- h5read(swe_list[31], "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent
sagehen_swe_wy16 <- h5read(swe_list[32], "/SWE", index = list(2523:2621, 2947:3172, 1:365)) #sagehen extent

# # 93 swe vs. fsca
swe15 <-c(sagehen_swe_wy15[50,80,1:365]/10)
swe93 <-c(sagehen_swe_wy93[50,80,1:365]/10)
swe16 <-c(sagehen_swe_wy16[50,80,1:365]/10)

# pull out one pixel time series
sca15 <-c(sagehen_fsca_wy15[50,80,1:365])
sca93 <-c(sagehen_fsca_wy93[50,80,1:365])
sca16 <-c(sagehen_fsca_wy16[50,80,1:365])

# take means
# swe15 <-round(apply(sagehen_swe_wy15/10, 3, mean), digits = 2)
# swe16 <-round(apply(sagehen_swe_wy16/10, 3, mean), digits = 2)
# swe93 <-round(apply(sagehen_swe_wy93/10, 3, mean), digits = 2)
# 
# sca15 <-round(apply(sagehen_fsca_wy15, 3, mean), digits = 2)
# sca16 <-round(apply(sagehen_fsca_wy16, 3, mean), digits = 2)
# sca93 <-round(apply(sagehen_fsca_wy93, 3, mean), digits = 2)

# dowy
dowy <-seq(1,length(swe15_mean),1)
dowy366 <-seq(1,366,1)

# make df
plot_df <-as.data.frame(cbind(dowy,swe15,sca15,swe93,sca93,swe16,sca16))
plot_df

# test plots
ggplot(plot_df) +
  geom_line(aes(y = swe16, x = dowy), col = "darkred") +
  geom_line(aes(y = sca16, x = dowy), col = "darkblue") +  
  scale_y_continuous(limits = c(0,30),breaks = c(seq(0,100,25)), expand = (c(0,.2))) +
  scale_x_continuous(breaks = c(seq(0,350,50)), expand = (c(0,0))) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))
  # ylim(c(0,70)) +
  # xlim(c(230,260))

ggplot(plot_df) +
  geom_line(aes(y = swe15, x = dowy), col = "darkred") +
  geom_line(aes(y = sca15, x = dowy), col = "darkblue") +
  scale_y_continuous(limits = c(0,105),breaks = c(seq(0,100,25)), expand = (c(0,.2))) +
  scale_x_continuous(breaks = c(seq(0,350,50)), expand = (c(0,0))) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

ggplot(plot_df) +
  geom_line(aes(y = swe93, x = dowy), col = "darkred") +
  geom_line(aes(y = sca93, x = dowy), col = "darkblue") +
  scale_y_continuous(limits = c(0,105),breaks = c(seq(0,100,25)), expand = (c(0,.2))) +
  scale_x_continuous(breaks = c(seq(0,350,50)), expand = (c(0,0))) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))
  # ylim(c(0,70)) +
  # xlim(c(280,320))


## snotel test
camp <-as.data.frame(snotel_download(539, path = tempdir(), internal = TRUE))
camp_16 <-filter(camp, date >= "2015-10-01" & date <= "2016-09-30")
camp_15 <-filter(camp, date >= "2014-10-01" & date <= "2015-09-30")
camp_93 <-filter(camp, date >= "1992-10-01" & date <= "1993-09-30")

creek <-as.data.frame(snotel_download(540, path = tempdir(), internal = TRUE))
creek_16 <-filter(creek, date >= "2015-10-01" & date <= "2016-09-30")
creek_15 <-filter(creek, date >= "2014-10-01" & date <= "2015-09-30")
creek_93 <-filter(creek, date >= "1992-10-01" & date <= "1993-09-30")

lake <-as.data.frame(snotel_download(541, path = tempdir(), internal = TRUE))
lake_16 <-filter(lake, date >= "2015-10-01" & date <= "2016-09-30")
lake_15 <-filter(lake, date >= "2014-10-01" & date <= "2015-09-30")
lake_93 <-filter(lake, date >= "1992-10-01" & date <= "1993-09-30")


# test plots
ggplot() +
  geom_hline(yintercept = max(swe16)*.15, linetype = 3) +
  geom_text(aes(x = 317, y = 12, label = "85% SNSR SWE gone")) +
  geom_line(data = plot_df, mapping = aes(y = swe16, x = dowy, col = "SNSR Pixel"), linewidth = 1.3) +
  geom_line(data = plot_df, mapping = aes(y = sca16, x = dowy, col = "fSCA (%)"), linewidth = .8) + 
  geom_line(data = lake_16, mapping = aes(y = snow_water_equivalent/10, x = dowy366, color = "Indy Lake")) +
  geom_line(data = camp_16, mapping = aes(y = snow_water_equivalent/10, x = dowy366, color = "Indy Camp")) +
  geom_line(data = creek_16, mapping = aes(y = snow_water_equivalent/10, x = dowy366, color = "Indy Creek")) +
  scale_color_manual(name = "", values = c("SNSR Pixel" = "black", 
                                           "fSCA (%)" = "darkviolet",
                                           "Indy Lake" = 'red',
                                           "Indy Camp" = 'green',
                                           "Indy Creek" = 'blue')) +
  scale_y_continuous(limits = c(0,140),breaks = c(seq(0,125,25)), expand = (c(0,.2))) +
  scale_x_continuous(breaks = c(seq(0,350,50)), expand = (c(0,0))) +
  labs(y = "SWE (cm) & fSCA (%)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        legend.direction = "vertical", 
        legend.position = c(.88,.75),
        #legend.key = element_rect(size = 10),
        legend.key.size = unit(.5, 'lines'))

ggsave("~/ch1_margulis/plots/fsca_thres_plot_v1.pdf",
       width = 8,
       height = 5,
       units = "in")

system("open ~/ch1_margulis/plots/fsca_thres_plot_v1.pdf")



ggplot() +
    geom_hline(yintercept = (max(swe15)-max(swe15)*.9)) +
    geom_line(data = plot_df, mapping = aes(y = swe15, x = dowy), col = "black", linewidth = 1.3) +
    geom_line(data = plot_df, mapping = aes(y = sca15, x = dowy), linewidth = .8, linetype = 3) + 
    geom_line(data = lake_15, mapping = aes(y = snow_water_equivalent/10, x = dowy), color = 'red') +
    geom_line(data = camp_15, mapping = aes(y = snow_water_equivalent/10, x = dowy), color = 'green') +
    geom_line(data = creek_15, mapping = aes(y = snow_water_equivalent/10, x = dowy), color = 'blue') +
    scale_y_continuous(limits = c(0,75),breaks = c(seq(0,100,25)), expand = (c(0,.2))) +
    scale_x_continuous(breaks = c(seq(0,350,50)), expand = (c(0,0))) +
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))
  
ggplot() +
    geom_hline(yintercept = (max(swe93)*.15)) +
    geom_line(data = plot_df, mapping = aes(y = swe93, x = dowy), col = "black", linewidth = 1.3) +
    geom_line(data = plot_df, mapping = aes(y = sca93, x = dowy), linewidth = .8, linetype = 3) + 
    geom_line(data = lake_93, mapping = aes(y = snow_water_equivalent/10, x = dowy), color = 'red') +
    geom_line(data = camp_93, mapping = aes(y = snow_water_equivalent/10, x = dowy), color = 'green') +
    geom_line(data = creek_93, mapping = aes(y = snow_water_equivalent/10, x = dowy), color = 'blue') +
    scale_y_continuous(limits = c(0,175),breaks = c(seq(0,175,25)), expand = (c(0,.2))) +
    scale_x_continuous(breaks = c(seq(0,350,50)), expand = (c(0,0))) +
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

150-(150*.9)
max(swe16)-max(swe16)*.9

head(lake_15)
##### mid winter ablation function function
fsca_thres <-function(x, percent){
  
    if (max(x) < percent){
      return(NA)
    } 
    else{
      dowy <-as.numeric(max(which(x > percent)))
      return(dowy)
    }
  }

# test
fsca_thres(z, 25)

# test for wy2015
mat_wy15 <-apply(sagehen_wy15, c(1,2), function(x) fsca_thres(x, 15))
rast_wy15 <-rast(mat_wy15)
plot(rast_wy15)
# writeRaster(rast_wy15, "./snow_metric_rasters/terra_rasters/mwa/tests/sh_wy15.tif")

# test for wy1993
mat_wy93 <-apply(sagehen_wy93, c(1,2), function(x) fsca_thres(x, 15))
rast_wy93 <-rast(mat_wy93)
plot(rast_wy93)
# writeRaster(rast_wy93, "./snow_metric_rasters/terra_rasters/mwa/tests/sh_wy93.tif")

mat_wy16 <-apply(sagehen_fsca_wy16, c(1,2), function(x) fsca_thres(x, 15))
rast_wy16 <-rast(mat_wy16)
plot(rast_wy16)
hist(rast_wy16, breaks = 100)

# differene
diff <-rast_wy93-rast_wy15
plot(diff)

max_swe_dowy_raster <-function(x){
  
#### top half
top <- h5read(hdf_file, "/SWE", index = list(1:3300,1:5701,1:365))
top_max_dowy_mat <-as.matrix(apply(top, c(1,2), max_swe_dowy))
rm(top)

#### bottomhalf half
bottom <- h5read(hdf_file, "/SWE", index = list(3301:6601,1:5701,1:365))
bottom_max_dowy_mat <-as.matrix(apply(bottom, c(1,2), max_dowy))
rm(bottom)

#bind chunks together
full <-rbind(top_max_dowy_mat, bottom_max_dowy_mat)
rast <-raster(full, xmn=-123.3, xmx=-117.6, ymn=35.4, ymx=42, CRS("+proj=leac +ellps=clrk66"))
plot(rast)
hist(rast)

name <- gsub(".h5", "", hdf_name)
good_name <- gsub("SN_SWE_", "max_dowy_", name)

setwd("/Volumes/jt/projects/margulis/snow_metric_rasters/max_dowy/rasters")
writeRaster(rast, paste0(good_name, ".tif"))
return(rast)
}


