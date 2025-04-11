# pull out snotel pixels of SNSR hdf
# save per station per year

library(rhdf5)
library(terra)
library(dplyr)
library(parallel)
library(pbmcapply)
library(snotelr)
library(EflowStats)

# setwd
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


# bring in real snotel locations
snotel_locs <-read.csv("./csvs/SNOTEL_MASTER_pt_update.csv")
head(snotel_locs)

# read in stack
flm2020 <-project(rast('./rasters/flm_wy2020_30m_30_sdd.tif'),"EPSG:4326", method = "near")
plot(flm2020)

# stations in CA with 32 years of record in the SNSR
good_stations <-as.integer(c(356, 428, 462, 463, 473, 508, 518, 539,
                             540, 541, 575, 697, 724, 771, 778, 784, 809, 834, 846, 848))

# filter for CA
snotel_ca <-filter(snotel_locs, Site_ID %in% good_stations)
snotel_ca

# convert to vect
ca_points <-vect(snotel_ca, geom = c("Longitude","Latitude"), crs = crs(cell_numbers))
names(ca_points) <-snotel_ca$Site_Name
plot(ca_points, add = T)

#### extract pixel numbers
flm_sdd <-terra::extract(flm2020, ca_points, 
                                 names = TRUE, 
                                 cells = TRUE, 
                                 xy = TRUE, 
                                 ID = TRUE,
                                 method = 'simple')

flm_sdd$site_name <-snotel_ca$Site_Name
flm_sdd

# download snotel data
snotel_df <-snotel_download(
  good_stations,
  network = "sntl",
  metric = TRUE,
  internal = TRUE
)


# format and filter for 2020
snotel_df$date <-lubridate::ymd(snotel_df$date)
snotel_df$waterYear <-calcWaterYear(snotel_df$date)
snotel_2020 <-filter(snotel_df, waterYear == 2020)
snotel_2020$dowy <-get_waterYearDay(snotel_2020$date, wyMonth = 10L)

p1 <-ggplot(snotel_2020, aes(x = dowy, y = snow_water_equivalent, color = site_name))+
  geom_line(alpha = .6)+
  scale_y_continuous(expand = (c(0,10))) +
  scale_x_continuous(limits = c(0,280),expand = (c(0,1))) +
  xlab("DOWY") + ylab("SWE (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
      legend.position = "none")

ggsave( "./plots/flm_snotel_2020.pdf",
        p1,
        width = 6,
        height = 4,
        units = "in")

# filter for bad stations
bad_stations <-filter(snotel_2020, site_name == "spratt creek " | site_name == "leavitt meadows " |
                      site_name == "tahoe city cross ")

p2 <-ggplot(bad_stations, aes(x = dowy, y = snow_water_equivalent, color = site_name))+
  geom_line(alpha = .6)+
  #geom_text(aes(label = site_name), hjust = -0.1, vjust = 0, size = 3) +  
  scale_y_continuous(expand = (c(0,5))) +
  scale_x_continuous(limits = c(0,280),expand = (c(0,1))) +
  xlab("DOWY") + ylab("SWE (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        legend.position = "none")

ggsave( "./plots/flm_bad_stations.pdf",
       p2,
       width = 6,
       height = 4,
       units = "in")

# calc metric 
snotel_sdd <-as.data.frame(snotel_2020) %>%
  group_by(site_name, waterYear) %>%
  summarise(sdd_dowy  = sdd(snow_water_equivalent, swe_thres = 100))

snotel_sdd

# make new df for flm
flm_df1 <-flm_sdd[c(2,6)]
names(flm_df1)[1] <-"flm_sdd_dowy"
flm_df1$data <-rep("flm", nrow(flm_df1))
flm_df1$flm_sdd_dowy <-as.integer(flm_df1$flm_sdd_dowy)

# snotel
snotel_df1 <-as.data.frame(cbind(snotel_sdd$sdd_dowy,flm_df1$site_name,rep("snotel",nrow(flm_df1))))
names(snotel_df1)[1:3] <-c("snotel_sdd_dowy","site_name","data")
snotel_df1$snotel_sdd_dowy <-as.integer(snotel_df1$snotel_sdd_dowy)
snotel_df1

# creat plootting df
plot_df <-cbind(flm_df1,snotel_df1$snotel_sdd_dowy)
names(plot_df)[4] <-"snotel_sdd_dowy"
plot_df

#plot
p3 <-ggplot(plot_df, mapping = aes(x = snotel_sdd_dowy , y = flm_sdd_dowy)) +
  geom_point(alpha = .5)+
  geom_text(aes(label = site_name), hjust = -0.1, vjust = 0, size = 3) +  
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_y_continuous(limits = c(130,260),expand = (c(0,.0))) +
  scale_x_continuous(limits = c(130,260),expand = (c(0,.0))) +
  xlab("SNOTEL SDD (DOWY)") + ylab("FLM SDD (DOWY)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        aspect.ratio = 1,
        legend.position = "none")

ggsave( "./plots/flm_snotel_sdd_compare.pdf",
        p3,
        width = 6,
        height = 6,
        units = "in")





