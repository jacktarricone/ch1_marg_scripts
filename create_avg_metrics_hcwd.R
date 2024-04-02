### create average SNSR and gridMET metrics
# june 20th, 2023
# jack tarricone

library(terra)
library(dplyr)

# set working directory
setwd("~/ch1_margulis")

# define mean function with na.rm
metric_mean <-function(x){terra::mean(x, na.rm = TRUE)}

# bring in shape files
snsr <-vect("./vectors/snsr_shp.gpkg")
snsr_basins <-vect("./vectors/ca_basins/snsr_all_basins.shp")

## hydro cat 
hydro_cat <-read.csv("./csvs/hydro_cat_years.csv")
hydro_cat$lyr <-seq(1,32,1) # lyr for subsetting
hydro_cat

# filter for four quads
cd <-filter(hydro_cat, hydro_cat == "cd")
cw <-filter(hydro_cat, hydro_cat == "cw")
hd <-filter(hydro_cat, hydro_cat == "hd")
hw <-filter(hydro_cat, hydro_cat == "hw")

metric_stack <-fm_stack
name <-"fm"

# define function
metric_hydro_cat_mean <-function(metric_stack,name){
  
  # subet layers into 4 quads
  metric_cd <-subset(metric_stack, cd$lyr)
  metric_cw <-subset(metric_stack, cw$lyr)
  metric_hd <-subset(metric_stack, hd$lyr)
  metric_hw <-subset(metric_stack, hw$lyr)

  # take mean per each quad
  metric_cd_mean <-app(metric_cd, fun = metric_mean, cores=14)
  names(metric_cd_mean) <-"metric_cd_mean"
  print("cd done!")
  metric_cw_mean <-app(metric_cw, fun = metric_mean, cores=14)
  names(metric_cw_mean) <-"metric_cw_mean"
  print("cw done!")
  metric_hd_mean <-app(metric_hd, fun = metric_mean, cores=14)
  names(metric_hd_mean) <-"metric_hd_mean"
  print("hd done!")
  metric_hw_mean <-app(metric_hw, fun = metric_mean, cores=14)
  names(metric_hw_mean) <-"metric_hw_mean"
  print("hw done!")

  # stack for saving
  stack_list <-list(metric_cd_mean,
                  metric_cw_mean,
                  metric_hd_mean,
                  metric_hw_mean)
  
  # naming stuff
  names_list1 <-c("metric_cd_mean",
                  "metric_cw_mean",
                  "metric_hd_mean",
                  "metric_hw_mean")
  
  names_list <-gsub("metric",name,names_list1)

  # save
  for (i in 1:4){
   name <-names(stack_list[[i]])
   writeRaster(stack_list[[i]], paste0("./rasters/snow_metric_averages/hydro_cat/",names_list[[i]],".tif"))
  }
  
}


##############
##### max ####
##############

# load in stack
max_stack <-rast("./rasters/snow_metrics/max_swe/max_stack_f_25mm_27obs.tif")
names(max_stack) <-hydro_cat$years
max_stack

# metric_hydro_cat_mean(max_stack)

####################
####### mwa ########
####################

# load in stack
mwa_paths <-list.files("./rasters/snow_metrics/mwa_ondjfm_mm/", pattern = ".tif", full.names = TRUE)
mwa_stack <-rast(mwa_paths)

# metric_hydro_cat_mean(mwa_stack,"mwa")

####################
####### fm #########
####################

# load in stack
fm_stack <-rast("./rasters/snow_metrics/fm_apr1/fm_stack_f_25mm_27obs.tif")

# run
# metric_hydro_cat_mean(fm_stack, "fm")

####################
#####   dom    ####
####################

# load in stack
dom_stack <-rast("./rasters/snow_metrics/max_swe_dowy/dom_stack_f_25mm_27obs.tif")

# metric_hydro_cat_mean(dom_stack,"dom")

####################
#####   tmean   ####
####################

# load in stack
tmean_stack <-rast("./rasters/gridmet/tmean/tmean_stack.tif")

# metric_hydro_cat_mean(tmean_stack,"tmean")


tmean_list <-list.files("./rasters/snow_metric_averages/hydro_cat/", pattern = "*tmean", full.names = T)
tmean_stack <-rast(tmean_list)
plot(tmean_stack)
