### rkt test with rando sample
# january 22nd 2022
# jack tarricone

library(terra)
library(data.table)

# set working directory
setwd("~/ch1_margulis")

# bring in shape files
snsr <-vect("./vectors/snsr_shp.gpkg")
snsr_basins <-vect("./vectors/ca_basins/snsr_all_basins.shp")
american <-vect("./vectors/ca_basins/")

##############
##### max ####
##############

# load in stack
max_stack <-rast("./rasters/snow_metrics/max_swe/max_stack_f_25mm_27obs.tif")
max_stack

# 
max_stack_df <-as.data.frame(max_stack, xy = TRUE, cell = TRUE)
fwrite(max_stack_df, "./csvs/max_stack_df.csv")