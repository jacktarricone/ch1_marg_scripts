### creating pixel-wise annual rasters for:
#### fwa: fracitonal winter ablation

# october 30, 2024
# jack tarricone

library(terra)

#set working directory
setwd("~/ch1_margulis")

# load in years for max and wa
max_list <-list.files("./rasters/snow_metrics/max_swe/years/", pattern = ".tif", full.names = TRUE)
max_stack <-rast(max_list)
max_stack

wa_list <-list.files("./rasters/snow_metrics/wa", pattern = ".tif", full.names = TRUE)
wa_stack <-rast(wa_list)
wa_stack[[9]]

# divide wa by max to get fractional winter ablation (fwa)
fwa_stack <-wa_stack/max_stack
fwa_stack
plot(fwa_stack[[31]])

test <-wa_stack[[31]]/max_stack[[31]]
test
hist(test, breaks = 100)

writeRaster(fwa_stack[[9]], "./rasters/snow_metrics/fwa/fwa_1993.tif")
writeRaster(fwa_stack[[31]], "./rasters/snow_metrics/fwa/fwa_2015.tif")

# create fwa file names
bname <-basename(wa_list)
fwa_names <-paste0("f", bname)
fwa_names

setwd("./rasters/snow_metrics/fwa/")

for (i in 1:length(fwa_names)){
  
  writeRaster(fwa_stack[[i]], fwa_names[[i]])
  
}