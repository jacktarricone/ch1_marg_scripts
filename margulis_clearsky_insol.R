# margulis clear sky insolation
# jack tarricone
# may 10th, 2023

library(raster)
library(terra)
library(insol)

####################################
####################################
####################################

# bring in raster
dem <-rast("~/ch1_margulis/rasters/static/SNSR_DEM.tif")
american_shp <-vect("~/ch1_margulis/vectors/ca_basins/american.gpkg")
american <-mask(crop(dem, american_shp), american_shp)

# insolation for usj
# convert from SpatRast to raster for calc
american_raster <-raster(american)
dem_raster <-raster(dem)
grad_mat <-cgrad(dem_raster, cArea = FALSE) # compute normal vector for dem a 1m cell cize

total_daily_solar <-function(day,month,year,raster_in){

  # make julian day for
  jd <- JDymd(year = year,
              month = month,
              day = day,
              hour = 0)
  
  # lat lon of margulis
  lat <- 39
  lon <- -120.4
  tmz <- -7
  
  # compute day length on day 1
  sun_timing <-daylength(lat,lon,jd,tmz)
  sunrise <-sun_timing[1]
  sunset <-sun_timing[2]
  print(sun_timing)
  
  # time interval
  deltat <- 1 # [hours]
  
  # create arrays for da loop
  nrow <-nrow(raster_in)
  ncol <-ncol(raster_in)
  nlay <-length(seq(sunrise,sunset,deltat)) # number of layers given by deltat
  
  # defining the two empty arraus to loop into
  Iglobal <-array(0,dim=dim(raster_in))
  Iglobal_v2 <-array(numeric(),c(nrow,ncol,nlay))
  
  ## define variables for insolation function
  height <-2600 # height asl [m]
  visibility <-90 # [km], guess
  RH <-50 #2/12 @ 11am
  tempK <- 278 # [kelvin]
  ozone_thickness <-.03 # [cm]
  landscape_alebdo <-.5 
  
  for (i in 1:nlay){
    hours <-seq(sun_timing[1],sun_timing[2],deltat) # sequence by half hour from sunrise to sunset
    layers <-seq(1,nlay,1) # vector to iterate through for layer creatation
    jd = JDymd(year = year,
               month = month,
               day = day, hour=hours[i]) # jd value for precise half hour time step
    sv = sunvector(jd,lat,lon,tmz) # sun vector based of position, day, and timezone
    hsh = as.array(hillshading(grad_mat,sv),dim=dim(raster_in)) # create hillshade
    sh = doshade(raster_in,sv) # shade
    zenith = sunpos(sv)[2] # calculate solar zenith angle
    
    ## direct radiation modified by terrain + diffuse irradiation (skyviewfactor ignored)
    # half hour timestep
    # values in J/m^2
    Idirdif = insolation(zenith,
                         jd,
                         height,
                         visibility,
                         RH,tempK,
                         ozone_thickness,
                         landscape_alebdo)
    
    # loop each half hour time step into array by layer
    Iglobal_v2[,,layers[i]] <- Iglobal[,,] + (Idirdif[1,1] * hsh + Idirdif[1,2])*3600*deltat
  }
  
  # convert array to rast
  solar_rad_hourly_j <-rast(Iglobal_v2)
  crs(solar_rad_hourly_j) <-crs(rast(raster_in)) # set crs from orginal dem
  ext(solar_rad_hourly_j) <-ext(rast(raster_in)) # set ext 
  
  # sum layers pixel-wise for total daily in Joules/m^2
  solar_rad_total_j <-app(solar_rad_hourly_j, sum)
  
  # convert to kilowatt-horus/m^2
  solar_rad_total_kwh <-solar_rad_total_j/3.6e6
  
  # return daily total in kWh/m^2 insolation
  return(solar_rad_total_kwh)
}


# apply function to list of days
oct_list <-lapply(as.list(seq(1,31,1)), function(x) total_daily_solar(day = x, 
                                                           month = 10,
                                                           year = 2016,
                                                           raster_in = dem_raster))

# apply function to list of days
nov_list <-lapply(as.list(seq(1,30,1)), function(x) total_daily_solar(day = x, 
                                                           month = 11,
                                                           year = 2016,
                                                           raster_in = dem_raster))

# apply function to list of days
dec_list <-lapply(as.list(seq(1,31,1)), function(x) total_daily_solar(day = x, 
                                                           month = 12,
                                                           year = 2016,
                                                           raster_in = dem_raster))

# apply function to list of days
jan_list <-lapply(as.list(seq(1,31,1)), function(x) total_daily_solar(day = x, 
                                                           month = 1,
                                                           year = 2016,
                                                           raster_in = dem_raster))
# apply function to list of days
feb_list <-lapply(as.list(seq(1,29,1)), function(x) total_daily_solar(day = x, 
                                                           month = 2,
                                                           year = 2016,
                                                           raster_in = dem_raster))

# run for mar
march_list <-lapply(as.list(seq(1,31,1)), function(x) total_daily_solar(day = x, 
                                                             month = 3,
                                                             year = 2016,
                                                             raster_in = dem_raster))


# #############################
# # test plot, changes daily! #
# #############################
# 
# # create stacks from each month and save
# # oct
# oct_stack <-rast(oct_list)
# writeRaster(oct_stack, "~/ch1_margulis/rasters/insolation/oct_insol_stack_v1.tif")
# 
# 
# # nov
# nov_stack <-rast(nov_list)
# writeRaster(nov_stack, "~/ch1_margulis/rasters/insolation/nov_insol_stack_v1.tif")
# 
# 
# # dec
# dec_stack <-rast(dec_list)
# writeRaster(dec_stack, "~/ch1_margulis/rasters/insolation/dec_insol_stack_v1.tif")
# 
# 
# # jan
# jan_stack <-rast(jan_list)
# writeRaster(jan_stack, "~/ch1_margulis/rasters/insolation/jan_insol_stack_v1.tif")
# 
# 
# # feb
# feb_stack <-rast(feb_list)
# writeRaster(feb_stack, "~/ch1_margulis/rasters/insolation/feb_insol_stack_v1.tif")
# 
# 
# # march
# march_stack <-rast(march_list)
# writeRaster(march_stack, "~/ch1_margulis/rasters/insolation/march_insol_stack_v1.tif")

insol_list <-list.files("~/ch1_margulis/rasters/insolation", full.names = TRUE, pattern = '*stack')
insol_stack <-rast(insol_list)
writeRaster(insol_stack, "~/ch1_margulis/rasters/insolation/snsr_insol_stack_v1.tif")

# take the mean of feb 12-26
ondjfm_avg_solar <-app(insol_stack, mean, cores = 14)
plot(ondjfm_avg_solar)
writeRaster(ondjfm_avg_solar, "~/ch1_margulis/rasters/insolation/snsr_dem_insol_v1.tif")

# mean test
global(ondjfm_avg_solar, mean, na.rm = TRUE)

# mask for avg metrics
mask <-rast("~/ch1_margulis/rasters/snow_metric_averages/fm_mean_f_25mm_27obs.tif")
solar_mask <-mask(ondjfm_avg_solar, mask)
plot(solar_mask)

# save
writeRaster(solar_mask, "~/ch1_margulis/rasters/insolation/snsr_dem_insol_masked_v1.tif")




