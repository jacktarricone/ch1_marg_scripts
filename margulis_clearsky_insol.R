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
grad_mat <-cgrad(american_raster, cArea = FALSE) # compute normal vector for dem a 1m cell cize

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
  deltat <- 24 # [hours]
  
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

# create sequence of days to calculate
days_list <-as.list(seq(1,31,1))

# apply function to list of days
oct_list <-lapply(days_list, function(x) total_daily_solar(day = x, 
                                                           month = 10,
                                                           year = 2016,
                                                           raster_in = american_raster))

# apply function to list of days
nov_list <-lapply(days_list, function(x) total_daily_solar(day = x, 
                                                           month = 11,
                                                           year = 2016,
                                                           raster_in = american_raster))

# apply function to list of days
dec_list <-lapply(days_list, function(x) total_daily_solar(day = x, 
                                                           month = 12,
                                                           year = 2016,
                                                           raster_in = american_raster))

# apply function to list of days
jan_list <-lapply(days_list, function(x) total_daily_solar(day = x, 
                                                           month = 1,
                                                           year = 2016,
                                                           raster_in = american_raster))
# apply function to list of days
feb_list <-lapply(days_list, function(x) total_daily_solar(day = x, 
                                                           month = 2,
                                                           year = 2016,
                                                           raster_in = american_raster))

# run for mar
march_list <-lapply(days_list, function(x) total_daily_solar(day = x, 
                                                             month = 3,
                                                             year = 2016,
                                                             raster_in = american_raster))





#############################
# test plot, changes daily! #
#############################

# create raster stack from list
stack <-rast(oct_list,nov_list,dec_list,jan_list,feb_list,march_list)

# take the mean of feb 12-26
ondjfm_avg_solar <-app(stack, mean)

# mean test
global(ondjfm_avg_solar, mean, na.rm = TRUE)

# mask for avg metrics
mask <-rast("./ch1_margulis/rasters/snow_metric_averages/fm_mean_f_25mm_27obs.tif")
solar_mask <-mask(avg_solar_kwh, mask)
plot(solar_mask)

# save
writeRaster(solar_mask, "~/ch1_margulis/rasters/insolation/snsr_dem_insol_v2.tif")




