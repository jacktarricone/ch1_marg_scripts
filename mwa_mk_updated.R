# mwa mk trend analysis
# march 31, 2023


library(terra)
library(spatialEco)



# read in scf rasters to stack
setwd("~/ch1_margulis/")
rast_list <-list.files("./rasters/mwa/", full.names = TRUE)
mwa_stack <-rast(rast_list)

# read in tahoe crop shape
tahoe_crop <-vect("./vectors/tahoe_crop_mk.geojson")
tahoe_crop
mwa_crop <-crop(mwa_stack, tahoe_crop)

# calculate number of non na obs per pixel
n_obs <-app(mwa_crop, function(x) sum(!is.na(x)))
plot(n_obs)

# mask for all values below 10
masking_value <-subst(n_obs, c(0:10), NA)
plot(masking_value)

# if there are less than 10 obsevations per pixel, make NA
mwa_crop_v2 <-mask(mwa_crop, masking_value, maskvalues = NA)
mwa_crop_v2

# calculate number of non na obs per pixel
n_obs_new <-app(mwa_crop_v2, function(x) sum(!is.na(x)))
n_obs_new_v2 <-subst(n_obs_new, 0, NA)
min(n_obs_new_v2)

###### mk test, trend.slope is full stats, 2 is just p-val and slope stats
trend.slope2 <-function(y, tau.pass = FALSE, p.value.pass = TRUE,  
                        confidence.pass = FALSE, z.value.pass = FALSE,
                        intercept.pass = FALSE) {
  fit <- suppressWarnings( EnvStats::kendallTrendTest(y ~ 1) )
  fit.results <- fit$estimate[2]
  if(p.value.pass == TRUE) { fit.results <- c(fit.results, fit$p.value) } 
  if(z.value.pass == TRUE) { fit.results <- c(fit.results, fit$statistic) } 
  if(confidence.pass == TRUE) { 
    ci <- unlist(fit$interval["limits"])
    if( length(ci) == 2) { 
      fit.results <- c(fit.results, ci)
    } else {
      fit.results <- c(fit.results, c(NA,NA))
    }			  
  }
  if(intercept.pass == TRUE) { fit.results <- c(fit.results, fit$estimate[3]) }  
  if(tau.pass == TRUE) { fit.results <- c(fit.results, fit$estimate[1]) }  
  return( fit.results )
}

# test with app
test <-app(mwa_crop, fun = trend.slope2, cores = 10)
test

plot(test[[1]])
plot(test[[2]])

max_p_value_full <-max_trends_full[[2]]
max_slope_full <-max_trends_full[[1]]
writeRaster(max_p_value_full,"./rasters/mk_results/max_p_value_full.tif")
writeRaster(max_slope_full, "./rasters/max_swe/mk_results/max_slope_full.tif")
