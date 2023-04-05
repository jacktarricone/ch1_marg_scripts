# max_dowy mk trend analysis
# april 4, 2023

library(terra)
library(spatialEco)

# read in mwa rasters to stack
setwd("~/ch1_margulis/")
rast_list <-list.files("./rasters/snow_metrics/max_swe_dowy", full.names = TRUE)
stack <-rast(rast_list)
stack

# # read in tahoe crop shape
# tahoe_crop <-vect("./vectors/tahoe_crop_mk.geojson")
# tahoe_crop
# mwa_crop <-crop(mwa_stack, tahoe_crop)

# calculate number of non na obs per pixel
n_obs <-app(stack, function(x) sum(!is.na(x)))
n_obs_v2 <-subst(n_obs, 0, NA)
plot(n_obs_v2)
# writeRaster(n_obs_v2, "./rasters/mk_results/mwa_n_obs.tif")

# convert all values below 10 to NA
masking_value <-subst(n_obs_v2, c(0:25), NA)
plot(masking_value)

# if there are less than 10 observations per pixel, make NA
stack_v2 <-mask(stack, masking_value, maskvalues = NA)
stack_v2

hist(stack_v2[[26]], breaks = 100)

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

# run mk test
mk_results <-app(stack_v2, fun = trend.slope2, cores = 6)
mk_results

# pull out results
p_val <-mk_results[[2]]
slope <-mk_results[[1]]

# mask for sig p_val
sig_p_val <-mwa_p_val
values(mwa_sig_p_val)[values(sig_p_val) > .05] <-NA

# sig slope
sig_slope <-mask(slope, sig_p_val)

# quick test plots
plot(slope)
plot(p_val)
plot(sig_p_val)

writeRaster(sig_p_val,"./rasters/mk_results/max_dowy_sig_p_val.tif")
writeRaster(slope, "./rasters/mk_results/max_dowy_slope.tif")
writeRaster(sig_slope, "./rasters/mk_results/max_dowy_sig_slope.tif")

