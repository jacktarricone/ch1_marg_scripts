# mwa mk trend analysis
# march 31, 2023

library(terra)
library(spatialEco)

# read in mwa rasters to stack
setwd("~/ch1_margulis/")
rast_list <-list.files("./rasters/mwa/", full.names = TRUE)
mwa_stack <-rast(rast_list)

# # read in tahoe crop shape
# tahoe_crop <-vect("./vectors/tahoe_crop_mk.geojson")
# tahoe_crop
# mwa_crop <-crop(mwa_stack, tahoe_crop)
# 
# calculate number of non na obs per pixel
n_obs <-app(mwa_stack, function(x) sum(!is.na(x)))
n_obs_v2 <-subst(n_obs, 0, NA)
plot(n_obs_v2)
# writeRaster(n_obs_v2, "./rasters/mk_results/mwa_n_obs.tif")

# convert all values below 10 to NA
masking_value <-subst(n_obs_v2, c(0:25), NA)
plot(masking_value)

# if there are less than 10 observations per pixel, make NA
mwa_stack_v2 <-mask(mwa_stack, masking_value, maskvalues = NA)
mwa_stack_v2

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
mk_results <-app(mwa_stack_v2, fun = trend.slope2, cores = 12)
mk_results

# pull out results
mwa_p_val <-mk_results[[2]]
mwa_slope <-mk_results[[1]]

# mask for sig p_val
mwa_sig_p_val <-mwa_p_val
values(mwa_sig_p_val)[values(mwa_sig_p_val) > .05] <-NA

# sig slope
mwa_sig_slope <-mask(mwa_slope, mwa_sig_p_val)

# quick test plots
plot(mwa_slope)
plot(mwa_p_val)
plot(mwa_sig_p_val)

writeRaster(mwa_sig_p_val,"./rasters/mk_results/mwa_sig_p_val.tif")
writeRaster(mwa_slope, "./rasters/mk_results/mwa_slope.tif")
writeRaster(mwa_sig_slope, "./rasters/mk_results/mwa_sig_slope.tif")

