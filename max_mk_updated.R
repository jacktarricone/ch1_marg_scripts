# max mk trend analysis
# april 4, 2023

library(terra)

# read in mwa rasters to stack
setwd("~/ch1_margulis/")
rast_list <-list.files("./rasters/snow_metrics/max_swe/years", full.names = TRUE)
stack <-rast(rast_list)
stack

# convert all values below 10 to NA
stack_v2 <-subst(stack, 0:25.4, NA)
plot(stack_v2[[31]])

# calculate number of non na obs per pixel
n_obs <-app(stack_v2, function(x) sum(!is.na(x)))
n_obs_v2 <-subst(n_obs, 0, NA)
plot(n_obs_v2)
writeRaster(n_obs_v2, "./rasters/mk_results/max_n_obs.tif")

# convert all values below 10 to NA
masking_value <-subst(n_obs_v2, 0:10, NA)
plot(masking_value)

# if there are less than 10 observations per pixel, make NA
stack_v3 <-mask(stack_v2, masking_value, maskvalues = NA)
stack_v3

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
mk_results <-app(stack_v3, fun = trend.slope2, cores = 14)
mk_results

# pull out results
p_val <-mk_results[[2]]
slope <-mk_results[[1]]

# mask for sig p_val
sig_p_val <-p_val
values(sig_p_val)[values(sig_p_val) > .05] <-NA

# sig slope
sig_slope <-mask(slope, sig_p_val)

# quick test plots
plot(p_val)
plot(sig_p_val)
plot(slope)
plot(sig_slope)

# save
writeRaster(p_val,"./rasters/mk_results/max_p_val.tif")
writeRaster(sig_p_val,"./rasters/mk_results/max_sig_p_val.tif")
writeRaster(slope, "./rasters/mk_results/max_trend.tif")
writeRaster(sig_slope, "./rasters/mk_results/max_sig_trend.tif")

