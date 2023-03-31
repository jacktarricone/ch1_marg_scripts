# mwa mk trend analysis
# march 31, 2023


library(terra)
library(spatialEco)



# read in scf rasters to stack
setwd("~/ch1_margulis/")
rast_list <-list.files("./rasters/mwa/", full.names = TRUE)
mwa_stack <-rast(rast_list)

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

# test with lapp
test <-app(mwa_stack[[1:5]], fun = trend.slope2, cores = 10)
test



# run it in parallel to see if stripping is gone 
beginCluster(n=7)

system.time(max_trends_full <- clusterR(max_stack, overlay, args=list(fun=trend.slope2)))

endCluster()


plot(max_trends_full[[1]])
plot(max_trends_full[[2]])

max_p_value_full <-max_trends_full[[2]]
max_slope_full <-max_trends_full[[1]]
writeRaster(max_p_value_full,"/Volumes/jt/projects/margulis/snow_metric_rasters/max_swe/mk_results/max_p_value_full.tif")
writeRaster(max_slope_full, "/Volumes/jt/projects/margulis/snow_metric_rasters/max_swe/mk_results/max_slope_full.tif")
