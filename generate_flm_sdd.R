library(terra)

setwd("~/ch1_margulis/")

# load in stack
flm_list <-list.files("~/ch3_fusion/rasters/flm/raw", full.names = T)
tail(flm_list)

# stack em
flm_stack1 <-rast(flm_list)
flm_stack <-ifel(flm_stack1 == 0, NA, flm_stack1)
plot(flm_stack[[180]])
flm_stack

# function for sdd
flm_sdd <- function(rast, threshold) {
  
  # check if rast
  if (!inherits(rast, "SpatRaster")) {
    stop("Input 'rast' must be a SpatRaster object.")
  }
  
  # nlyrs
  n_layers <- nlyr(rast)
  
  # function
  sdd <- function(values) {
    
    # find dowy
    below_idx <- which(values < threshold)
    
    if (length(below_idx) == 0) {
      return(nlyr(rast))
    } else {
      return(below_idx[1])
    }
  }
  
  # apply fun
  result <- app(rast, sdd)
  
  return(result)
}

wy20_sdd <-
