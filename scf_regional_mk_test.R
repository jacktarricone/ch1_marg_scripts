# scf regional mann-kendall test script start
# jack tarricone
# january 18 2023

library(terra)
library(tidyverse)
library(rkt)

# format stack
setwd("/Users/jacktarricone/ch1_margulis/")

# read in stack american stack
scf_stack <-rast("./snow_metric_rasters/terra_rasters/scf/scf_american.tif")
scf_stack

# test mk code by first running it on .5 degrees lat near tahoe
american_v1 <-vect("./american_test/american.shp")
american <-project(american_v1, crs(scf_stack))

# test plot
plot(scf_stack[[6]])

# bring in ascpet
aspect_v1 <-rast("/Users/jacktarricone/ch1_margulis/static/rasters/SNSR_aspect.tif")
aspect <-project(aspect_v1, crs(scf_stack))
plot(aspect)
hist(aspect, breaks = 100)

# bin into 4 categories north, south, east, west
aspect_classes <-matrix(c(315,360,1, 00,45,1, # 1 = north
                          135,225,2,          # 2 = south
                          45,135,3,           # 3 = east 
                          225,315,4),         # 4 = west
                          ncol=3, byrow=TRUE)

aspect_classes

aspect_cat <-classify(aspect, rcl = aspect_classes)
plot(aspect_cat)
hist(aspect_cat)
# writeRaster(aspect_cat, "./static/aspect_cat.tif")

# mask for american for plotting
aspect_cat_american_v1 <-crop(aspect_cat, american)
aspect_cat_american <-mask(aspect_cat_american_v1, american)
plot(aspect_cat_american)
hist(aspect_cat_american)
# writeRaster(aspect_cat_american)

# mask scf with aspect
scf_stack_mask <-mask(scf_stack, aspect_cat_american)

##### calculate land area of cells that aren't NA
# aspect_cat
area_by_direction <-expanse(aspect_cat_american, unit="km", byValue = TRUE)
aspect_total_area <-expanse(aspect_cat_american, unit="km", byValue = FALSE)

# scf
scf_total_area <-expanse(scf_stack_mask[[9]], unit="km", byValue = FALSE)

# convert to df
scf_df <-as.data.frame(scf_stack, xy = TRUE, cells = TRUE, na.rm = TRUE)
ac_df <-as.data.frame(aspect_cat_american, xy = TRUE, cells = TRUE, na.rm = TRUE)
ac_df

# filter down to same cell numbers
scf_filt <-subset(scf_df, cell %in% ac_df$cell)
ac_filt <-subset(ac_df, cell %in% scf_df$cell)

# years seq
years <-seq(1985,2016,1)

colnames(scf_filt)[4:ncol(scf_filt)] <-years
head(scf_filt)

test <-pivot_longer(scf_filt[4:35,], names_to = "year", values_to = "scf")

# check if the same
identical(scf_filt$cell, ac_filt$cell)


data("pie1w")
pie1w
rkt(pie1w$Date,pie1w$SO4)

ggplot(pie1w) +
  geom_line(aes(x = "Date", y = "S04"))

plot(pie1w$Date, pie1w$NO3)

hist(pie1w$Date)


##### calculate land area of cells that aren't NA
# aspect_cat
area_by_direction <-expanse(aspect_cat_american, unit="km", byValue = TRUE)
total_area <-expanse(aspect_cat_american, unit="km", byValue = FALSE)

# scf
scf_total_area <-expanse(scf_stack[[6]], unit="km", byValue = FALSE)





















###### mk test, trend.slope is full stats, 2 is just p-val and slope stats
trend.slope <- function(y, p.value.pass = TRUE, z.pass = TRUE, 
                        tau.pass = TRUE, confidence.pass = TRUE, intercept.pass = TRUE) {
  options(warn = -1)
  fit <- EnvStats::kendallTrendTest(y ~ 1)
  fit.results <- fit$estimate[2]
  if (tau.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[1])
  }
  if (intercept.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[3])
  }
  if (p.value.pass == TRUE) {
    fit.results <- c(fit.results, fit$p.value)
  }
  if (z.pass == TRUE) {
    fit.results <- c(fit.results, fit$statistic)
  }
  if (confidence.pass == TRUE) {
    ci <- unlist(fit$interval["limits"])
    if (length(ci) == 2) {
      fit.results <- c(fit.results, ci)
    }
    else {
      fit.results <- c(fit.results, c(NA, NA))
    }
  }
  options(warn = 0)
  return(fit.results)
}

trend.slope2 <- function(y, p.value.pass = TRUE, z.pass = FALSE, 
                         tau.pass = FALSE, confidence.pass = FALSE, intercept.pass = FALSE) {
  options(warn = -1)
  fit <- EnvStats::kendallTrendTest(y ~ 1)
  fit.results <- fit$estimate[2]
  if (tau.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[1])
  }
  if (intercept.pass == TRUE) {
    fit.results <- c(fit.results, fit$estimate[3])
  }
  if (p.value.pass == TRUE) {
    fit.results <- c(fit.results, fit$p.value)
  }
  if (z.pass == TRUE) {
    fit.results <- c(fit.results, fit$statistic)
  }
  if (confidence.pass == TRUE) {
    ci <- unlist(fit$interval["limits"])
    if (length(ci) == 2) {
      fit.results <- c(fit.results, ci)
    }
    else {
      fit.results <- c(fit.results, c(NA, NA))
    }
  }
  options(warn = 0)
  return(fit.results)
}


# run it in parallel to see if stripping is gone 
beginCluster(n=7)

system.time(scf_trends_full <- clusterR(scf_stack, overlay, args=list(fun=trend.slope2)))

endCluster()


plot(scf_trends_full[[1]])
plot(scf_trends_full[[2]])

scf_p_value_full <-scf_trends_full[[2]]
scf_slope_full <-scf_trends_full[[1]]
writeRaster(scf_p_value_full,"/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_p_value_full.tif")
writeRaster(scf_slope_full, "/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_slope_full.tif")


###########################################
############# calculations testing
###########################################

scf_p_value <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_p_value.tif")
scf_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/scf_slope.tif")


sig_pval <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/sig_pval.tif")
sig_ele <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/sig_ele.tif")
sig_slope <-raster("/Volumes/jt/projects/margulis/snow_metric_rasters/scf/mk_results/sig_slope.tif")
crs(sig_pval)<-"+proj=leac +ellps=clrk66"
crs(sig_ele)<-"+proj=leac +ellps=clrk66"
crs(sig_slope)<-"+proj=leac +ellps=clrk66"

mk_stack <-stack(sig_pval, sig_ele, sig_slope)

plot(mk_stack)


values(sig_ele)[values(sig_ele) <= 0] = NA
hist(sig_ele)


values(sig_slope)[values(sig_slope) == 0.0000] = NA
hist(sig_slope)


plot(values(sig_slope), values(sig_ele))

tahoe_array <-as.array(tahoe_crop)
test_col <-as.data.frame(tahoe_array[250, 2900, 1:32])
wy <-c(1985:2016)
test_col <-cbind(test_col, wy)
colnames(test_col)[1] <- "scf"
hist(test_col)

ggplot(test_col)+
  geom_point(aes(wy, scf)) + 
  theme_light()


# read in static rasters to stack for calculations
static_stack <-stack("/Volumes/jt/projects/margulis/static/rasters/static_stack.tif")
crs(static_stack)<-"+proj=leac +ellps=clrk66"
plot(static_stack)
static_stack

# test mk code by first running it on .5 degrees lat near tahoe
static_crop <-crop(static_stack, extent(-123.3, -117.6, 39, 39.5))
static_crop
plot(static_crop)

hist(sig_pix)
freq(sig_pix)
















?raster.kendall

top1deg_results2
plot(top1deg_results2[[1]])
########

plot(max_results)
max_results_stack <-stack(max_results)
crs(max_results_stack)<-"+proj=leac +ellps=clrk66"

p_value <-max_results[[2]]
slope <-max_results[[1]]
writeRaster(p_value,"/Volumes/jt/projects/margulis/mk_results/p_value.tif")
writeRaster(slope, "/Volumes/jt/projects/margulis/mk_results/slope.tif")
