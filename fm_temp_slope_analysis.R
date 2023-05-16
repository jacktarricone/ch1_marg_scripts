# annual temp vs fm and short wave
# jack tarricone
# may 15th, 2023

library(terra)
library(tidyr)
library(ggplot2)
library(scales)
library(cowplot)
library(viridis)
library(data.table)

setwd("~/ch1_margulis")

# set custom plot theme
theme_classic <-function(base_size = 11, base_family = "",
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22) {
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # no background and no grid
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # show axes
      # axis.line      = element_line(colour = "black", linewidth = rel(1)),
      
      # match legend key to panel.background
      legend.key       = element_blank(),
      
      # simple, black and white strips
      strip.background = element_rect(fill = "white", colour = "black", linewidth = rel(2)),
      # NB: size is 1 but clipped, it looks like the 0.5 of the axes
      
      complete = TRUE
    )
}

theme_set(theme_classic(14))

# bring vectors
american <-vect("./vectors/ca_basins/american.gpkg")

# single rasters: insol, temp normal, dem
insol_american <-mask(crop(rast("./rasters/insolation/snsr_dem_insol_watts_masked_v1.tif"),ext(american)), american)
names(insol_american) <-"insol_watts"
plot(insol_american)

# temp
temp_american <-mask(crop(rast("./rasters/daymet/tmean_normal_1985_2016.tif"),ext(american)), american)
names(temp_american) <-"mean_temp_c"
plot(temp_american)

# dem
dem_american <-mask(crop(rast("./rasters/static/SNSR_DEM.tif"),ext(american)), american)
names(dem_american) <-"elevation"
plot(dem_american)

### stacks
# fm load and format
fm_list <-list.files("./rasters/snow_metrics/fm_apr1", full.names = TRUE)
fm_stack_american <-mask(crop(rast(fm_list[1:32]),ext(american)), american)
plot(fm_stack_american[[31]])

# temp load and format
tmean_list <-list.files("./rasters/daymet/tmean", full.names = TRUE)
tmean_stack_american <-mask(crop(rast(tmean_list[1:32]),ext(american)), american)
plot(tmean_stack_american[[2]])

# rename layers
wy_names <-seq(1985,2016,1)
names(tmean_stack_american) <-wy_names
names(fm_stack_american) <-wy_names
tmean_stack_american

# calculate number of non na obs per pixel
fm_n_obs <-app(fm_stack_american, function(x) sum(!is.na(x)))
n_obs_v2 <-subst(fm_n_obs, 0, NA)
plot(n_obs_v2)

# convert all values below 10 to NA
masking_value <-subst(n_obs_v2, 0:27, NA)
plot(masking_value)

# if there are less than 10 observations per pixel, make NA
fm_v3 <-mask(fm_stack_american, masking_value, maskvalues = NA)

analysis_stack <-c(fm_v3, tmean_stack_american)
analysis_stack 

# slope_fun  <-function(x){ if (is.na(x[1])){ NA } else { lm(x[1:32] ~ x[33:64])$coefficients[2] }}
# intercept_fun  <-function(x){ if (is.na(x[1])){ NA } else { lm(x[1:32] ~ x[33:64])$coefficients[1] }}
# r2_fun <-function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:32]] ~ x[1:32]]);summary(m)$r.squared }}

###### mk test, trend.slope is full stats, 2 is just p-val and slope stats
fm_temp_mk <-function(y, tau.pass = FALSE, p.value.pass = TRUE,  
                        confidence.pass = FALSE, z.value.pass = FALSE,
                        intercept.pass = FALSE) {
  fit <- suppressWarnings( EnvStats::kendallTrendTest(y[1:32] ~ y[33:64]) )
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
mk_results <-app(analysis_stack, fun = fm_temp_mk, cores = 14)
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
hist(slope)


# test plot
ggplot(full_df)+
  geom_bin2d(bins = 100, aes(y = frac_melt, x = annual_tmean_c, fill = ..density..)) +
  #geom_tile(fill = 'darkred', width = 14/100, height = 1/100) +
  scale_color_gradientn(colors = scale, name = expression(Insolation ~ '(W m'^{"-2"} ~ ')')) +
  scale_x_continuous(limits = c(-6,8), expand = (c(0,0))) +
  scale_y_continuous(limits = c(0,1),expand = (c(0,0))) +
  # geom_point(color = 'darkred')+
  # geom_smooth(method='lm', se = FALSE)+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))
