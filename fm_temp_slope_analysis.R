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
usj <-vect("./vectors/ca_basins/usj.gpkg")

# single rasters: insol, temp normal, dem
insol_usj <-mask(crop(rast("./rasters/insolation/snsr_dem_insol_watts_masked_v1.tif"),ext(usj)), usj)
names(insol_usj) <-"insol_watts"
plot(insol_usj)

# temp
temp_usj <-mask(crop(rast("./rasters/daymet/tmean_normal_1985_2016.tif"),ext(usj)), usj)
names(temp_usj) <-"mean_temp_c"
plot(temp_usj)

# dem
dem_usj <-mask(crop(rast("./rasters/static/SNSR_DEM.tif"),ext(usj)), usj)
names(dem_usj) <-"elevation"
plot(dem_usj)

### stacks
# fm load and format
fm_list <-list.files("./rasters/snow_metrics/fm_apr1", full.names = TRUE)
fm_stack_usj <-mask(crop(rast(fm_list[1:32]),ext(usj)), usj)
plot(fm_stack_usj[[31]])

# temp load and format
tmean_list <-list.files("./rasters/daymet/tmean", full.names = TRUE)
tmean_stack_usj <-mask(crop(rast(tmean_list[1:32]),ext(usj)), usj)
plot(tmean_stack_usj[[2]])

# rename layers
wy_names <-seq(1985,2016,1)
names(tmean_stack_usj) <-wy_names
names(fm_stack_usj) <-wy_names
tmean_stack_usj

# stack with other layers
tmean_full <-c(insol_usj, temp_usj, dem_usj, tmean_stack_usj)
fm_full <-c(insol_usj, temp_usj, dem_usj, fm_stack_usj)

# convert to df
tmean_usj_df_v1 <-as.data.frame(tmean_full, xy=TRUE, cells=TRUE)
tmean_usj_df <-as.data.frame(pivot_longer(tmean_usj_df_v1, 7:38, names_to = "wy"))
colnames(tmean_usj_df)[8] <-'annual_tmean_c'
head(tmean_usj_df)

# convert to df
fm_usj_df_v1 <-as.data.frame(fm_full, xy=TRUE, cells=TRUE)
fm_usj_df <-as.data.frame(pivot_longer(fm_usj_df_v1, 7:38, names_to = "wy"))
colnames(fm_usj_df)[8] <-'frac_melt'
head(fm_usj_df)

# join
full_df <-full_join(fm_usj_df, tmean_usj_df)
head(full_df)
# fwrite(full_df, "./csvs/usj_temp_fm_slope_analysis.csv", row.names = FALSE)
full_df <-fread("./csvs/usj_temp_fm_slope_analysis.csv")

# pull out one cell
test <-filter(full_df, cell <10000)
head(test)
results_list <-as.list(test %>% group_by(cell) %>% 
  do(extract_lm_stats(y = frac_melt, x = annual_tmean_c, data = .)))

# pull out stats
extract_lm_stats <-function(y,x,data){
  intercept <-coef(lm(y ~ x, data))[[1]]
  slope <-coef(lm(y ~ x, data))[[2]]
  df <-as.data.frame(intercept, slope)
  return(df)
}

# calculate number of non na obs per pixel
tmean_n_obs <-app(tmean_stack_usj, function(x) sum(!is.na(x)))
plot(tmean_n_obs)
fm_n_obs <-app(fm_stack_usj, function(x) sum(!is.na(x)))
plot(fm_n_obs)
n_obs_v2 <-subst(fm_n_obs, 0, NA)
plot(n_obs_v2)
writeRaster(n_obs_v2, "./rasters/mk_results/max_n_obs.tif")

# convert all values below 10 to NA
masking_value <-subst(n_obs_v2, 0:27, NA)
plot(masking_value)

# if there are less than 10 observations per pixel, make NA
fm_v3 <-mask(fm_stack_usj, masking_value, maskvalues = NA)

analysis_stack <-c(fm_v3, tmean_stack_usj)
analysis_stack 

# slope_fun  <-function(x){ if (is.na(x[1])){ NA } else { lm(x[1:32] ~ x[33:64])$coefficients[2] }}
# intercept_fun  <-function(x){ if (is.na(x[1])){ NA } else { lm(x[1:32] ~ x[33:64])$coefficients[1] }}
# r2_fun <-function(x) { if (is.na(x[1])){ NA } else { m <- lm(x[1:32]] ~ x[1:32]]);summary(m)$r.squared }}

###### mk test, trend.slope is full stats, 2 is just p-val and slope stats
trend.slope2 <-function(y, tau.pass = FALSE, p.value.pass = TRUE,  
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
mk_results <-app(analysis_stack, fun = trend.slope2, cores = 14)
mk_results




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
