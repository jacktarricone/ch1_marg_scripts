# scf regional mann-kendall test script start
# jack tarricone
# january 18 2023

library(terra)
library(tidyverse)
library(rkt)

# set working dir
setwd("/Users/jacktarricone/ch1_margulis/")

# read in stack american stack
scf_stack <-rast("./snow_metric_rasters/terra_rasters/scf/scf_american.tif")
scf_stack

# bring in american shape file
american_v1 <-vect("./american_test/american.shp")
american <-project(american_v1, crs(scf_stack))

# test plot
plot(scf_stack[[6]])
plot(american, add = TRUE)

# bring in classes 4
classes <-rast("./static/american_dem_aspect_classified.tif")
plot(classes, add = TRUE)

# convert to df
scf_df <-as.data.frame(scf_stack, xy = TRUE, cells = TRUE, na.rm = TRUE)
classes_df <-as.data.frame(classes, xy = TRUE, cells = TRUE, na.rm = TRUE)

# filter down to same cell numbers for each df
scf_filt <-subset(scf_df, cell %in% classes_df$cell)
classes_filt <-subset(classes_df, cell %in% scf_df$cell)

# check if the same
identical(scf_filt$cell, classes_filt$cell) # yes

# years seq
years <-seq(1985,2016,1)

# rename columns, which are the annual rasters, with the correct year name
colnames(scf_filt)[4:ncol(scf_filt)] <-years

# join classes and scf
scf_joined <-right_join(classes_filt,scf_filt,by = c("cell","x","y"))
scf_joined$SNSR_DEM <-classes_filt$SNSR_DEM # fix classes col
head(scf_joined)

# pivot longer for test
# creates "year" col and "scf_percent" col while preserving meta data info
scf_mk_test_df <-as.data.frame(scf_joined %>%
                             pivot_longer(-c(cell,x,y,SNSR_DEM), names_to = "year", values_to = "scf_percent"))

# convert to int for test
scf_mk_test_df$year <-as.integer(scf_mk_test_df$year)
scf_mk_df <-scf_mk_test_df[order(scf_mk_test_df$year),] # sort by year
scf_mk_df$scf_percent_100 <-scf_mk_df$scf_percent*100
head(scf_mk_df) # looks good!

# subet to each class
b1_n <-filter(scf_mk_df, SNSR_DEM == 1)
b1_s <-filter(scf_mk_df, SNSR_DEM == 2)
b2_n <-filter(scf_mk_df, SNSR_DEM == 3)
b2_s <-filter(scf_mk_df, SNSR_DEM == 4)
b3_n <-filter(scf_mk_df, SNSR_DEM == 5)
b3_s <-filter(scf_mk_df, SNSR_DEM == 6)

# using rkt package run regional kendall by classes category

# rkt_df_output <-function(df)
# b3_s
b3_s_rkt_results <-rkt(b3_s$year,         # time vector of years
                       b3_s$scf_percent_100,  # scf_percent data 
                       b3_s$SNSR_DEM,     # 'block' aka class variable
                       correct = TRUE,
                       rep = "m") 

# this runs!!
print(b3_s_rkt_results)
b3_s_rkt_results

# pull out results
slope <-b3_s_rkt_results$B     # B = Theil-Sen's slope for Regional Kendall Slope estimator for RKT
p_val <-b3_s_rkt_results$sl    # sl = two sided p-value
tau <-b3_s_rkt_results$tau     # tau = Kendall's tau
score <-b3_s_rkt_results$S     # S = Kendall's score
varS <-b3_s_rkt_results$varS   # varS = variance of S, after correction for intra-block correlation

as.data.frame(col.names = c(slope,p_val))
results <-as.data.frame(cbind(slope,p_val,tau,score,varS))
write.csv(results, "./rkt_results/scf_b3_s_results.csv", row.names = FALSE)



#######################################
#######################################
#######################################

df <-b3_s
head(df)

scf_rkt_df_output <-function(df){

    # run rkt
    results <-rkt(df$year,      # time vector of years
                  df$scf_percent_100,  # scf_percent_100 data 
                  df$SNSR,     # 'block' aka class variable
                  correct = TRUE,
                  rep = "m") 

    # pull out results from list
    slope <-results$B     # B = Theil-Sen's slope for Regional Kendall Slope estimator for RKT
    p_val <-results$sl    # sl = two sided p-value
    tau <-results$tau     # tau = Kendall's tau
    score <-results$S     # S = Kendall's score
    varS <-results$varS   # varS = variance of S, after correction for intra-block correlation

    # convet to df
    results_df <-as.data.frame(cbind(slope,p_val,tau,score,varS))
    return(results_df)
    write.csv(results_df, "./rkt_results/scf_b3_s_results_v2.csv", row.names = FALSE)

}

system.time(scf_rkt_df_output(df = b3_s))





# b3_n
b3_n_rkt_results <-rkt(b3_n$year,         # time vector of years
                       b3_n$scf_percent,  # scf_percent data 
                       b3_n$SNSR_DEM,     # 'block' aka class variable
                       correct = TRUE,
                       rep = "m") 

# this runs!!
print(b3_n_rkt_results)

??rkt::rkt


# crop to just north facing slopes
south_crop <-filter(mk_df, SNSR_classes == 1)
tail(south_crop)

# using rkt package run regional kendall by classes category
south_rkt_results <-rkt(south_crop$year,         # time vector of years
                         south_crop$scf_percent,  # scf_percent data 
                         south_crop$SNSR_classes,
                         correct = TRUE,
                         rep = "m") # block aka classes (numbers 1:4)

print(south_rkt_results)
















#######################
###### brain storm code
#################################

# cell_numbers <-mk_df$cell[1:800]
# mk_crop <-filter(mk_df, cell %in% cell_numbers) # filter for certain cells


### test plot
# one cell
north <-filter(mk_df, SNSR_classes == 1)

# quick test plot
library(scattermore)
ggplot(north, aes(x = year, y = scf_percent)) +
  geom_scattermore()

data(pie1)
pie1
ex<-rkt(pie1$Year,pie1$SO4,pie1$Month, correct = TRUE)
print(ex)
