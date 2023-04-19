# max regional mann-kendall test script start
# jack tarricone
# january 18 2023

library(terra)
library(dplyr)
library(rkt)

# set working dir
setwd("~/ch1_margulis/")

# read in stack american stack
max_stack <-rast("./rasters/snow_metrics/max_swe/max_stack.tif")
max_stack

# bring in american shape file
kern <-vect("./vectors/ca_basins/kern.gpkg")
kern

# mask kern
max_kern_v1 <-mask(max_stack, kern)
max_kern <-crop(max_kern_v1, kern)
plot(max_kern[[1]])

# bring in classes 4
classes <-rast("./rasters/categorized/aspect_4deg_ns.tif")

# classes kern
kern_classes_v1 <-crop(classes, kern)
kern_classes <-mask(kern_classes_v1, kern)
plot(kern_classes)

# kern south
kern_south <-subst(kern_classes, 1, NA)
plot(kern_south)

# mask for north facing slopes
max_kern_north_v1 <-mask(max_kern, kern_classes, maskvalue = 3) # mask 3 aka south out
max_kern_north <-mask(max_kern_north_v1, kern_classes, maskvalue = NA)
plot(max_kern_north[[1]])

### test this solution of applying rkt function to raster statck
### https://gis.stackexchange.com/questions/429168/rkt-package-function-on-time-series-analysis-is-returning-error?rq=1

# define funciton

rkt_rast <-function(x,block){
  
  if(all(is.na(x))){
    
    c(NA,NA,NA)
    
  } else {
    
    years <-1:32
    analysis <-rkt::rkt(years, x, max_kern_north)
    a <-analysis$B # theil sen slope
    b <-analysis$sl # pvalue
    c <-analysis$tau # tau
    return(cbind(a, b, c))
  } 
}

# run mk test
mk_results <-app(max_kern, fun = rkt_rast)
mk_results
plot(mk_results)









# convert to df
max_df <-as.data.frame(max_stack, xy = TRUE, cells = TRUE, na.rm = TRUE)
classes_df <-as.data.frame(classes, xy = TRUE, cells = TRUE, na.rm = TRUE)

# filter down to same cell numbers for each df
max_filt <-subset(max_df, cell %in% classes_df$cell)
classes_filt <-subset(classes_df, cell %in% max_df$cell)

# check if the same
identical(max_filt$cell, classes_filt$cell) # yes

# years seq
years <-seq(1985,2016,1)

# rename columns, which are the annual rasters, with the correct year name
colnames(max_filt)[4:ncol(max_filt)] <-years

# join classes and max
max_joined <-right_join(classes_filt,max_filt,by = c("cell","x","y"))
max_joined$SNSR_DEM <-classes_filt$SNSR_DEM # fix classes col
head(max_joined)

# pivot longer for test
# creates "year" col and "max_percent" col while preserving meta data info
max_mk_test_df <-as.data.frame(max_joined %>%
                             pivot_longer(-c(cell,x,y,SNSR_DEM), names_to = "year", values_to = "max_swe_mm"))

# convert to int for test
max_mk_test_df$year <-as.integer(max_mk_test_df$year)
max_mk_df <-max_mk_test_df[order(max_mk_test_df$year),] # sort by year
max_mk_df$max_swe_m <-max_mk_df$max_swe_mm*(1/1000)
max_mk_df$max_swe_cm <-max_mk_df$max_swe_mm*(1/10)
head(max_mk_df) # looks good!



# subet to each class
b1_n <-filter(max_mk_df, SNSR_DEM == 1)
b1_s <-filter(max_mk_df, SNSR_DEM == 2)
b2_n <-filter(max_mk_df, SNSR_DEM == 3)
b2_s <-filter(max_mk_df, SNSR_DEM == 4)
b3_n <-filter(max_mk_df, SNSR_DEM == 5)
b3_s <-filter(max_mk_df, SNSR_DEM == 6)

# using rkt package run regional kendall by classes category
# b3_s
b3_s_rkt_results <-rkt(b3_s$year,       # time vector of years
                       b3_s$max_swe_cm, # max_percent data 
                       b3_s$SNSR_DEM,   # 'block' aka class variable
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

results <-as.data.frame(cbind(slope,p_val,tau,score,varS))
results
write.csv(results, "./rkt_results/max_b3_s_results.csv", row.names = FALSE)



#######################################
#######################################
#######################################

# define function that outputs df of results
max_rkt_df_output <-function(df){

    # run rkt
    results <-rkt(df$year,      # time vector of years
                  df$max_swe_cm,  # max_percent_100 data 
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
    #write.csv(results_df, "./rkt_results/max_b3_s_results_v2.csv", row.names = FALSE)

}

# test function
# test_df <-max_rkt_df_output(df = b3_s)
test_df_v2 <-max_rkt_df_output(df = b3_n)
# write.csv(test_df_v2, "./rkt_results/max_b3_n_results.csv", row.names = FALSE)


system.time(test_df_v3 <-max_rkt_df_output(df = b2_s))
