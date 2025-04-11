# comparing SNSR and SNOTEL snow metrics
# jack tarricone

library(dplyr)
library(ggplot2)
library(lubridate)
library(hydroGOF)
library(cowplot)
library(viridis)
library(ggpointdensity)
library(data.table)
library(tidyr)
library(snotelr)
library(dataRetrieval)

# set wd
setwd('~/ch1_margulis')

# load in functions
# this file has all the snow metric functions and the raster creation one
url <-"https://raw.githubusercontent.com/jacktarricone/ch1_marg_scripts/main/snow_metric_functions.R"
devtools::source_url(url)

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


#########################
#########################
## read in snotel data ##
#########################
#########################

# stations in CA with 32 years of record in the SNSR
good_stations <-as.integer(c(356, 428, 462, 463, 473, 508, 518, 539, 
                             540, 541, 575, 697, 724, 771, 778, 784, 809, 834, 846, 848))

snotel_df <-snotel_download(
  good_stations,
  network = "sntl",
  metric = TRUE,
  internal = TRUE
)

snotel_df$date <-lubridate::ymd(snotel_df$date)
snotel_df$waterYear <-calcWaterYear(snotel_df$date)

snotel_2020 <-filter(snotel_df, waterYear == 2020)


#########################
#########################
##     max swe     ######
#########################
#########################

# calc metric 
snotel_sdd_df <-as.data.frame(snotel_2020) %>%
                              group_by(site_name, waterYear) %>%
                              summarise(sdd_dowy  = sdd(snow_water_equivalent, swe_thres = 100))

snotel_sdd_df





# loop through cells
cells <- paste0("cell", 1:9)  
print(cells)

r_df <- vector("numeric", 9) 

for(i in 1:9) {
  # file
  test_cell <-cells[i]
  filt_df <-filter(snsr_max_df, cell_num == test_cell)
  test_df1 <-cbind(filt_df, snotel_max_df$max_snotel_m)
  colnames(test_df1)[5] <-"max_snotel_m"
  test_df <-drop_na(test_df1)
  result <-round(cor(test_df$max_snsr_m, test_df$max_snotel_m, method = "pearson"),4)
  r_df[i] <-paste(result)
  max_r_df <-as.data.frame(cbind(cells,r_df))
  colnames(max_r_df)[1:2] <-c("cell","r")
}

# test df
max_r_df

# filter for max
best_cell <-filter(max_r_df, r == max(r))
best_cell

# snsr

max_25 <-ggplot(data = max_df, mapping = aes(x = max_snotel_25.4, y = max_snsr_25.4)) +
  geom_pointdensity(adjust = .1, size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_color_viridis(option = "H") +
  scale_y_continuous(limits = c(0,3),expand = (c(0,.0))) +
  scale_x_continuous(limits = c(0,3),expand = (c(0,.0))) +
  xlab("SNOTEL MSWE (m)") + ylab("SNSR MSWE (m)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        aspect.ratio = 1,
        legend.position = "none")

plot(max_25)

# # save
# ggsave( "./plots/max_metric_compare_v8.pdf",
#        max_25,
#        width = 4.5,
#        height = 4.5,
#        units = "in")
# 
# system("open ./plots/max_metric_compare_v8.pdf")
# 

#########################
#########################
##     max_dowy swe    ##
#########################
#########################

# calc metric 
max_dowy_df <-as.data.frame(swe_df %>%
                         group_by(site_name, waterYear) %>%
                         summarise(max_dowy_snotel_0 = max_swe_dowy(snotel_swe_mm, swe_thres = 0),
                                   max_dowy_snsr_0   = max_swe_dowy(snsr_swe_mm, swe_thres = 0),
                                   max_dowy_snotel_25.4 = max_swe_dowy(snotel_swe_mm, swe_thres = 25.4),
                                   max_dowy_snsr_25.4   = max_swe_dowy(snsr_swe_mm, swe_thres = 25.4),
                                   max_dowy_snotel_50.8 = max_swe_dowy(snotel_swe_mm, swe_thres = 50.8),
                                   max_dowy_snsr_50.8   = max_swe_dowy(snsr_swe_mm, swe_thres = 50.8)))


# plot
max_dowy_25 <-ggplot(data = max_dowy_df, mapping = aes(x = max_dowy_snotel_25.4, y = max_dowy_snsr_25.4)) +
  geom_pointdensity(adjust = 5, size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_color_viridis(option = "H") +
  scale_y_continuous(limits = c(50,300),expand = (c(0,3))) +
  scale_x_continuous(limits = c(50,300),expand = (c(0,3))) +
  xlab("SNOTEL DOM (DOWY)") + ylab("SNSR DOM (DOWY)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        aspect.ratio = 1,
        legend.position = "none")

plot(max_dowy_25)

# # save
# ggsave( "./plots/max_dowy_metric_compare_v6.pdf",
#        max_dowy_25,
#        width = 4.5,
#        height = 4.5,
#        units = "in")
# 
# system("open ./plots/max_dowy_metric_compare_v6.pdf")


##########################
##########################
#######    wa   ##########
##########################
##########################
test_wy <-1990

bl_sno <-filter(snotel_df, site_name == "css lab " & waterYear == test_wy)
bl_sno$dowy <-seq(1,nrow(bl_sno),1)
bl_sno

bl_snsr <-filter(snsr_df, site_name_v2 == "CSS_LAB" & wy == test_wy)


ggplot()+
  geom_line(data = bl_snsr, aes(x = dowy, y = swe_mm, color = cell_num)) +
  geom_line(data = bl_sno, aes(x = dowy, y = snotel_swe_mm), color = "black")

# calc metric 
snotel_wa_df <-as.data.frame(snotel_df) %>%
  group_by(site_name, waterYear) %>%
  summarise(wa_snotel_mm  = wa(snotel_swe_mm, swe_thres = 25.4))

snotel_wa_df

snsr_wa_df <-as.data.frame(snsr_df) %>%
  group_by(cell_num, site_name_v2, wy) %>%
  summarise(wa_snsr_mm  = wa(swe_mm, swe_thres = 25.4))

snsr_wa_mean_df <-as.data.frame(snsr_wa_df) %>%
  group_by(site_name_v2, wy) %>%
  summarise(mean_wa_snsr_mm = mean(wa_snsr_mm, na.rm = TRUE), .groups = "drop")

colnames(snotel_wa_df)[2] <-"wy"

testing <-cbind(snsr_wa_mean_df,snotel_wa_df)
testing <-drop_na(testing)

result <-round(cor(testing$mean_wa_snsr_mm, 
                   testing$wa_snotel_mm, 
                   method = "pearson"),4)

snsr_wa_df
hist(snsr_wa_df, breaks = 100)
hist(snotel_wa_df, breaks = 100, add = T, col = )

example <-cbind(snotel_wa_df, snsr_wa_mean_df$mean_wa_snsr_mm)
colnames(example)[4] <-"wa_snsr_mean_mm"
example

r_df <- vector("numeric", 9) 

for(i in 1:9) {
  # file
  test_cell <-cells[i]
  filt_df <-filter(snsr_wa_df, cell_num == test_cell)
  test_df1 <-cbind(filt_df, snotel_wa_df$wa_snotel_mm)
  colnames(test_df1)[5] <-"wa_snotel_mm"
  test_df <-drop_na(test_df1)
  result <-round(cor(test_df$wa_snsr_mm, test_df$wa_snotel_mm, method = "pearson"),4)
  r_df[i] <-paste(result)
  wa_r_df <-as.data.frame(cbind(cells,r_df))
  colnames(wa_r_df)[1:2] <-c("cell","r")
}

# test df
wa_r_df

# filter for max
best_cell <-filter(wa_r_df, r == max(r))
best_cell

best_wa <-filter(snsr_df, cell_num ==best_cell$cell[1])

snsr_wa_df_best <-as.data.frame(best_wa) %>%
  group_by(cell_num, site_name_v2, wy) %>%
  summarise(wa_snsr_mm  = wa(swe_mm, swe_thres = 25.4))

wa_plotting_df <-cross_join(snotel_wa_df,snsr_wa_df_best)
head(wa_plotting_df)

# plot
wa_25 <-ggplot(wa_plotting_df, aes(x = wa_snotel_mm, y = wa_snsr_mm)) +
  geom_pointdensity(adjust = 50, size = 1) +
  scale_color_viridis(option = "H") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_y_continuous(limits = c(0,600),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,600),expand = (c(0,.02))) +
  xlab("SNOTEL WA (mm)") + ylab("SNSR WA (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        aspect.ratio = 1,
        legend.position = "none")

plot(wa_25)


##########################
##########################
#######    mwa   ##########
##########################
##########################

mwa_ondjfm_mm_v1 <-function(x, swe_thres){
  
  # set threshold
  if (max(x) < swe_thres){
    return(NA)
  } else {x}
  if (length(x) == 365){ # non leap year
    
    # trim vector to dec 1 - march 31
    djfm <-x[1:182]
    
    # find difference between values
    val_diff <-diff(djfm)
    val_diff
    
    # sum all negative values
    mwa_mm <-abs(sum(val_diff[val_diff<0]))
    mwa_mm
    return(mwa_mm)
  }
  else{ # leap year
    # trim vector to dec 1 - march 31
    djfm <-x[1:183]
    
    # find difference between values
    val_diff <-diff(djfm)
    val_diff
    
    # sum all negative values
    mwa_mm <-abs(sum(val_diff[val_diff<0]))
    mwa_mm
    return(mwa_mm)
  }
}

# calc metric 
mwa_df <-as.data.frame(swe_df %>%
                        group_by(site_name, waterYear) %>%
                        summarise(mwa_snotel_25.4 = mwa_ondjfm_mm_v1(snotel_swe_mm, swe_thres = 25.4),
                                  mwa_snsr_25.4   = mwa_ondjfm_mm_v1(snsr_swe_mm, swe_thres = 25.4)))
head(mwa_df)
max(mwa_df$mwa_snotel_25.4, na.rm = TRUE)

# plot
mwa_25 <-ggplot(mwa_df, aes(x = mwa_snotel_25.4, y = mwa_snsr_25.4)) +
  geom_pointdensity(adjust = 50, size = 1) +
  scale_color_viridis(option = "H") +
  geom_abline(intercept = 0, slope = 1, linetype = 2, linewidth = .25) +
  scale_y_continuous(limits = c(0,600),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,600),expand = (c(0,.02))) +
  xlab("SNOTEL MWA (mm)") + ylab("SNSR MWA (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        aspect.ratio = 1,
        legend.position = "none")

plot(mwa_25)

# # save
# ggsave( "./plots/mwa_metric_compare_v2.pdf",
#         mwa_25,
#         width = 4.5,
#         height = 4.5,
#         units = "in")
# 
# system("open ./plots/mwa_metric_compare_v2.pdf")



# stack with cow plot
plot_grid(max_25,max_dowy_25, fm_25, mwa_25,
          labels = c("(a)","(b)","(c)","(d)"),
          align = "hv",
          vjust =  2,
          hjust = -.2,
          ncol = 2, 
          rel_widths = c(1/2, 1/2))

ggsave("./plots/snsr_snotel_metric_compare_new_v1.pdf",
       width = 8, 
       height = 8,
       units = "in",
       dpi = 500)

system("open ./plots/snsr_snotel_metric_compare_new_v1.pdf")

#########################################
### calculate goodness of it metrics ####
#########################################

# define function that creates vector of metrics
gof_func <-function(snsr, snotel, metric_name){
  
  # calculate the 5 metrics
  corr <-round(hydroGOF::rPearson(snsr, snotel, na.rm = TRUE), digits = 2)
  r2 <-round(corr^2, digits = 2)
  rmse <-round(hydroGOF::rmse(snsr, snotel, na.rm = TRUE), digits = 2)
  nrmse <-round(hydroGOF::nrmse(snsr, snotel, na.rm = TRUE, norm = "maxmin"), digits = 0)
  mae <-round(hydroGOF::mae(snsr, snotel, na.rm = TRUE), digits = 2)
  nmae <-round(mae/(max(snsr, na.rm = TRUE)-min(snsr, na.rm = TRUE)), digits = 2)
  me <-round(hydroGOF::me(snsr, snotel,na.rm = TRUE), digits = 2)
  pb <-round(hydroGOF::pbias(snsr, snotel, na.rm = TRUE), digits = 2)
  
  # bind
  row <-c(metric_name,corr,r2,rmse,mae,me,pb)
  return(row)
}

# create rows
metric_names <-c("Snow Metric", "R", "R^2", "RMSE","MAE","ME", "PB (%)")
max_stats <-gof_func(max_df$max_snsr_25.4, max_df$max_snotel_25.4, "Max SWE (m)")
max_dowy_stats <-gof_func(max_dowy_df$max_dowy_snsr_25.4, max_dowy_df$max_dowy_snotel_25.4, "Max SWE (DOWY)")
fm_stats <-gof_func(fm_df$fm_snsr_25.4, fm_df$fm_snotel_25.4, "FM")
mwa_stats <-gof_func(mwa_df$mwa_snsr_25.4, mwa_df$mwa_snotel_25.4, "MWA (mm)")
fm_stats

# make df 
table <-as.data.frame(rbind(max_stats, max_dowy_stats, fm_stats, mwa_stats))

# rename cols
colnames(table)[1:7] <-metric_names
# table <-table[-1,]
table

# save
write.csv(table, "./csvs/snow_metric_error_metric_v6.csv", row.names = FALSE)
