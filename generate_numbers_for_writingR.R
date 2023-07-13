library(scales)
library(dplyr)

snow <-read.csv("~/ch1_margulis/csvs/snow_met_physio_results/snow_avg_metric_results_table_v4.csv")
met <-read.csv("~/ch1_margulis/csvs/snow_met_physio_results/met_avg_metric_results_table_v2.csv")

# calc kern insol
kern <-filter(met, basin_name == "kern")
n_insol_mean <-mean(kern$mean_ez_srad_North)
s_insol_mean <-mean(kern$mean_ez_srad_South)

# calc usj insol
usj <-filter(met, basin_name == "usj")
n_insol_mean <-mean(usj$mean_ez_srad_North)
s_insol_mean <-mean(usj$mean_ez_srad_South)


t2 <-filter(t1, zone_name == "2700-3100 m" | zone_name == "3100-4361 m")

head(snow)
mean(snow$diff_ez_dom)
mean(met$mean_cs_insol_South)
mean(met$mean_cs_insol_North)

# mean MSWE south north differnces
mswe_n_mean <-mean(snow$mean_ez_mswe_North)
mswe_s_mean <-mean(snow$mean_ez_mswe_South)
diff <-mswe_n_mean - mswe_s_mean

1660 - 1193

t1 <-filter(snow, basin_name != "yuba")
t2 <-filter(t1, zone_name == "2700-3100 m" | zone_name == "3100-4361 m")

fm_north_mean <-mean(t2$mean_ez_fm_North)
fm_south_mean <-mean(t2$mean_ez_fm_South)

mwa_north_mean <-mean(t2$mean_ez_mwa_North)
mwa_south_mean <-mean(t2$mean_ez_mwa_South)

mwa_south_mean/mwa_north_mean

head(t1)
  
ez1 <-percent((.87 - .78) / old_value)
percentage_difference <- function(value, value_two) {
  (value - value_two) / ((value + value_two) / 2) * 100
} 


ez1 <-percentage_difference(.87,.78)
ez2 <-percentage_difference(.79,.58)
ez3 <-percentage_difference(.62,.34)
ez4 <-percentage_difference(.47,.15)
ez5 <-percentage_difference(.25,.04)
mean <-mean(c(ez1,ez2,ez3,ez4,ez5))
