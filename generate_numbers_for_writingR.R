library(scales)


snow <-read.csv("~/ch1_margulis/csvs/snow_met_physio_results/snow_avg_metric_results_table_v4.csv")
met <-read.csv("~/ch1_margulis/csvs/snow_met_physio_results/met_avg_metric_results_table_v2.csv")

head(snow)
mean(snow$diff_ez_dom)

sf_mean_diff_fm <-

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
