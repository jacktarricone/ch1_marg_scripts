####### start mk results analysis table
# jack tarricone
# april 19th, 2023

library(terra)
library(dplyr)
library(tidyr)
library(data.table)
library(ggplot2)

theme_classic <- function(base_size = 11, base_family = "",
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

theme_set(theme_classic(12))

setwd("~/ch1_margulis")

#####################################
# head_df for col referencing
##############################################
df <-fread("./csvs/hydro_cat/full_df_hydro_cat_v1.csv")
df$basin_name <-ifelse(df$basin_name == "kern","Kern",df$basin_name)
df$basin_name <-ifelse(df$basin_name == "usj","USJ",df$basin_name)
df$basin_name <-ifelse(df$basin_name == "yuba","Yuba",df$basin_name)
df$hydr0_cat <-ifelse(df$hydr0_cat == "cw","CW",df$hydr0_cat)
df$hydr0_cat <-ifelse(df$hydr0_cat == "hd","HD",df$hydr0_cat)

# creat new zones of 200 m
df$ez2 <-NA
df$ez2 <-ifelse(df$elevation > 1500 & df$elevation <= 1700, 1, df$ez2)
df$ez2 <-ifelse(df$elevation > 1700 & df$elevation <= 1900, 2, df$ez2)
df$ez2 <-ifelse(df$elevation > 1900 & df$elevation <= 2100, 3, df$ez2)
df$ez2 <-ifelse(df$elevation > 2100 & df$elevation <= 2300, 4, df$ez2)
df$ez2 <-ifelse(df$elevation > 2300 & df$elevation <= 2500, 5, df$ez2)
df$ez2 <-ifelse(df$elevation > 2500 & df$elevation <= 2700, 6, df$ez2)
df$ez2 <-ifelse(df$elevation > 2700 & df$elevation <= 2900, 7, df$ez2)
df$ez2 <-ifelse(df$elevation > 2900 & df$elevation <= 3100, 8, df$ez2)
df$ez2 <-ifelse(df$elevation > 3100 & df$elevation <= 3300, 9, df$ez2)
df$ez2 <-ifelse(df$elevation > 3300 & df$elevation <= 3500, 10, df$ez2)
df$ez2 <-ifelse(df$elevation > 3500 & df$elevation <= 3700, 11, df$ez2)
df$ez2 <-ifelse(df$elevation > 3700 & df$elevation <= 3900, 12, df$ez2)
df$ez2 <-ifelse(df$elevation > 3900 & df$elevation <= 4100, 13, df$ez2)
df$ez2 <-ifelse(df$elevation > 4100 & df$elevation <= 4400, 14, df$ez2)

# rename
# df$bin_name <-ifelse(df$ez == 1, "1500-1900 m", df$ez)
# df$bin_name <-ifelse(df$ez == 2, "1900-2300 m", df$bin_name)
# df$bin_name <-ifelse(df$ez == 3, "2300-2700 m", df$bin_name)
# df$bin_name <-ifelse(df$ez == 4, "2700-3100 m", df$bin_name)
# df$bin_name <-ifelse(df$ez == 5, "3100-4361 m", df$bin_name)
# df$bin_name <-ifelse(df$ez == 6, "3100-4361 m", df$bin_name)

# filter aspects
df <-dplyr::filter(df, aspect != 2 & aspect != 4)
df <-dplyr::filter(df, hydr0_cat != "hw" & hydr0_cat != "cd")

# check
head(df)
unique(df$hydr0_cat)

# filter
df$aspect_name <-ifelse(df$aspect ==  1, "North", df$aspect)
df$aspect_name <-ifelse(df$aspect ==  3, "South", df$aspect_name)
head(df)




######################################
######################################
############ snow data ###############
######################################
######################################

snow_results <-df %>%
  group_by(basin_name, ez2, hydr0_cat, aspect_name) %>%
  summarise(mean_ez_mswe = as.integer(mean(mean_mswe_mm)),
            mean_ez_dom = as.integer(mean(mean_dom_dowy)),
            mean_ez_fm = round(mean(mean_fm),2),
            mean_ez_mwa = as.integer(mean(mean_mwa)))

snow_results_cw <-filter(snow_results, hydr0_cat == "cw" & basin_name == "kern")
snow_results_hd <-filter(snow_results, hydr0_cat == "hd" & basin_name == "kern")
snow_results_cw
snow_results_hd

# Reshape the data into separate columns for aspect_name
snow_results_wide <- snow_results  %>%
  tidyr::pivot_wider(names_from = aspect_name,
              values_from = c(mean_ez_mswe, mean_ez_dom, mean_ez_fm, mean_ez_mwa))


# calc diff
df_diff <- snow_results_wide %>%
  mutate(
    diff_ez_mswe = mean_ez_mswe_South - mean_ez_mswe_North,
    diff_ez_dom = mean_ez_dom_South - mean_ez_dom_North,
    diff_ez_fm = mean_ez_fm_South - mean_ez_fm_North,
    diff_ez_mwa = mean_ez_mwa_South - mean_ez_mwa_North
  )

# Sort the dataframe
df_sorted <- df_diff %>%
  select(basin_name, ez2, matches("mswe"), matches("dom"), matches("fm"), matches("mwa"))

# View the sorted dataframe
df_sorted
kern <-filter(df_sorted, basin_name == "kern")

ggplot(df_sorted, aes(x=ez2,y=diff_ez_fm,linetype=basin_name,color=hydr0_cat)) +
  geom_line() +
  ylab("North/South FM Difference (-)") + xlab("Elevation Zone")+
  labs(linetype="Basin", color="Hydro Cat")+
  scale_x_continuous(limits = c(1,14), breaks = seq(1,13,2))+
  scale_linetype_manual(values = c("Kern" = "solid", "USJ" = "dotted", "Yuba" = "longdash")) +
    theme(panel.border = element_rect(colour = "black", fill = NA, linewidth  = 1),
          legend.position = 'top',
          legend.direction = 'horizontal',
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0),
          plot.margin = unit(c(.25,.25, 0,.25), "cm"))


ggplot(df_sorted, aes(x=ez2,y=diff_ez_mswe,linetype=basin_name,color=hydr0_cat,
                      group=interaction(basin_name,hydr0_cat))) +
  geom_line() 

# save
write.csv(df_sorted, "./csvs/snow_avg_metric_results_table_v4.csv", row.names = FALSE)
system("open ./csvs/snow_avg_metric_results_table_v4.csv")


######################################
######################################
############ met data ###############
######################################
######################################

# met data
met_results <-df %>%
  group_by(basin_name, zone_name, aspect_name) %>%
  summarise(mean_ez_temp_c  = round(mean(mean_temp_c),2),
            mean_ez_ah = round(mean(mean_ah),2),
            mean_ez_srad = as.integer(mean(mean_srad)),
            mean_cs_insol  = as.integer(mean(insol_watts)))

print(met_results, n = nrow(met_results))

# Reshape the data into separate columns for aspect_name
met_results_wide <- met_results   %>%
  tidyr::pivot_wider(names_from = aspect_name,
                     values_from = c(mean_ez_temp_c, mean_ez_ah, mean_ez_srad, mean_cs_insol))

# calc diff
met_diff <- met_results_wide  %>%
  mutate(
    diff_ez_temp_c = mean_ez_temp_c_South - mean_ez_temp_c_North,
    diff_ez_ah = mean_ez_ah_South - mean_ez_ah_North,
    diff_ez_srad = mean_ez_srad_South - mean_ez_srad_North,
    diff_ez_insol = mean_cs_insol_South - mean_cs_insol_North
    
  )

# Sort the dataframe
met_df_sorted <- met_diff %>%
  select(basin_name, zone_name, matches("temp"), matches("ah"), matches("srad"), matches("insol"))

# View the sorted dataframe
met_df_sorted

# save
write.csv(met_df_sorted, "./csvs/met_avg_metric_results_table_v2.csv", row.names = FALSE)
system("open ./csvs/met_avg_metric_results_table_v2.csv")



