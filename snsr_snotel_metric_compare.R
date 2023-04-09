# comparing max swe to SNOTELS

library(terra)
library(dplyr)
library(ggplot2)
library(snotelr)
library(lubridate)

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

snotel_df <-read.csv("./csvs/snotel_df.csv")
head(snotel_df)

#########################
#########################
###  read in SNSR data ##
#########################
#########################

# read in csvs
snsr_snotel_list <-sort(list.files("./csvs/snsr_snotel_data", full.names = TRUE))
snsr_snotel_data <-lapply(snsr_snotel_list, read.csv)

# bind together
snsr_df <-bind_rows(snsr_snotel_data)


# bind SNSR and snotel data together





# calc snotel max for 2016
max_snsr_snotel_df <-as.data.frame(snsr_snotel_df %>%
  group_by(site_name, wy) %>%
  summarise(max_swe_mm = max_swe(snsr_swe_mm, swe_thres = 25.4)))

head(max_snsr_snotel_df)

# check to see if names are the same
unique(snotel_max_df$site_name)
unique(snotel_ca$Site_Name)

# # extract cell number from pit lat/lon point
# snotel_cell_numbers <-as.data.frame(cells(max_stack, ca_points_snsr))
# snotel_cell_numbers 
# 
# ## loop to pull create string of snotel cell and 8 surrounding 
# 
# dummy_list <-list()
# for (i in seq_along(snotel_cell_numbers$cell)){
#   neighbor_cells <-c(adjacent(max_stack, cells = snotel_cell_numbers$cell[i], directions ="8"))
#   dummy_list[[i]] <-c(snotel_cell_numbers$cell[i], neighbor_cells)
# }
# 
# # extract using that vector
# nine_cells <-terra::extract(max_stack, dummy_list[[20]], xy = TRUE)
# nine_cells

snsr_max_snotel <-terra::extract(max_stack, ca_points_snsr, 
                                 names = TRUE, 
                                 cells = TRUE, 
                                 xy = TRUE, 
                                 ID = TRUE,
                                 method = 'bilinear')
# rename cols
years <-seq(1985,2016,1)
snsr_max_snotel <-cbind(unique(snotel_df$site_name), snsr_max_snotel)
colnames(snsr_max_snotel)[3:34] <-years
colnames(snsr_max_snotel)[1] <-"site_name_v2"
snsr_max_snotel

# -c(cell,x,y,SNSR_aspect), 
snsr_max_df <-as.data.frame(pivot_longer(snsr_max_snotel, cols = 3:34, 
                                         names_to = "year",
                                         values_to = "snsr_max_mm"))
head(snsr_max_df)
head(snotel_max_df)

# make df for plotting
compare_df <-cbind(snsr_max_df, snotel_max_df)
head(compare_df)

ggplot(compare_df) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(aes(x = snsr_max_mm, y = snotel_max_mm), size = .9, color = "darkred") +
  scale_y_continuous(limits = c(0,3000),expand = (c(0,0))) +
  scale_x_continuous(limits = c(0,3000),expand = (c(0,0))) +
  ylab("SNSR Max SWE (mm)") + xlab("SNOTEL Max (mm)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1))

cor(compare_df$snotel_max_mm, compare_df$snsr_max_mm, use = "complete.obs")

# filter for 2016
big_df16 <-filter(snotel, date >= "2015-10-01" & date <= "2016-09-30")

ggplot(big_df16)+
  geom_line(aes(y = snow_water_equivalent, x = date, group = site_name), alpha = .4)

# filter for palisades
pt <-filter(snotel_df, site_name == "palisades tahoe ")
pt95 <-filter(pt, waterYear == 1995)


ggplot(pt95)+
  geom_line(aes(y = snow_water_equivalent, x = Date))

