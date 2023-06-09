# scatter plot for each of the 6 elevation zones for FM
# jack tarricone
# june 8th, 2023

library(dplyr)
library(ggplot2)
library(hydroGOF)
library(cowplot)
library(viridis)
library(ggpointdensity)
library(plyr)
library(ggpubr)


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

# set working dir
setwd("~/ch1_margulis")

# read back in using data.table
df_v1 <-fread("./csvs/fm_temp_eb_ns_csv_v6.csv")

# sample down to a milli
df <-df_v1[sample(.N, 1000000)]
head(df)

# test hists
hist(df_v1$temp_c, breaks = 50)
hist(df$temp_c, breaks = 50)
mean(df_v1$temp_c)
mean(df$temp_c)

##################################
#### make time series box plot ###
##################################

bin_stats <-as.data.frame((df) %>%
  group_by(bin_name, aspect_name) %>%
  summarise(mean_frac_melt = round(mean(frac_melt),2),
            mean_temp = round(mean(temp_c),2)))

# print
bin_stats

zone <-filter(df, ele_bin == 6 & aspect == 3)
mod <-lm(frac_melt ~ temp_c, zone)
summary(mod)
plot(mod)

## set color
scale <-c("grey",viridis(30, option = "H", direction = 1))

lm_eqn = function(df){
  m = lm(frac_melt ~ temp_c, df);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = round(coef(m)[1], digits = 2), 
                        b = round(coef(m)[2], digits = 2), 
                        r2 =round(summary(m)$r.squared, digits = 2)))
  as.character(as.expression(eq));                 
}

# stat plot
p1 <-ggplot(data = df, aes(y = frac_melt, x = temp_c)) +
  # geom_point(color = "steelblue", size = .01, alpha = .05) +
  # geom_pointdensity(adjust = 5, size = .1) +
  geom_bin2d(bins = 40, aes(fill = ..density..)) +
  scale_fill_gradientn(colors = scale) +
  scale_y_continuous(limits = c(0,1),  expand = c(0,0))+
  scale_x_continuous(limits = c(-10,10), expand = c(.1,.1))+
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  stat_cor(method = "spearman", label.y = .92, size = 3, color = "darkred")+ 
  stat_regline_equation(label.y = .82, size = 3, color = "darkred") +
  labs(y = "FM", x = expression('ONDJFM ' ~T["Mean"] ~ '(°C)')) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1))+
  facet_wrap(~ aspect_name + bin_name, ncol = 6) 

# # add lm statistics
# eq <- ddply(df,.(bin_name, aspect_name),lm_eqn)
# p2 <- p1 +  geom_text(data=eq,aes(x = -2, y = .92,label=V1), color = "darkred", size = 2, parse = TRUE, inherit.aes=FALSE) + 
#   facet_wrap(~ aspect_name + bin_name, ncol = 6) 
# p2
p1

# test save
ggsave(file = "./plots/temp_fm_facet_v7.png",
       width = 11, 
       height = 5,
       units = "in",
       dpi = 300) 

system("open ./plots/temp_fm_facet_v7.png")

# test plotting just slopes
slopes <-c(.069,.055,.036,.024,.012,.0043)
intercepts <-c(.033,.08,.11,.085,.068,.0043)
my_colors <-c("#f8f9ac","#89a02e","#ba4202","#751504","#7d4a2b","#b5b5b5")

ggplot()+
  geom_abline(aes(slope = slopes[1], intercept = intercepts[1], color = "ez1"), linewidth = 1.5)+
  geom_abline(aes(slope = slopes[2], intercept = intercepts[2], color = "ez2"), linewidth = 1.5)+
  geom_abline(aes(slope = slopes[3], intercept = intercepts[3], color = "ez3"), linewidth = 1.5)+
  geom_abline(aes(slope = slopes[4], intercept = intercepts[4], color = "ez4"), linewidth = 1.5)+
  geom_abline(aes(slope = slopes[5], intercept = intercepts[5], color = "ez5"), linewidth = 1.5)+
  geom_abline(aes(slope = slopes[6], intercept = intercepts[6], color = "ez6"), linewidth = 1.5)+
  scale_color_manual(name = "Elevation Zones",
                     values = c("ez1"=my_colors[1], "ez2"=my_colors[2], "ez3"=my_colors[3], 
                                "ez4"=my_colors[4], "ez5"=my_colors[5], "ez6"=my_colors[6]),
                     labels = c('1500-1900 m', '1900-2300 m', '2300-2700 m',
                                '2700-3100 m','3100-3500 m', '3500-4361 m'))+
  scale_y_continuous(limits = c(0,.5)) +
  scale_x_continuous(limits = c(-6,6)) +
  labs(y = "FM", x = expression('ONDJFM ' ~T["Mean"] ~ '(°C)')) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1),
        legend.position = c(.2,.75))

# test save
ggsave(file = "./plots/slopes_ez_temp_fm_v1.png",
       width = 6, 
       height = 5,
       units = "in",
       dpi = 300) 

system("open ./plots/slopes_ez_temp_fm_v1.png")




