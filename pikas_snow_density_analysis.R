# Sierra Nevada SNOTEL: Oct–Feb snow density
# Publication-quality plot: daily mean (black) + ±1 SD ribbon (gray) +
# monthly means (red circles) + ±1 SD error bars with caps
#
# Requires: snotelr, tidyverse, lubridate, janitor
# install.packages("snotelr", "tidyverse", "lubridate", "janitor", "stringr", "scales")

library(snotelr)
library(tidyverse)
library(lubridate)
library(janitor)
library(stringr)
library(scales)

# --------------------------
# Theme (matches your style)
# --------------------------
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
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.key       = element_blank(),
      strip.background = element_rect(fill = "white", colour = "black", linewidth = rel(2)),
      complete = TRUE
    )
}
theme_set(theme_classic(12))

# ---- User controls ----
sierra_bbox <- list(
  lon_min = -122.8,
  lon_max = -116.5,
  lat_min =  35.0,
  lat_max =  41.7
)
states_keep <- c("CA", "NV")
min_date <- as.Date("1950-10-01")
max_date <- Sys.Date()

# -----------------------------
# 1) Site metadata + filter
# -----------------------------
sites_raw <- snotel_info() %>% clean_names()

lat_col   <- intersect(names(sites_raw), c("latitude", "lat"))[1]
lon_col   <- intersect(names(sites_raw), c("longitude", "lon", "long"))[1]
id_col    <- intersect(names(sites_raw), c("site_id", "id"))[1]
state_col <- intersect(names(sites_raw), c("state", "state_code"))[1]
net_col   <- intersect(names(sites_raw), c("network", "station_network", "site_network"))[1]
type_col  <- intersect(names(sites_raw), c("site_type", "station_type", "type"))[1]

stopifnot(!is.na(lat_col), !is.na(lon_col), !is.na(id_col))

sites0 <- sites_raw %>%
  mutate(
    lat_num = as.numeric(.data[[lat_col]]),
    lon_num = as.numeric(.data[[lon_col]])
  )

sites1 <- sites0 %>%
  filter(
    !is.na(lat_num), !is.na(lon_num),
    between(lon_num, sierra_bbox$lon_min, sierra_bbox$lon_max),
    between(lat_num, sierra_bbox$lat_min, sierra_bbox$lat_max)
  )

sites2 <- sites1 %>%
  { if (!is.na(state_col)) filter(., .data[[state_col]] %in% states_keep) else . }

# Optional SNOTEL-only filter using OR logic; skip if it would remove everything
sites3 <- sites2 %>%
  mutate(
    net_u  = if (!is.na(net_col))  str_to_upper(coalesce(as.character(.data[[net_col]]),  "")) else NA_character_,
    type_u = if (!is.na(type_col)) str_to_upper(coalesce(as.character(.data[[type_col]]), "")) else NA_character_,
    is_snotel = case_when(
      !is.na(net_u)  & str_detect(net_u,  "SNOTEL") ~ TRUE,
      !is.na(type_u) & str_detect(type_u, "SNOTEL") ~ TRUE,
      TRUE ~ FALSE
    )
  )

sites_sierra <- if (sum(sites3$is_snotel, na.rm = TRUE) > 0) {
  sites3 %>% filter(is_snotel)
} else {
  message("NOTE: net/type columns didn't flag any SNOTEL rows; skipping SNOTEL-only filter.")
  sites2
}

sites_sierra <- sites_sierra %>%
  distinct(.data[[id_col]], .keep_all = TRUE) %>%
  rename(site_id = all_of(id_col))

message("Sierra sites found: ", nrow(sites_sierra))
if (nrow(sites_sierra) == 0) stop("No sites found; adjust bbox/state filters.")

# -----------------------------
# 2) Download daily time series
# -----------------------------
safe_download <- purrr::possibly(
  function(id) {
    snotel_download(site_id = id, internal = TRUE) %>%
      clean_names() %>%
      mutate(site_id = id)
  },
  otherwise = NULL
)

snow_all <- sites_sierra %>%
  pull(site_id) %>%
  sort() %>%
  purrr::map(safe_download) %>%
  purrr::compact() %>%
  bind_rows()

if (nrow(snow_all) == 0) stop("No data downloaded.")

# -----------------------------------------
# 3) Density + Oct–Feb + season_day index
# -----------------------------------------
needed <- c("date", "snow_water_equivalent", "snow_depth", "site_id")
missing <- setdiff(needed, names(snow_all))
if (length(missing) > 0) {
  stop("Missing expected columns: ", paste(missing, collapse = ", "),
       "\nInspect names(snow_all) and adjust.")
}

dens <- snow_all %>%
  mutate(
    date = as.Date(date),
    swe_mm   = suppressWarnings(as.numeric(snow_water_equivalent)),
    depth_mm = suppressWarnings(as.numeric(snow_depth)),
    density_kg_m3 = if_else(
      depth_mm > 0 & !is.na(swe_mm) & swe_mm >= 0,
      (swe_mm / depth_mm) * 1000,
      NA_real_
    )
  ) %>%
  filter(!is.na(date)) %>%
  filter(date >= min_date, date <= max_date) %>%
  filter(month(date) %in% c(10, 11, 12, 1, 2)) %>%
  mutate(
    season_year  = if_else(month(date) >= 10L, year(date) + 1L, year(date)),
    season_start = as.Date(paste0(season_year - 1L, "-10-01")),
    season_day   = as.integer(date - season_start) + 1L
  ) %>%
  filter(!is.na(density_kg_m3)) %>%
  filter(season_day >= 1, season_day <= 153)

if (nrow(dens) == 0) stop("dens is empty after filtering; likely missing SWE/depth for many sites.")

# -----------------------------
# 4) Daily mean + SD
# -----------------------------
daily_mean <- dens %>%
  group_by(season_day) %>%
  summarise(
    mean_density_kg_m3 = mean(density_kg_m3, na.rm = TRUE),
    sd_density_kg_m3   = sd(density_kg_m3, na.rm = TRUE),
    n_obs              = n(),
    .groups = "drop"
  )

# -----------------------------
# 5) Monthly mean + SD (Oct–Feb)
# -----------------------------
monthly_stats <- dens %>%
  mutate(mon = month(date)) %>%
  group_by(mon) %>%
  summarise(
    mean_density_kg_m3 = mean(density_kg_m3, na.rm = TRUE),
    sd_density_kg_m3   = sd(density_kg_m3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(mon %in% c(10, 11, 12, 1, 2)) %>%
  mutate(
    mon_lab = factor(mon, levels = c(10, 11, 12, 1, 2),
                     labels = c("Oct", "Nov", "Dec", "Jan", "Feb"))
  )

# Place monthly points at mid-month on the season_day axis (dummy season)
season_start_dummy <- as.Date("2001-10-01")
month_mid_dates <- tibble(
  mon_lab = factor(c("Oct", "Nov", "Dec", "Jan", "Feb"),
                   levels = c("Oct", "Nov", "Dec", "Jan", "Feb")),
  mid_date = as.Date(c("2001-10-15", "2001-11-15", "2001-12-15", "2002-01-15", "2002-02-15"))
) %>%
  mutate(season_day = as.integer(mid_date - season_start_dummy) + 1L) %>%
  select(mon_lab, season_day)

monthly_stats <- monthly_stats %>%
  left_join(month_mid_dates, by = "mon_lab")

# -----------------------------
# 6) Publication-quality plot
# -----------------------------
x0 <- as.Date("2001-10-01")
break_days <- c(1, 32, 62, 93, 124, 153)

# Legend mapping via aesthetics (no “show.legend = FALSE” hacks)
p <- ggplot() +
  # SD ribbon (daily)
  geom_ribbon(
    data = daily_mean,
    aes(
      x = season_day,
      ymin = mean_density_kg_m3 - sd_density_kg_m3,
      ymax = mean_density_kg_m3 + sd_density_kg_m3,
      fill = "Daily mean ±1 SD"
    ),
    alpha = 0.35
  ) +
  # Daily mean line (black)
  geom_line(
    data = daily_mean,
    aes(x = season_day, y = mean_density_kg_m3, color = "Daily mean"),
    linewidth = 0.9
  ) +
  # Monthly mean ± SD with caps + circles
  geom_errorbar(
    data = monthly_stats,
    aes(
      x = season_day,
      ymin = mean_density_kg_m3 - sd_density_kg_m3,
      ymax = mean_density_kg_m3 + sd_density_kg_m3,
      color = "Monthly mean ±1 SD"
    ),
    width = 6, linewidth = 0.6  # width gives caps
  ) +
  geom_point(
    data = monthly_stats,
    aes(x = season_day, y = mean_density_kg_m3, color = "Monthly mean ±1 SD"),
    shape = 16, size = 2.3
  ) +
  scale_x_continuous(
    breaks = break_days,
    labels = format(x0 + (break_days - 1), "%b %d"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.04))) +
  scale_x_continuous(expand = expansion(mult = c(0.008, 0.01))) +
  scale_color_manual(
    values = c("Daily mean" = "black", "Monthly mean ±1 SD" = "red"),
    breaks = c("Daily mean", "Monthly mean ±1 SD"),
    name = NULL
  ) +
  scale_fill_manual(
    values = c("Daily mean ±1 SD" = "grey70"),
    breaks = c("Daily mean ±1 SD"),
    name = NULL
  ) +
  guides(
    fill  = guide_legend(order = 1, override.aes = list(alpha = 0.35)),
    color = guide_legend(order = 2)
  ) +
  labs(
    x = "Accumulation season day (Oct 1 → March 1)",
    y = "Snow density (kg/m³)"
  ) +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.title.y = element_text(color = "black"),
    axis.text.y  = element_text(color = "black"),
    axis.ticks.y = element_line(color = "black"),
    axis.title.x = element_text(color = "black"),
    axis.text.x  = element_text(color = "black"),
    axis.ticks.x = element_line(color = "black"),
    plot.margin  = unit(c(.5, .5, .5, .5), "cm"),
    # Legend inside bottom-right
    legend.position = c(0.98, 0.05),
    legend.justification = c(1, 0),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.3),
    legend.margin = margin(4, 6, 4, 6)
  )

print(p)

# ---- Save (publication) ----
dir.create("./plots", showWarnings = FALSE)
ggsave(
  filename = "./plots/snotel_sierra_density_oct_feb_mean_sd_monthly.pdf",
  plot = p,
  width = 6, height = 4.2
)
ggsave(
  filename = "./plots/snotel_sierra_density_oct_feb_mean_sd_monthly.png",
  plot = p,
  width = 6, height = 4.2,
  dpi = 600
)
getwd()
system("open ./plots/snotel_sierra_density_oct_feb_mean_sd_monthly.pdf")
