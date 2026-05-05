library(tidyverse)
library(ggplot2)
library(ggmap)
library(geosphere)
library(grid)

# ustawienie folderu roboczego

setwd("~/Rlab/R lang/proj3/")

# wczytanie danych

ext_tracks_widths <- c(
  7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3,
  4, 3, 3, 3,
  4, 3, 3, 3,
  4, 3, 3, 3,
  2, 6, 1
)

ext_tracks_colnames <- c(
  "storm_code", "storm_name", "month", "day", "hour", "year",
  "latitude", "longitude", "max_wind", "min_pressure",
  "rad_max_wind", "eye_diameter",
  "pressure_1", "pressure_2",
  paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
  paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
  paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
  "storm_type", "dist_land", "hemisphere"
)

hurricane_data <- read.fwf(
  "ebtrk_atlc_1988_2015.txt",
  widths = ext_tracks_widths,
  col.names = ext_tracks_colnames,
  header = FALSE,
  strip.white = TRUE,
  na.strings = "-99",
  stringsAsFactors = FALSE
)

# przetwarzanie danych

hurricane_data <- hurricane_data %>%
  mutate(
    storm_name = str_to_title(str_to_lower(storm_name)),
    storm_id = paste0(storm_name, "-", year),
    longitude = -longitude,
    date = as.POSIXct(
      sprintf("%04d-%02d-%02d %02d:00:00", year, month, day, hour),
      tz = "UTC"
    )
  )

hurricane_long <- hurricane_data %>%
  select(
    storm_id, storm_name, date, latitude, longitude,
    radius_34_ne, radius_34_se, radius_34_sw, radius_34_nw,
    radius_50_ne, radius_50_se, radius_50_sw, radius_50_nw,
    radius_64_ne, radius_64_se, radius_64_sw, radius_64_nw
  ) %>%
  pivot_longer(
    cols = starts_with("radius_"),
    names_to = c("wind_speed", "quadrant"),
    names_pattern = "radius_(\\d+)_(ne|se|sw|nw)",
    values_to = "radius"
  ) %>%
  mutate(
    wind_speed = factor(wind_speed, levels = c("34", "50", "64"))
  ) %>%
  pivot_wider(
    names_from = quadrant,
    values_from = radius
  ) %>%
  filter(!is.na(ne), !is.na(se), !is.na(sw), !is.na(nw))

# definicja geom

StatHurricane <- ggproto(
  "StatHurricane",
  Stat,
  
  required_aes = c("x", "y", "r_ne", "r_se", "r_sw", "r_nw"),
  
  compute_panel = function(data, scales, scale_radii = 1, n = 90) {
    pieces <- vector("list", nrow(data))
    
    for (i in seq_len(nrow(data))) {
      row <- data[i, ]
      
      bearings <- c(
        seq(0, 90, length.out = n),
        seq(90, 180, length.out = n),
        seq(180, 270, length.out = n),
        seq(270, 360, length.out = n)
      )
      
      radii_nm <- c(
        rep(row$r_ne, n),
        rep(row$r_se, n),
        rep(row$r_sw, n),
        rep(row$r_nw, n)
      )
      
      radii_m <- radii_nm * 1852 * scale_radii
      
      coords <- destPoint(
        p = c(row$x, row$y),
        b = bearings,
        d = radii_m
      )
      
      out <- row[rep(1, length(bearings)), ]
      out$x <- coords[, 1]
      out$y <- coords[, 2]
      out$group <- i
      out$order <- seq_along(bearings)
      
      pieces[[i]] <- out
    }
    
    bind_rows(pieces)
  }
)

geom_hurricane <- function(
    mapping = NULL,
    data = NULL,
    stat = "hurricane",
    position = "identity",
    ...,
    scale_radii = 1,
    n = 90,
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatHurricane,
    geom = GeomPolygon,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      scale_radii = scale_radii,
      n = n,
      na.rm = na.rm,
      ...
    )
  )
}

# filtrowanie obserwacji

ike <- hurricane_long %>%
  filter(storm_id == "Ike-2008") %>%
  filter(date == as.POSIXct("2008-09-13 06:00:00", tz = "UTC")) %>%
  arrange(wind_speed)

# wykres

p <- ggplot() +
  borders(
    "state",
    regions = c("texas", "louisiana"),
    fill = "gray95",
    colour = "gray60"
  ) +
  geom_hurricane(
    data = ike,
    aes(
      x = longitude,
      y = latitude,
      r_ne = ne,
      r_se = se,
      r_sw = sw,
      r_nw = nw,
      fill = wind_speed,
      color = wind_speed
    ),
    alpha = 0.35,
    linewidth = 0.8
  ) +
  coord_quickmap(
    xlim = c(-99, -88),
    ylim = c(25, 32)
  ) +
  scale_fill_manual(
    name = "Wind speed (kts)",
    breaks = c("34", "50", "64"),
    values = c("34" = "yellow", "50" = "orange", "64" = "red")
  ) +
  scale_color_manual(
    name = "Wind speed (kts)",
    breaks = c("34", "50", "64"),
    values = c("34" = "yellow", "50" = "orange", "64" = "red")
  ) +
  labs(
    title = "Hurricane Ike, 2008-09-13 06:00 UTC",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

p

# zapisanie wynikowego wykresu

ggsave(
  "ike_hurricane_geom.png",
  plot = p,
  width = 9,
  height = 7,
  dpi = 300
)

# zmieniony wykres po przeskalowaniu

p_scaled <- ggplot() +
  borders(
    "state",
    regions = c("texas", "louisiana"),
    fill = "gray95",
    colour = "gray60"
  ) +
  geom_hurricane(
    data = ike,
    aes(
      x = longitude,
      y = latitude,
      r_ne = ne,
      r_se = se,
      r_sw = sw,
      r_nw = nw,
      fill = wind_speed,
      color = wind_speed
    ),
    alpha = 0.35,
    linewidth = 0.8,
    scale_radii = 0.5
  ) +
  coord_quickmap(
    xlim = c(-99, -88),
    ylim = c(25, 32)
  ) +
  scale_fill_manual(
    name = "Wind speed (kts)",
    breaks = c("34", "50", "64"),
    values = c("34" = "yellow", "50" = "orange", "64" = "red")
  ) +
  scale_color_manual(
    name = "Wind speed (kts)",
    breaks = c("34", "50", "64"),
    values = c("34" = "yellow", "50" = "orange", "64" = "red")
  ) +
  labs(
    title = "Hurricane Ike, 2008-09-13 06:00 UTC, scale_radii = 0.5",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()

p_scaled

# zapisanie wynikowego wykresu

ggsave(
  "ike_hurricane_geom_scaled.png",
  plot = p_scaled,
  width = 9,
  height = 7,
  dpi = 300
)
