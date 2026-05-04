library(lubridate)
library(dplyr)

dane <- read.csv("/Users/damia/OneDrive/Dokumenty/Rlab/stat/projekt/data/chalupy_hourly_10000_days.csv")

dane$datetime <- as.POSIXct(dane$datetime, format = "%Y-%m-%dT%H:%M")
dane$dzien <- yday(dane$datetime)
dane$godzina <- hour(dane$datetime)

n <- 1

train <- dane[year(dane$datetime) < 2025, ]
test  <- dane[year(dane$datetime) >= 2025, ]

train$wind  <- lead(train$wind_speed_10m, n)
train$windg <- lead(train$wind_gusts_10m, n)

test$wind  <- lead(test$wind_speed_10m, n)
test$windg <- lead(test$wind_gusts_10m, n)

train <- train[!is.na(train$wind) & !is.na(train$windg), ]
test  <- test[!is.na(test$wind) & !is.na(test$windg), ]
model_wind <- lm(
  wind ~ wind_speed_100m +
    temperature_2m +
    relative_humidity_2m +
    cloud_cover +
    precipitation +
    weather_code +
    pressure_msl +
    shortwave_radiation +
    sin(2*pi*dzien/365) +
    cos(2*pi*dzien/365) +
    sin(2*pi*godzina/24) +
    cos(2*pi*godzina/24),
  data = train
)

model_wind_g <- lm(
  windg ~ wind_speed_100m +
    temperature_2m +
    relative_humidity_2m +
    cloud_cover +
    precipitation +
    weather_code +
    pressure_msl +
    shortwave_radiation +
    sin(2*pi*dzien/365) +
    cos(2*pi*dzien/365) +
    sin(2*pi*godzina/24) +
    cos(2*pi*godzina/24),
  data = train
)

test$pred_wind  <- predict(model_wind, newdata = test)
test$pred_windg <- predict(model_wind_g, newdata = test)

# wybór dokładnie 2 dni
start_day <- as.Date("2025-06-02")
end_day <- start_day + 1

wykres <- test[as.Date(test$datetime) >= start_day & as.Date(test$datetime) <= end_day, ]

# WIATR
plot(
  wykres$datetime,
  wykres$wind,
  type = "p",
  pch = 16,
  col = "black",
  xlab = "Czas",
  ylab = "Wiatr [km/h]",
  main = paste("Wiatr - dane i predykcja:", start_day, "-", end_day)
)

lines(
  wykres$datetime,
  wykres$pred_wind,
  col = "red",
  lwd = 2
)

# PORYWY
plot(
  wykres$datetime,
  wykres$windg,
  type = "p",
  pch = 16,
  col = "black",
  xlab = "Czas",
  ylab = "Porywy wiatru [km/h]",
  main = paste("Porywy wiatru - dane i predykcja:", start_day, "-", end_day)
)

lines(
  wykres$datetime,
  wykres$pred_windg,
  col = "red",
  lwd = 2
)

summary(model_wind)
summary(model_wind_g)
