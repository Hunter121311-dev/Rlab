# ============================================================
# REGRESJA POISSONOWSKA DLA DANYCH METEOROLOGICZNYCH
# LICZBA GODZIN SPRZYJAJĄCYCH KITESURFINGOWI W CIĄGU DNIA
# ============================================================

# Pakiety
library(dplyr)
library(lubridate)
library(pscl)

# ------------------------------------------------------------
# 1. Wczytanie danych
# ------------------------------------------------------------

dane <- read.csv("/Users/damia/OneDrive/Dokumenty/Rlab/stat/projekt/data/chalupy_hourly_10000_days.csv")

# Konwersja czasu
dane$datetime <- as.POSIXct(dane$datetime, format = "%Y-%m-%dT%H:%M")

# Zmienne kalendarzowe
dane$data <- as.Date(dane$datetime)
dane$rok <- year(dane$datetime)
dane$miesiac <- month(dane$datetime)
dane$dzien_roku <- yday(dane$datetime)
dane$godzina <- hour(dane$datetime)

# ------------------------------------------------------------
# 2. Definicja godziny sprzyjającej kitesurfingowi
# ------------------------------------------------------------

# Progi - dostosuj w razie potrzeby do jednostek i założeń
min_wiatr <- 12
max_wiatr <- 40
max_poryw <- 55

# Godzina dobra do pływania:
# - wiatr w odpowiednim zakresie
# - porywy nie za silne
# - jest dzień
dane$good_hour <- ifelse(
  dane$wind_speed_10m >= min_wiatr &
    dane$wind_speed_10m <= max_wiatr &
    dane$wind_gusts_10m <= max_poryw &
    dane$is_day == 1,
  1, 0
)

# ------------------------------------------------------------
# 3. Agregacja danych do poziomu dziennego
# ------------------------------------------------------------

dane_dzienne <- dane %>%
  group_by(data) %>%
  summarise(
    good_hours = sum(good_hour, na.rm = TRUE),          # zmienna zależna
    mean_wind_10m = mean(wind_speed_10m, na.rm = TRUE),
    max_wind_10m = max(wind_speed_10m, na.rm = TRUE),
    mean_gusts_10m = mean(wind_gusts_10m, na.rm = TRUE),
    max_gusts_10m = max(wind_gusts_10m, na.rm = TRUE),
    mean_wind_100m = mean(wind_speed_100m, na.rm = TRUE),
    mean_temp = mean(temperature_2m, na.rm = TRUE),
    mean_humidity = mean(relative_humidity_2m, na.rm = TRUE),
    mean_pressure = mean(pressure_msl, na.rm = TRUE),
    sum_precipitation = sum(precipitation, na.rm = TRUE),
    mean_cloud_cover = mean(cloud_cover, na.rm = TRUE),
    sum_shortwave = sum(shortwave_radiation, na.rm = TRUE),
    day_hours = sum(is_day, na.rm = TRUE),              # liczba godzin dziennych
    rok = first(rok),
    miesiac = first(miesiac),
    dzien_roku = first(dzien_roku)
  ) %>%
  ungroup()

# Zmienne sezonowe
dane_dzienne$sin_dzien <- sin(2 * pi * dane_dzienne$dzien_roku / 365)
dane_dzienne$cos_dzien <- cos(2 * pi * dane_dzienne$dzien_roku / 365)

# Podgląd
head(dane_dzienne)
summary(dane_dzienne)

# ------------------------------------------------------------
# 4. Wstępna eksploracja
# ------------------------------------------------------------

# Rozkład liczby dobrych godzin
table(dane_dzienne$good_hours)

plot(
  table(dane_dzienne$good_hours),
  main = "Rozkład liczby dobrych godzin w ciągu dnia",
  xlab = "Liczba dobrych godzin",
  ylab = "Liczba dni"
)

# Liczba i udział dni z zerem
liczba_zer <- sum(dane_dzienne$good_hours == 0)
udzial_zer <- mean(dane_dzienne$good_hours == 0)

liczba_zer
udzial_zer

# Przykładowe wykresy zależności
plot(log(good_hours + 0.5) ~ cut(mean_wind_10m, breaks = 10), data = dane_dzienne,
     xlab = "Średnia dzienna prędkość wiatru 10 m",
     ylab = "log(good_hours + 0.5)",
     main = "Dobre godziny a średnia prędkość wiatru")

plot(log(good_hours + 0.5) ~ cut(max_gusts_10m, breaks = 10), data = dane_dzienne,
     xlab = "Maksymalne porywy",
     ylab = "log(good_hours + 0.5)",
     main = "Dobre godziny a maksymalne porywy")

plot(log(good_hours + 0.5) ~ cut(day_hours, breaks = unique(quantile(dane_dzienne$day_hours, 0:4/4))), 
     data = dane_dzienne,
     xlab = "Liczba godzin dziennych",
     ylab = "log(good_hours + 0.5)",
     main = "Dobre godziny a długość dnia")

# ------------------------------------------------------------
# 5. Pełny model regresji Poissonowskiej
# ------------------------------------------------------------

pois_fit <- glm(
  good_hours ~ mean_wind_10m + max_wind_10m + mean_gusts_10m + max_gusts_10m +
    mean_wind_100m + mean_temp + mean_humidity + mean_pressure +
    sum_precipitation + mean_cloud_cover + sum_shortwave + day_hours +
    sin_dzien + cos_dzien,
  data = dane_dzienne,
  family = poisson
)

summary(pois_fit)

# Ilorazy intensywności
exp(coef(pois_fit))

# AIC i logLik
AIC(pois_fit)
logLik(pois_fit)

# Nadmierna dyspersja
mean(dane_dzienne$good_hours)
var(dane_dzienne$good_hours)
deviance(pois_fit) / df.residual(pois_fit)

# ------------------------------------------------------------
# 6. Pełny model ZIP
# ------------------------------------------------------------

zip_full_fit <- zeroinfl(
  good_hours ~ mean_wind_10m + max_wind_10m + mean_gusts_10m + max_gusts_10m +
    mean_wind_100m + mean_temp + mean_humidity + mean_pressure +
    sum_precipitation + mean_cloud_cover + sum_shortwave + day_hours +
    sin_dzien + cos_dzien,
  data = dane_dzienne
)

summary(zip_full_fit)

AIC(zip_full_fit)
logLik(zip_full_fit)

# ------------------------------------------------------------
# 7. Model ZIP zredukowany
# ------------------------------------------------------------

# Część count: czynniki wpływające na liczbę dobrych godzin
# Część zero: czynniki wpływające na prawdopodobieństwo dnia bez ani jednej dobrej godziny

zip_reduced_fit <- zeroinfl(
  good_hours ~ mean_wind_10m + max_gusts_10m + mean_pressure + sum_precipitation +
    mean_cloud_cover + day_hours + sin_dzien + cos_dzien |
    mean_wind_10m + max_gusts_10m + sum_precipitation + day_hours + cos_dzien,
  data = dane_dzienne
)

summary(zip_reduced_fit)

AIC(zip_reduced_fit)
logLik(zip_reduced_fit)

# ------------------------------------------------------------
# 8. Porównanie modeli
# ------------------------------------------------------------

porownanie_modeli <- data.frame(
  model = c("Poisson", "ZIP full", "ZIP reduced"),
  logLik = c(
    as.numeric(logLik(pois_fit)),
    as.numeric(logLik(zip_full_fit)),
    as.numeric(logLik(zip_reduced_fit))
  ),
  AIC = c(
    AIC(pois_fit),
    AIC(zip_full_fit),
    AIC(zip_reduced_fit)
  )
)

porownanie_modeli

# ------------------------------------------------------------
# 9. Predykcje
# ------------------------------------------------------------

pois_pred <- predict(pois_fit, type = "response")
zip_pred <- predict(zip_reduced_fit, type = "response")

head(pois_pred)
head(zip_pred)

mean(dane_dzienne$good_hours)
mean(pois_pred)
mean(zip_pred)

# ------------------------------------------------------------
# 10. Prawdopodobieństwo zera
# ------------------------------------------------------------

# Poisson
pois_prob_zero <- dpois(0, lambda = pois_pred)
mean(pois_prob_zero)

# ZIP
zip_prob_zero <- predict(zip_reduced_fit, type = "zero")
head(zip_prob_zero)
mean(zip_prob_zero)

# ------------------------------------------------------------
# 11. Ilorazy intensywności - interpretacja praktyczna
# ------------------------------------------------------------

# Ilorazy intensywności dla modelu Poissonowskiego
irr_pois <- exp(coef(pois_fit))
irr_pois

# Ilorazy intensywności dla części count modelu ZIP
irr_zip_count <- exp(coef(zip_reduced_fit, model = "count"))
irr_zip_count

# Ilorazy szans dla części zero modelu ZIP
or_zip_zero <- exp(coef(zip_reduced_fit, model = "zero"))
or_zip_zero