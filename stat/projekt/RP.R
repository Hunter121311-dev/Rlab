# ============================================================
# REGRESJA POISSONOWSKA DLA DANYCH METEOROLOGICZNYCH
# LICZBA GODZIN SPRZYJAJĄCYCH KITESURFINGOWI W CIĄGU DNIA
# ============================================================

# ------------------------------------------------------------
# 0. Pakiety i ustawienia
# ------------------------------------------------------------

library(dplyr)
library(lubridate)
library(pscl)

# Foldery na wyniki i wykresy
plot_dir <- "plots_poisson"
results_dir <- "wyniki_poisson"

dir.create(plot_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)

# Funkcja pomocnicza do zapisywania wykresów
zapisz_wykres <- function(nazwa, kod) {
  png(
    filename = file.path(plot_dir, nazwa),
    width = 1200,
    height = 800,
    res = 140
  )
  on.exit(dev.off(), add = TRUE)
  eval(substitute(kod), envir = parent.frame())
}

# Funkcja do bezpiecznego cięcia zmiennej na kwartyle
cut_kwartyle <- function(x) {
  br <- unique(quantile(x, probs = seq(0, 1, 0.25), na.rm = TRUE))
  if (length(br) < 2) {
    return(factor(x))
  } else {
    return(cut(x, breaks = br, include.lowest = TRUE))
  }
}

# Funkcje metryk predykcyjnych
rmse <- function(y, y_hat) sqrt(mean((y - y_hat)^2, na.rm = TRUE))
mae <- function(y, y_hat) mean(abs(y - y_hat), na.rm = TRUE)

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

min_wiatr <- 12
max_wiatr <- 40
max_poryw <- 55

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
    good_hours = sum(good_hour, na.rm = TRUE),
    
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
    
    day_hours = sum(is_day, na.rm = TRUE),
    
    n_hours = n(),
    rok = first(rok),
    miesiac = first(miesiac),
    dzien_roku = first(dzien_roku)
  ) %>%
  ungroup()

# Zmienne sezonowe
dane_dzienne$sin_dzien <- sin(2 * pi * dane_dzienne$dzien_roku / 365)
dane_dzienne$cos_dzien <- cos(2 * pi * dane_dzienne$dzien_roku / 365)

# Zmienne jakościowe do wykresów eksploracyjnych
dane_dzienne$miesiac_factor <- factor(
  dane_dzienne$miesiac,
  levels = 1:12,
  labels = c("I", "II", "III", "IV", "V", "VI",
             "VII", "VIII", "IX", "X", "XI", "XII")
)

dane_dzienne$sezon <- case_when(
  dane_dzienne$miesiac %in% c(12, 1, 2) ~ "Zima",
  dane_dzienne$miesiac %in% c(3, 4, 5) ~ "Wiosna",
  dane_dzienne$miesiac %in% c(6, 7, 8) ~ "Lato",
  dane_dzienne$miesiac %in% c(9, 10, 11) ~ "Jesień"
)

dane_dzienne$sezon <- factor(
  dane_dzienne$sezon,
  levels = c("Zima", "Wiosna", "Lato", "Jesień")
)

# Usunięcie ewentualnych braków w zmiennych modelowych
dane_dzienne <- dane_dzienne %>%
  filter(complete.cases(
    good_hours,
    mean_wind_10m,
    max_wind_10m,
    mean_gusts_10m,
    max_gusts_10m,
    mean_wind_100m,
    mean_temp,
    mean_humidity,
    mean_pressure,
    sum_precipitation,
    mean_cloud_cover,
    sum_shortwave,
    day_hours,
    sin_dzien,
    cos_dzien
  ))

# ------------------------------------------------------------
# 4. Eksploracja zmiennej zależnej good_hours
# ------------------------------------------------------------

head(dane_dzienne)
summary(dane_dzienne)

rozkład_good_hours <- as.data.frame(table(dane_dzienne$good_hours))
colnames(rozkład_good_hours) <- c("good_hours", "liczba_dni")

liczba_zer <- sum(dane_dzienne$good_hours == 0)
udzial_zer <- mean(dane_dzienne$good_hours == 0)

opis_good_hours <- data.frame(
  liczba_dni = nrow(dane_dzienne),
  min = min(dane_dzienne$good_hours),
  q1 = quantile(dane_dzienne$good_hours, 0.25),
  mediana = median(dane_dzienne$good_hours),
  srednia = mean(dane_dzienne$good_hours),
  q3 = quantile(dane_dzienne$good_hours, 0.75),
  max = max(dane_dzienne$good_hours),
  wariancja = var(dane_dzienne$good_hours),
  stosunek_wariancji_do_sredniej = var(dane_dzienne$good_hours) / mean(dane_dzienne$good_hours),
  liczba_zer = liczba_zer,
  udzial_zer = udzial_zer
)

rozkład_good_hours
opis_good_hours

write.csv(rozkład_good_hours, file.path(results_dir, "rozkład_good_hours.csv"), row.names = FALSE)
write.csv(opis_good_hours, file.path(results_dir, "opis_good_hours.csv"), row.names = FALSE)

# Wykres rozkładu liczby dobrych godzin
zapisz_wykres("01_rozkład_good_hours.png", {
  plot(
    table(dane_dzienne$good_hours),
    main = "Rozkład liczby dobrych godzin w ciągu dnia",
    xlab = "Liczba dobrych godzin",
    ylab = "Liczba dni"
  )
})

# Histogram
zapisz_wykres("02_histogram_good_hours.png", {
  hist(
    dane_dzienne$good_hours,
    breaks = seq(-0.5, max(dane_dzienne$good_hours) + 0.5, by = 1),
    main = "Histogram liczby dobrych godzin",
    xlab = "Liczba dobrych godzin w dniu",
    ylab = "Liczba dni"
  )
})

# ------------------------------------------------------------
# 5. Wykresy zależności, zgodnie ze schematem z notatnika
# ------------------------------------------------------------

# Prosty wykres liczby dobrych godzin względem średniej prędkości wiatru
zapisz_wykres("03_good_hours_vs_mean_wind_10m.png", {
  plot(
    good_hours ~ mean_wind_10m,
    data = dane_dzienne,
    main = "Liczba dobrych godzin a średnia prędkość wiatru 10 m",
    xlab = "Średnia dzienna prędkość wiatru 10 m",
    ylab = "Liczba dobrych godzin"
  )
  lines(lowess(dane_dzienne$mean_wind_10m, dane_dzienne$good_hours), lwd = 2)
})

# Wersja logarytmiczna, analogiczna do log(ofp + 0.5)
zapisz_wykres("04_log_good_hours_mean_wind_10m.png", {
  plot(
    log(good_hours + 0.5) ~ cut_kwartyle(mean_wind_10m),
    data = dane_dzienne,
    main = "log(good_hours + 0.5) a średnia prędkość wiatru",
    xlab = "Średnia dzienna prędkość wiatru 10 m - kwartyle",
    ylab = "log(good_hours + 0.5)"
  )
})

zapisz_wykres("05_log_good_hours_max_gusts.png", {
  plot(
    log(good_hours + 0.5) ~ cut_kwartyle(max_gusts_10m),
    data = dane_dzienne,
    main = "log(good_hours + 0.5) a maksymalne porywy",
    xlab = "Maksymalne porywy - kwartyle",
    ylab = "log(good_hours + 0.5)"
  )
})

zapisz_wykres("06_log_good_hours_day_hours.png", {
  plot(
    log(good_hours + 0.5) ~ cut_kwartyle(day_hours),
    data = dane_dzienne,
    main = "log(good_hours + 0.5) a liczba godzin dziennych",
    xlab = "Liczba godzin dziennych - kwartyle",
    ylab = "log(good_hours + 0.5)"
  )
})

# Wykres względem zmiennej jakościowej - miesiąc
zapisz_wykres("07_log_good_hours_month.png", {
  plot(
    log(good_hours + 0.5) ~ miesiac_factor,
    data = dane_dzienne,
    main = "log(good_hours + 0.5) w zależności od miesiąca",
    xlab = "Miesiąc",
    ylab = "log(good_hours + 0.5)"
  )
})

# Wykres względem zmiennej jakościowej - sezon
zapisz_wykres("08_log_good_hours_season.png", {
  plot(
    log(good_hours + 0.5) ~ sezon,
    data = dane_dzienne,
    main = "log(good_hours + 0.5) w zależności od sezonu",
    xlab = "Sezon",
    ylab = "log(good_hours + 0.5)"
  )
})

# Spinogram - odpowiednik wykresu z notatnika
dane_dzienne$good_hours_cat <- cut(
  dane_dzienne$good_hours,
  breaks = c(-0.1, 0, 2, 4, 8, 12, 24),
  labels = c("0", "1-2", "3-4", "5-8", "9-12", "13+")
)

zapisz_wykres("09_spinogram_good_hours_season.png", {
  spineplot(
    good_hours_cat ~ sezon,
    data = dane_dzienne,
    main = "Spinogram: liczba dobrych godzin względem sezonu",
    xlab = "Sezon",
    ylab = "Kategoria liczby dobrych godzin"
  )
})

zapisz_wykres("10_spinogram_good_hours_month.png", {
  spineplot(
    good_hours_cat ~ miesiac_factor,
    data = dane_dzienne,
    main = "Spinogram: liczba dobrych godzin względem miesiąca",
    xlab = "Miesiąc",
    ylab = "Kategoria liczby dobrych godzin"
  )
})

# ------------------------------------------------------------
# 6. Pełny model regresji Poissonowskiej
# ------------------------------------------------------------

formula_full <- good_hours ~ mean_wind_10m + max_wind_10m +
  mean_gusts_10m + max_gusts_10m +
  mean_wind_100m + mean_temp + mean_humidity + mean_pressure +
  sum_precipitation + mean_cloud_cover + sum_shortwave + day_hours +
  sin_dzien + cos_dzien

pois_fit <- glm(
  formula_full,
  data = dane_dzienne,
  family = poisson
)

summary(pois_fit)

capture.output(
  summary(pois_fit),
  file = file.path(results_dir, "summary_poisson.txt")
)

# Tabela współczynników i ilorazów intensywności
pois_summary <- coef(summary(pois_fit))

pois_coef_table <- data.frame(
  zmienna = rownames(pois_summary),
  estimate = pois_summary[, "Estimate"],
  std_error = pois_summary[, "Std. Error"],
  z_value = pois_summary[, "z value"],
  p_value = pois_summary[, "Pr(>|z|)"],
  IRR = exp(pois_summary[, "Estimate"])
)

pois_coef_table

write.csv(
  pois_coef_table,
  file.path(results_dir, "poisson_wspolczynniki_IRR.csv"),
  row.names = FALSE
)

# AIC i log-likelihood
AIC(pois_fit)
logLik(pois_fit)

# ------------------------------------------------------------
# 7. Nadmierna dyspersja i zera w modelu Poissona
# ------------------------------------------------------------

srednia_y <- mean(dane_dzienne$good_hours)
wariancja_y <- var(dane_dzienne$good_hours)

disp_deviance <- deviance(pois_fit) / df.residual(pois_fit)
disp_pearson <- sum(residuals(pois_fit, type = "pearson")^2) / df.residual(pois_fit)
p_value_overdispersion <- pchisq(
  sum(residuals(pois_fit, type = "pearson")^2),
  df = df.residual(pois_fit),
  lower.tail = FALSE
)

overdispersion_table <- data.frame(
  srednia_good_hours = srednia_y,
  wariancja_good_hours = wariancja_y,
  wariancja_do_sredniej = wariancja_y / srednia_y,
  dyspersja_deviance = disp_deviance,
  dyspersja_pearson = disp_pearson,
  p_value_testu_pearsona = p_value_overdispersion
)

overdispersion_table

write.csv(
  overdispersion_table,
  file.path(results_dir, "nadmierna_dyspersja.csv"),
  row.names = FALSE
)

# Predykcje Poissona
pois_pred <- predict(pois_fit, type = "response")

# Prawdopodobieństwo zera w klasycznym Poissonie
pois_prob_zero <- dpois(0, lambda = pois_pred)

zero_poisson_table <- data.frame(
  rzeczywista_liczba_zer = sum(dane_dzienne$good_hours == 0),
  rzeczywisty_udzial_zer = mean(dane_dzienne$good_hours == 0),
  oczekiwana_liczba_zer_poisson = sum(pois_prob_zero),
  srednie_prawdopodobienstwo_zera_poisson = mean(pois_prob_zero)
)

zero_poisson_table

write.csv(
  zero_poisson_table,
  file.path(results_dir, "zera_poisson.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 8. Pełny model ZIP
# ------------------------------------------------------------

# Pełny ZIP: ten sam zestaw predyktorów w części count i zero
zip_full_fit <- zeroinfl(
  good_hours ~ mean_wind_10m + max_wind_10m +
    mean_gusts_10m + max_gusts_10m +
    mean_wind_100m + mean_temp + mean_humidity + mean_pressure +
    sum_precipitation + mean_cloud_cover + sum_shortwave + day_hours +
    sin_dzien + cos_dzien |
    mean_wind_10m + max_wind_10m +
    mean_gusts_10m + max_gusts_10m +
    mean_wind_100m + mean_temp + mean_humidity + mean_pressure +
    sum_precipitation + mean_cloud_cover + sum_shortwave + day_hours +
    sin_dzien + cos_dzien,
  data = dane_dzienne,
  dist = "poisson"
)

summary(zip_full_fit)

capture.output(
  summary(zip_full_fit),
  file = file.path(results_dir, "summary_ZIP_full.txt")
)

AIC(zip_full_fit)
logLik(zip_full_fit)

# ------------------------------------------------------------
# 9. Model ZIP zredukowany
# ------------------------------------------------------------

zip_reduced_fit <- zeroinfl(
  good_hours ~ mean_wind_10m + max_gusts_10m + mean_pressure +
    sum_precipitation + mean_cloud_cover + day_hours +
    sin_dzien + cos_dzien |
    mean_wind_10m + max_gusts_10m + sum_precipitation +
    day_hours + cos_dzien,
  data = dane_dzienne,
  dist = "poisson"
)

summary(zip_reduced_fit)

capture.output(
  summary(zip_reduced_fit),
  file = file.path(results_dir, "summary_ZIP_reduced.txt")
)

AIC(zip_reduced_fit)
logLik(zip_reduced_fit)

# Tabele współczynników ZIP
zip_count_summary <- coef(summary(zip_reduced_fit))$count
zip_zero_summary <- coef(summary(zip_reduced_fit))$zero

zip_count_table <- data.frame(
  czesc_modelu = "count",
  zmienna = rownames(zip_count_summary),
  estimate = zip_count_summary[, "Estimate"],
  std_error = zip_count_summary[, "Std. Error"],
  z_value = zip_count_summary[, "z value"],
  p_value = zip_count_summary[, "Pr(>|z|)"],
  IRR = exp(zip_count_summary[, "Estimate"])
)

zip_zero_table <- data.frame(
  czesc_modelu = "zero",
  zmienna = rownames(zip_zero_summary),
  estimate = zip_zero_summary[, "Estimate"],
  std_error = zip_zero_summary[, "Std. Error"],
  z_value = zip_zero_summary[, "z value"],
  p_value = zip_zero_summary[, "Pr(>|z|)"],
  OR = exp(zip_zero_summary[, "Estimate"])
)

zip_count_table
zip_zero_table

write.csv(
  zip_count_table,
  file.path(results_dir, "ZIP_reduced_count_IRR.csv"),
  row.names = FALSE
)

write.csv(
  zip_zero_table,
  file.path(results_dir, "ZIP_reduced_zero_OR.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 10. Porównanie modeli
# ------------------------------------------------------------

zip_full_pred <- predict(zip_full_fit, type = "response")
zip_reduced_pred <- predict(zip_reduced_fit, type = "response")

# Prawdopodobieństwo zera w ZIP:
# type = "zero" daje część strukturalną,
# type = "prob" pozwala odczytać całkowite P(Y = 0).
zip_full_prob_matrix <- predict(zip_full_fit, type = "prob")
zip_reduced_prob_matrix <- predict(zip_reduced_fit, type = "prob")

zip_full_total_prob_zero <- zip_full_prob_matrix[, 1]
zip_reduced_total_prob_zero <- zip_reduced_prob_matrix[, 1]

zip_reduced_structural_zero <- predict(zip_reduced_fit, type = "zero")

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
  ),
  
  srednia_predykcja = c(
    mean(pois_pred),
    mean(zip_full_pred),
    mean(zip_reduced_pred)
  ),
  
  RMSE = c(
    rmse(dane_dzienne$good_hours, pois_pred),
    rmse(dane_dzienne$good_hours, zip_full_pred),
    rmse(dane_dzienne$good_hours, zip_reduced_pred)
  ),
  
  MAE = c(
    mae(dane_dzienne$good_hours, pois_pred),
    mae(dane_dzienne$good_hours, zip_full_pred),
    mae(dane_dzienne$good_hours, zip_reduced_pred)
  ),
  
  przewidywana_liczba_zer = c(
    sum(pois_prob_zero),
    sum(zip_full_total_prob_zero),
    sum(zip_reduced_total_prob_zero)
  ),
  
  srednie_prawdopodobienstwo_zera = c(
    mean(pois_prob_zero),
    mean(zip_full_total_prob_zero),
    mean(zip_reduced_total_prob_zero)
  )
)

porownanie_modeli

write.csv(
  porownanie_modeli,
  file.path(results_dir, "porownanie_modeli_poisson_ZIP.csv"),
  row.names = FALSE
)

# Dodatkowa tabela: rzeczywista średnia i rzeczywiste zera
podsumowanie_predykcji <- data.frame(
  rzeczywista_srednia_good_hours = mean(dane_dzienne$good_hours),
  rzeczywista_liczba_zer = sum(dane_dzienne$good_hours == 0),
  rzeczywisty_udzial_zer = mean(dane_dzienne$good_hours == 0),
  srednia_pred_poisson = mean(pois_pred),
  srednia_pred_ZIP_full = mean(zip_full_pred),
  srednia_pred_ZIP_reduced = mean(zip_reduced_pred),
  srednie_p_zero_poisson = mean(pois_prob_zero),
  srednie_p_zero_ZIP_full = mean(zip_full_total_prob_zero),
  srednie_p_zero_ZIP_reduced = mean(zip_reduced_total_prob_zero),
  srednie_p_zero_strukturalne_ZIP_reduced = mean(zip_reduced_structural_zero)
)

podsumowanie_predykcji

write.csv(
  podsumowanie_predykcji,
  file.path(results_dir, "podsumowanie_predykcji.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 11. Wykresy diagnostyczne i predykcyjne
# ------------------------------------------------------------

# Rzeczywiste vs przewidywane - Poisson
zapisz_wykres("11_rzeczywiste_vs_pred_poisson.png", {
  plot(
    dane_dzienne$good_hours,
    pois_pred,
    main = "Poisson: wartości rzeczywiste vs przewidywane",
    xlab = "Rzeczywista liczba dobrych godzin",
    ylab = "Przewidywana liczba dobrych godzin"
  )
  abline(0, 1, lwd = 2)
})

# Rzeczywiste vs przewidywane - ZIP reduced
zapisz_wykres("12_rzeczywiste_vs_pred_ZIP_reduced.png", {
  plot(
    dane_dzienne$good_hours,
    zip_reduced_pred,
    main = "ZIP reduced: wartości rzeczywiste vs przewidywane",
    xlab = "Rzeczywista liczba dobrych godzin",
    ylab = "Przewidywana liczba dobrych godzin"
  )
  abline(0, 1, lwd = 2)
})

# Szereg czasowy - rzeczywiste i predykcje
zapisz_wykres("13_szereg_czasowy_predykcji.png", {
  plot(
    dane_dzienne$data,
    dane_dzienne$good_hours,
    type = "l",
    main = "Rzeczywista i przewidywana liczba dobrych godzin",
    xlab = "Data",
    ylab = "Liczba dobrych godzin"
  )
  lines(dane_dzienne$data, pois_pred, lty = 2, lwd = 2)
  lines(dane_dzienne$data, zip_reduced_pred, lty = 3, lwd = 2)
  legend(
    "topright",
    legend = c("Rzeczywiste", "Poisson", "ZIP reduced"),
    lty = c(1, 2, 3),
    lwd = c(1, 2, 2),
    bty = "n"
  )
})

# Reszty Pearsona modelu Poissona
zapisz_wykres("14_reszty_poisson.png", {
  plot(
    pois_pred,
    residuals(pois_fit, type = "pearson"),
    main = "Poisson: reszty Pearsona względem wartości dopasowanych",
    xlab = "Wartości dopasowane",
    ylab = "Reszty Pearsona"
  )
  abline(h = 0, lwd = 2)
})

# Q-Q plot reszt Poissona
zapisz_wykres("15_qqplot_reszt_poisson.png", {
  qqnorm(
    residuals(pois_fit, type = "pearson"),
    main = "Poisson: wykres Q-Q reszt Pearsona"
  )
  qqline(residuals(pois_fit, type = "pearson"), lwd = 2)
})

# Prawdopodobieństwo zera strukturalnego w ZIP
zapisz_wykres("16_ZIP_prawdopodobienstwo_zera_strukturalnego.png", {
  plot(
    dane_dzienne$data,
    zip_reduced_structural_zero,
    type = "l",
    main = "ZIP reduced: prawdopodobieństwo zera strukturalnego",
    xlab = "Data",
    ylab = "Prawdopodobieństwo zera strukturalnego"
  )
})

# ------------------------------------------------------------
# 12. Porównanie rozkładów obserwowanych i przewidywanych
# ------------------------------------------------------------

max_y <- max(dane_dzienne$good_hours)

obs_counts <- as.numeric(table(factor(
  dane_dzienne$good_hours,
  levels = 0:max_y
)))

expected_pois_counts <- sapply(
  0:max_y,
  function(k) sum(dpois(k, lambda = pois_pred))
)

# Dla ZIP reduced: prawdopodobieństwa dla kolejnych wartości
zip_prob_matrix <- predict(zip_reduced_fit, type = "prob")

expected_zip_counts <- numeric(length = max_y + 1)

for (k in 0:max_y) {
  kolumna <- as.character(k)
  if (kolumna %in% colnames(zip_prob_matrix)) {
    expected_zip_counts[k + 1] <- sum(zip_prob_matrix[, kolumna])
  } else {
    expected_zip_counts[k + 1] <- 0
  }
}

rozklad_obs_pred <- data.frame(
  good_hours = 0:max_y,
  obserwowane = obs_counts,
  poisson_oczekiwane = expected_pois_counts,
  ZIP_reduced_oczekiwane = expected_zip_counts
)

rozklad_obs_pred

write.csv(
  rozklad_obs_pred,
  file.path(results_dir, "rozklad_obserwowany_i_przewidywany.csv"),
  row.names = FALSE
)

# Wykres rozkładów: obserwowany vs Poisson vs ZIP
zapisz_wykres("17_rozkład_obserwowany_vs_modele.png", {
  barplot(
    t(as.matrix(rozklad_obs_pred[, c("obserwowane", "poisson_oczekiwane", "ZIP_reduced_oczekiwane")])),
    beside = TRUE,
    names.arg = rozklad_obs_pred$good_hours,
    main = "Rozkład obserwowany i przewidywany przez modele",
    xlab = "Liczba dobrych godzin",
    ylab = "Liczba dni",
    legend.text = c("Obserwowane", "Poisson", "ZIP reduced"),
    args.legend = list(x = "topright", bty = "n")
  )
})

# Rootogram uproszczony: obserwowane - oczekiwane na skali pierwiastkowej
rootogram_table <- data.frame(
  good_hours = 0:max_y,
  root_diff_poisson = sqrt(obs_counts) - sqrt(expected_pois_counts),
  root_diff_ZIP = sqrt(obs_counts) - sqrt(expected_zip_counts)
)

write.csv(
  rootogram_table,
  file.path(results_dir, "rootogram_roznice.csv"),
  row.names = FALSE
)

zapisz_wykres("18_rootogram_poisson.png", {
  plot(
    rootogram_table$good_hours,
    rootogram_table$root_diff_poisson,
    type = "h",
    lwd = 4,
    main = "Rootogram uproszczony: Poisson",
    xlab = "Liczba dobrych godzin",
    ylab = "sqrt(obserwowane) - sqrt(oczekiwane)"
  )
  abline(h = 0, lwd = 2)
})

zapisz_wykres("19_rootogram_ZIP_reduced.png", {
  plot(
    rootogram_table$good_hours,
    rootogram_table$root_diff_ZIP,
    type = "h",
    lwd = 4,
    main = "Rootogram uproszczony: ZIP reduced",
    xlab = "Liczba dobrych godzin",
    ylab = "sqrt(obserwowane) - sqrt(oczekiwane)"
  )
  abline(h = 0, lwd = 2)
})

# ------------------------------------------------------------
# 13. Ilorazy intensywności i ilorazy szans - podsumowanie
# ------------------------------------------------------------

irr_pois <- exp(coef(pois_fit))
irr_zip_count <- exp(coef(zip_reduced_fit, model = "count"))
or_zip_zero <- exp(coef(zip_reduced_fit, model = "zero"))

irr_pois
irr_zip_count
or_zip_zero

write.csv(
  data.frame(zmienna = names(irr_pois), IRR = as.numeric(irr_pois)),
  file.path(results_dir, "IRR_poisson.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(zmienna = names(irr_zip_count), IRR = as.numeric(irr_zip_count)),
  file.path(results_dir, "IRR_ZIP_count.csv"),
  row.names = FALSE
)

write.csv(
  data.frame(zmienna = names(or_zip_zero), OR = as.numeric(or_zip_zero)),
  file.path(results_dir, "OR_ZIP_zero.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 14. Opcjonalnie: test Vuonga Poisson vs ZIP
# ------------------------------------------------------------

# Test Vuonga bywa używany do porównania modeli niezanurzonych.
# Jeżeli wynik będzie istotny i dodatni, przemawia na korzyść pierwszego modelu.
# Jeżeli istotny i ujemny, przemawia na korzyść drugiego modelu.
# W razie problemów można pominąć w raporcie.

vuong_poisson_zip <- tryCatch(
  vuong(pois_fit, zip_reduced_fit),
  error = function(e) e
)

vuong_poisson_zip

capture.output(
  vuong_poisson_zip,
  file = file.path(results_dir, "vuong_poisson_vs_ZIP_reduced.txt")
)

# ------------------------------------------------------------
# 15. Komunikat końcowy
# ------------------------------------------------------------

cat("\nZakończono analizę Poissona i ZIP.\n")
cat("Wykresy zapisano w folderze:", plot_dir, "\n")
cat("Tabele i summary zapisano w folderze:", results_dir, "\n")