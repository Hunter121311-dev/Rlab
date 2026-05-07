# ============================================================
# REGRESJA PORZĄDKOWA DLA DANYCH METEOROLOGICZNYCH
# POZIOM WARUNKÓW DO KITESURFINGU W NASTĘPNEJ GODZINIE
# ============================================================

# ------------------------------------------------------------
# 0. Pakiety i ustawienia
# ------------------------------------------------------------

library(dplyr)
library(lubridate)
library(ordinal)

plot_dir <- "plots_ordinal"
results_dir <- "wyniki_ordinal"

dir.create(plot_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)

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

rmse <- function(y, y_hat) {
  sqrt(mean((y - y_hat)^2, na.rm = TRUE))
}

mae <- function(y, y_hat) {
  mean(abs(y - y_hat), na.rm = TRUE)
}

# ------------------------------------------------------------
# 1. Wczytanie danych
# ------------------------------------------------------------

dane <- read.csv("/Users/damia/OneDrive/Dokumenty/Rlab/stat/projekt/data/chalupy_hourly_10000_days.csv")

dane$datetime <- as.POSIXct(dane$datetime, format = "%Y-%m-%dT%H:%M")

dane <- dane %>%
  arrange(datetime)

# Zmienne kalendarzowe
dane$data <- as.Date(dane$datetime)
dane$rok <- year(dane$datetime)
dane$miesiac <- month(dane$datetime)
dane$dzien_roku <- yday(dane$datetime)
dane$godzina <- hour(dane$datetime)

# ------------------------------------------------------------
# 2. Zmienne pomocnicze i przesunięcie czasowe
# ------------------------------------------------------------

# Zmienna zależna będzie dotyczyła warunków w następnej godzinie.
# Predyktory pozostają z chwili t, a klasa warunków jest wyznaczana dla t+1.

dane$wind_speed_10m_next <- lead(dane$wind_speed_10m)
dane$wind_gusts_10m_next <- lead(dane$wind_gusts_10m)
dane$is_day_next <- lead(dane$is_day)

# Sezonowość roczna i dobowa
dane$sin_dzien <- sin(2 * pi * dane$dzien_roku / 365)
dane$cos_dzien <- cos(2 * pi * dane$dzien_roku / 365)

dane$sin_godzina <- sin(2 * pi * dane$godzina / 24)
dane$cos_godzina <- cos(2 * pi * dane$godzina / 24)

# Kierunek wiatru jako zmienna cykliczna
dane$wind_dir_sin <- sin(2 * pi * dane$wind_direction_10m / 360)
dane$wind_dir_cos <- cos(2 * pi * dane$wind_direction_10m / 360)

# Grupa sezonowa do porównania modeli, analogicznie do analizy osobno dla grup w notatniku
dane$polrocze <- ifelse(
  dane$miesiac %in% c(4, 5, 6, 7, 8, 9),
  "cieple",
  "chlodne"
)

dane$polrocze <- factor(dane$polrocze, levels = c("chlodne", "cieple"))

# ------------------------------------------------------------
# 3. Definicja uporządkowanej zmiennej zależnej
# ------------------------------------------------------------

# Poziomy warunków:
# Zle         - warunki nieodpowiednie, noc albo parametry wyraźnie poza zakresem
# Umiarkowane - warunki graniczne
# Dobre       - warunki spełniające podstawowe progi kitesurfingowe
# Bardzo_dobre - stabilniejsze i bardziej komfortowe warunki w środku zakresu

dane$warunki_poziom_next <- case_when(
  is.na(dane$wind_speed_10m_next) |
    is.na(dane$wind_gusts_10m_next) |
    is.na(dane$is_day_next) ~ NA_character_,
  
  dane$is_day_next != 1 ~ "Zle",
  
  dane$wind_speed_10m_next >= 16 &
    dane$wind_speed_10m_next <= 30 &
    dane$wind_gusts_10m_next <= 45 &
    dane$wind_gusts_10m_next / pmax(dane$wind_speed_10m_next, 0.1) <= 1.6 ~ "Bardzo_dobre",
  
  dane$wind_speed_10m_next >= 12 &
    dane$wind_speed_10m_next <= 40 &
    dane$wind_gusts_10m_next <= 55 ~ "Dobre",
  
  dane$wind_speed_10m_next >= 8 &
    dane$wind_speed_10m_next <= 45 &
    dane$wind_gusts_10m_next <= 65 ~ "Umiarkowane",
  
  TRUE ~ "Zle"
)

dane$warunki_poziom_next <- factor(
  dane$warunki_poziom_next,
  levels = c("Zle", "Umiarkowane", "Dobre", "Bardzo_dobre"),
  ordered = TRUE
)

# ------------------------------------------------------------
# 4. Przygotowanie danych modelowych
# ------------------------------------------------------------

# Predyktory zbliżone do wcześniejszych rozdziałów, ale bez zmiennych z przyszłości.
# weather_code potraktowano jako predyktor kategoryczny.

dane$weather_code <- as.factor(dane$weather_code)

predyktory_pelne <- c(
  "wind_speed_10m",
  "wind_speed_100m",
  "wind_gusts_10m",
  "wind_dir_sin",
  "wind_dir_cos",
  "temperature_2m",
  "relative_humidity_2m",
  "dew_point_2m",
  "apparent_temperature",
  "precipitation",
  "cloud_cover",
  "pressure_msl",
  "shortwave_radiation",
  "weather_code",
  "is_day",
  "sin_dzien",
  "cos_dzien",
  "sin_godzina",
  "cos_godzina",
  "polrocze"
)

# Zostawienie tylko predyktorów, które faktycznie istnieją w danych
predyktory_pelne <- predyktory_pelne[predyktory_pelne %in% names(dane)]

dane_model <- dane %>%
  select(
    data,
    datetime,
    warunki_poziom_next,
    all_of(predyktory_pelne)
  ) %>%
  filter(complete.cases(.))

# Rozkład klas
rozkład_klas <- as.data.frame(table(dane_model$warunki_poziom_next))
colnames(rozkład_klas) <- c("klasa", "liczba_obserwacji")
rozkład_klas$udzial <- rozkład_klas$liczba_obserwacji / sum(rozkład_klas$liczba_obserwacji)

rozkład_klas

write.csv(
  rozkład_klas,
  file.path(results_dir, "rozkład_klas_regresja_porzadkowa.csv"),
  row.names = FALSE
)

# Podział chronologiczny, zgodny z wcześniejszymi częściami projektu
train_data <- dane_model %>%
  filter(data < as.Date("2025-01-01"))

test_data <- dane_model %>%
  filter(data >= as.Date("2025-01-01"))

# Sprawdzenie rozkładu klas w train/test
rozkład_train <- as.data.frame(table(train_data$warunki_poziom_next))
rozkład_test <- as.data.frame(table(test_data$warunki_poziom_next))

colnames(rozkład_train) <- c("klasa", "train")
colnames(rozkład_test) <- c("klasa", "test")

rozkład_train_test <- merge(rozkład_train, rozkład_test, by = "klasa", all = TRUE)
rozkład_train_test[is.na(rozkład_train_test)] <- 0

rozkład_train_test

write.csv(
  rozkład_train_test,
  file.path(results_dir, "rozkład_klas_train_test.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 5. Wykresy rozkładu zmiennej porządkowej
# ------------------------------------------------------------

zapisz_wykres("01_rozkład_klas_warunki_poziom.png", {
  barplot(
    table(dane_model$warunki_poziom_next),
    main = "Rozkład poziomów warunków do kitesurfingu",
    xlab = "Poziom warunków",
    ylab = "Liczba obserwacji"
  )
})

zapisz_wykres("02_rozkład_klas_train_test.png", {
  barplot(
    rbind(
      table(train_data$warunki_poziom_next),
      table(test_data$warunki_poziom_next)
    ),
    beside = TRUE,
    main = "Rozkład poziomów warunków w zbiorze treningowym i testowym",
    xlab = "Poziom warunków",
    ylab = "Liczba obserwacji",
    legend.text = c("Train", "Test"),
    args.legend = list(x = "topright", bty = "n")
  )
})

# Zależność poziomu warunków od bieżącej prędkości wiatru
dane_model$wind_band <- cut(
  dane_model$wind_speed_10m,
  breaks = c(-Inf, 8, 12, 16, 30, 40, 45, Inf),
  labels = c("<8", "8-12", "12-16", "16-30", "30-40", "40-45", ">45"),
  include.lowest = TRUE
)

zapisz_wykres("03_spinogram_warunki_vs_wind_band.png", {
  spineplot(
    warunki_poziom_next ~ wind_band,
    data = dane_model,
    main = "Poziom warunków za godzinę a bieżąca prędkość wiatru",
    xlab = "Bieżąca prędkość wiatru 10 m",
    ylab = "Poziom warunków za godzinę"
  )
})

# Zależność poziomu warunków od bieżących porywów
dane_model$gust_band <- cut(
  dane_model$wind_gusts_10m,
  breaks = c(-Inf, 30, 45, 55, 65, Inf),
  labels = c("<30", "30-45", "45-55", "55-65", ">65"),
  include.lowest = TRUE
)

zapisz_wykres("04_spinogram_warunki_vs_gust_band.png", {
  spineplot(
    warunki_poziom_next ~ gust_band,
    data = dane_model,
    main = "Poziom warunków za godzinę a bieżące porywy",
    xlab = "Bieżące porywy wiatru",
    ylab = "Poziom warunków za godzinę"
  )
})

# Zależność od półrocza
zapisz_wykres("05_spinogram_warunki_vs_polrocze.png", {
  spineplot(
    warunki_poziom_next ~ polrocze,
    data = dane_model,
    main = "Poziom warunków za godzinę względem półrocza",
    xlab = "Półrocze",
    ylab = "Poziom warunków za godzinę"
  )
})

# ------------------------------------------------------------
# 6. Pełny model regresji porządkowej bez skalowania
# ------------------------------------------------------------

formula_full <- as.formula(
  paste("warunki_poziom_next ~", paste(predyktory_pelne, collapse = " + "))
)

ordinal_full_unscaled_fit <- clm(
  formula_full,
  data = train_data,
  link = "logit",
  Hess = TRUE
)

summary(ordinal_full_unscaled_fit)

capture.output(
  summary(ordinal_full_unscaled_fit),
  file = file.path(results_dir, "summary_ordinal_full_unscaled.txt")
)

cond_full_unscaled <- ordinal_full_unscaled_fit$cond.H
cond_full_unscaled

# ------------------------------------------------------------
# 7. Skalowanie predyktorów numerycznych
# ------------------------------------------------------------

train_scaled <- train_data
test_scaled <- test_data

# Skalujemy tylko predyktory numeryczne.
# Nie skalujemy zmiennej zależnej, daty, datetime ani czynników.

zmienne_numeryczne <- names(train_scaled)[sapply(train_scaled, is.numeric)]
zmienne_numeryczne <- setdiff(
  zmienne_numeryczne,
  c("data", "datetime")
)

# Skalowanie według parametrów ze zbioru treningowego
scaling_params <- data.frame(
  zmienna = character(),
  srednia = numeric(),
  odchylenie = numeric()
)

for (zm in zmienne_numeryczne) {
  srednia <- mean(train_scaled[[zm]], na.rm = TRUE)
  odchylenie <- sd(train_scaled[[zm]], na.rm = TRUE)
  
  if (is.na(odchylenie) || odchylenie == 0) {
    odchylenie <- 1
  }
  
  train_scaled[[zm]] <- (train_scaled[[zm]] - srednia) / odchylenie
  test_scaled[[zm]] <- (test_scaled[[zm]] - srednia) / odchylenie
  
  scaling_params <- rbind(
    scaling_params,
    data.frame(
      zmienna = zm,
      srednia = srednia,
      odchylenie = odchylenie
    )
  )
}

write.csv(
  scaling_params,
  file.path(results_dir, "parametry_skalowania.csv"),
  row.names = FALSE
)

summary(train_scaled)

capture.output(
  summary(train_scaled),
  file = file.path(results_dir, "summary_train_scaled.txt")
)

# ------------------------------------------------------------
# 8. Pełny model regresji porządkowej po skalowaniu
# ------------------------------------------------------------

ordinal_full_scaled_fit <- clm(
  formula_full,
  data = train_scaled,
  link = "logit",
  Hess = TRUE
)

summary(ordinal_full_scaled_fit)

capture.output(
  summary(ordinal_full_scaled_fit),
  file = file.path(results_dir, "summary_ordinal_full_scaled.txt")
)

cond_full_scaled <- ordinal_full_scaled_fit$cond.H
cond_full_scaled

# Tabela współczynników pełnego modelu
full_scaled_summary <- coef(summary(ordinal_full_scaled_fit))

full_scaled_coef_table <- data.frame(
  parametr = rownames(full_scaled_summary),
  estimate = full_scaled_summary[, "Estimate"],
  std_error = full_scaled_summary[, "Std. Error"],
  z_value = full_scaled_summary[, "z value"],
  p_value = full_scaled_summary[, "Pr(>|z|)"]
)

write.csv(
  full_scaled_coef_table,
  file.path(results_dir, "ordinal_full_scaled_wspolczynniki.csv"),
  row.names = FALSE
)

# Ilorazy szans dla predyktorów lokalizacyjnych
or_full_scaled <- data.frame(
  predyktor = names(ordinal_full_scaled_fit$beta),
  OR = exp(as.numeric(ordinal_full_scaled_fit$beta))
)

or_full_scaled

write.csv(
  or_full_scaled,
  file.path(results_dir, "ordinal_full_scaled_OR.csv"),
  row.names = FALSE
)

# Testy pomocnicze z pakietu ordinal
# nominal_test i scale_test pomagają sprawdzić, czy model nie wymaga
# dodatkowych efektów nominalnych lub skali.

nominal_test_full <- tryCatch(
  nominal_test(ordinal_full_scaled_fit),
  error = function(e) e
)

scale_test_full <- tryCatch(
  scale_test(ordinal_full_scaled_fit),
  error = function(e) e
)

nominal_test_full
scale_test_full

capture.output(
  nominal_test_full,
  file = file.path(results_dir, "nominal_test_full_scaled.txt")
)

capture.output(
  scale_test_full,
  file = file.path(results_dir, "scale_test_full_scaled.txt")
)

# ------------------------------------------------------------
# 9. Model zredukowany
# ------------------------------------------------------------

# Model zredukowany opiera się na najważniejszych praktycznie predyktorach.
# Po otrzymaniu wyników można go dodatkowo dostosować do p-value i AIC.

predyktory_reduced <- c(
  "wind_speed_10m",
  "wind_speed_100m",
  "wind_gusts_10m",
  "wind_dir_sin",
  "wind_dir_cos",
  "pressure_msl",
  "precipitation",
  "cloud_cover",
  "shortwave_radiation",
  "is_day",
  "sin_dzien",
  "cos_dzien",
  "sin_godzina",
  "cos_godzina",
  "polrocze"
)

predyktory_reduced <- predyktory_reduced[predyktory_reduced %in% names(train_scaled)]

formula_reduced <- as.formula(
  paste("warunki_poziom_next ~", paste(predyktory_reduced, collapse = " + "))
)

ordinal_reduced_fit <- clm(
  formula_reduced,
  data = train_scaled,
  link = "logit",
  Hess = TRUE
)

summary(ordinal_reduced_fit)

capture.output(
  summary(ordinal_reduced_fit),
  file = file.path(results_dir, "summary_ordinal_reduced.txt")
)

cond_reduced <- ordinal_reduced_fit$cond.H
cond_reduced

reduced_summary <- coef(summary(ordinal_reduced_fit))

reduced_coef_table <- data.frame(
  parametr = rownames(reduced_summary),
  estimate = reduced_summary[, "Estimate"],
  std_error = reduced_summary[, "Std. Error"],
  z_value = reduced_summary[, "z value"],
  p_value = reduced_summary[, "Pr(>|z|)"]
)

write.csv(
  reduced_coef_table,
  file.path(results_dir, "ordinal_reduced_wspolczynniki.csv"),
  row.names = FALSE
)

or_reduced <- data.frame(
  predyktor = names(ordinal_reduced_fit$beta),
  OR = exp(as.numeric(ordinal_reduced_fit$beta))
)

or_reduced

write.csv(
  or_reduced,
  file.path(results_dir, "ordinal_reduced_OR.csv"),
  row.names = FALSE
)

# Analiza usuwania predyktorów
drop1_reduced <- tryCatch(
  drop1(ordinal_reduced_fit, test = "Chisq"),
  error = function(e) e
)

drop1_reduced

capture.output(
  drop1_reduced,
  file = file.path(results_dir, "drop1_ordinal_reduced.txt")
)

# ------------------------------------------------------------
# 10. Predykcje i ocena modeli
# ------------------------------------------------------------

pobierz_pred_klase <- function(model, newdata, poziomy) {
  pred <- predict(model, newdata = newdata, type = "class")$fit
  factor(pred, levels = poziomy, ordered = TRUE)
}

ocen_model <- function(model, train_data, test_data, nazwa_modelu) {
  poziomy <- levels(train_data$warunki_poziom_next)
  
  pred_train <- pobierz_pred_klase(model, train_data, poziomy)
  pred_test <- pobierz_pred_klase(model, test_data, poziomy)
  
  y_train <- train_data$warunki_poziom_next
  y_test <- test_data$warunki_poziom_next
  
  conf_train <- table(
    Przewidywane = pred_train,
    Rzeczywiste = y_train
  )
  
  conf_test <- table(
    Przewidywane = pred_test,
    Rzeczywiste = y_test
  )
  
  accuracy_train <- mean(pred_train == y_train)
  accuracy_test <- mean(pred_test == y_test)
  
  error_train <- 1 - accuracy_train
  error_test <- 1 - accuracy_test
  
  y_train_num <- as.numeric(y_train)
  y_test_num <- as.numeric(y_test)
  
  pred_train_num <- as.numeric(pred_train)
  pred_test_num <- as.numeric(pred_test)
  
  mae_train <- mae(y_train_num, pred_train_num)
  mae_test <- mae(y_test_num, pred_test_num)
  
  rmse_train <- rmse(y_train_num, pred_train_num)
  rmse_test <- rmse(y_test_num, pred_test_num)
  
  within_one_train <- mean(abs(y_train_num - pred_train_num) <= 1)
  within_one_test <- mean(abs(y_test_num - pred_test_num) <= 1)
  
  metryki <- data.frame(
    model = nazwa_modelu,
    logLik = as.numeric(logLik(model)),
    AIC = AIC(model),
    cond_H = model$cond.H,
    accuracy_train = accuracy_train,
    error_train = error_train,
    accuracy_test = accuracy_test,
    error_test = error_test,
    MAE_ordinal_train = mae_train,
    MAE_ordinal_test = mae_test,
    RMSE_ordinal_train = rmse_train,
    RMSE_ordinal_test = rmse_test,
    within_one_category_train = within_one_train,
    within_one_category_test = within_one_test
  )
  
  list(
    pred_train = pred_train,
    pred_test = pred_test,
    conf_train = conf_train,
    conf_test = conf_test,
    metryki = metryki
  )
}

ocena_full <- ocen_model(
  ordinal_full_scaled_fit,
  train_scaled,
  test_scaled,
  "Ordinal full scaled"
)

ocena_reduced <- ocen_model(
  ordinal_reduced_fit,
  train_scaled,
  test_scaled,
  "Ordinal reduced"
)

ocena_full$conf_test
ocena_reduced$conf_test

porownanie_modeli_ordinal <- rbind(
  ocena_full$metryki,
  ocena_reduced$metryki
)

porownanie_modeli_ordinal

write.csv(
  porownanie_modeli_ordinal,
  file.path(results_dir, "porownanie_modeli_ordinal.csv"),
  row.names = FALSE
)

write.csv(
  as.data.frame.matrix(ocena_full$conf_test),
  file.path(results_dir, "confusion_test_ordinal_full.csv")
)

write.csv(
  as.data.frame.matrix(ocena_reduced$conf_test),
  file.path(results_dir, "confusion_test_ordinal_reduced.csv")
)

# Wykresy tablic pomyłek
zapisz_wykres("06_confusion_test_ordinal_full.png", {
  mosaicplot(
    ocena_full$conf_test,
    main = "Regresja porządkowa pełna: tablica pomyłek na zbiorze testowym",
    xlab = "Przewidywane",
    ylab = "Rzeczywiste"
  )
})

zapisz_wykres("07_confusion_test_ordinal_reduced.png", {
  mosaicplot(
    ocena_reduced$conf_test,
    main = "Regresja porządkowa zredukowana: tablica pomyłek na zbiorze testowym",
    xlab = "Przewidywane",
    ylab = "Rzeczywiste"
  )
})

# ------------------------------------------------------------
# 11. Prawdopodobieństwa klas dla przykładowej obserwacji
# ------------------------------------------------------------

# W niektórych przypadkach predict(..., type = "prob") dla pojedynczego
# wiersza może powodować błąd wymiarów. Dlatego prawdopodobieństwa
# wyznaczamy ręcznie na podstawie progów i współczynników modelu clm.

dopasuj_poziomy_czynnikow <- function(newdata, refdata) {
  newdata <- as.data.frame(newdata)
  refdata <- as.data.frame(refdata)
  
  for (zm in names(refdata)) {
    if (zm %in% names(newdata) && is.factor(refdata[[zm]])) {
      newdata[[zm]] <- factor(
        newdata[[zm]],
        levels = levels(refdata[[zm]]),
        ordered = is.ordered(refdata[[zm]])
      )
    }
  }
  
  newdata
}

pred_prob_clm_manual <- function(model, newdata, refdata) {
  newdata <- dopasuj_poziomy_czynnikow(newdata, refdata)
  
  # Macierz modelu dla predyktorów
  X <- model.matrix(
    delete.response(terms(model)),
    data = newdata
  )
  
  # Usunięcie interceptu, jeżeli występuje w macierzy modelu
  if ("(Intercept)" %in% colnames(X)) {
    X <- X[, colnames(X) != "(Intercept)", drop = FALSE]
  }
  
  # Dopasowanie kolumn do współczynników modelu
  brakujace <- setdiff(names(model$beta), colnames(X))
  
  if (length(brakujace) > 0) {
    for (zm in brakujace) {
      X <- cbind(X, 0)
      colnames(X)[ncol(X)] <- zm
    }
  }
  
  X <- X[, names(model$beta), drop = FALSE]
  
  eta <- as.numeric(X %*% model$beta)
  
  progi <- model$alpha
  
  # Dla modelu proporcjonalnych szans:
  # P(Y <= j) = logistic(theta_j - eta)
  cum_prob <- plogis(outer(progi, eta, "-"))
  
  # Zamiana prawdopodobieństw skumulowanych na prawdopodobieństwa klas
  prob_matrix <- apply(cum_prob, 2, function(cp) {
    c(
      cp[1],
      diff(cp),
      1 - cp[length(cp)]
    )
  })
  
  prob_matrix <- t(prob_matrix)
  
  colnames(prob_matrix) <- levels(refdata$warunki_poziom_next)
  
  as.data.frame(prob_matrix)
}

# Prawdopodobieństwa dla 10. obserwacji testowej
prob_full_10 <- pred_prob_clm_manual(
  model = ordinal_full_scaled_fit,
  newdata = test_scaled[10, ],
  refdata = train_scaled
)

prob_reduced_10 <- pred_prob_clm_manual(
  model = ordinal_reduced_fit,
  newdata = test_scaled[10, ],
  refdata = train_scaled
)

prob_10_table <- data.frame(
  klasa = colnames(prob_full_10),
  p_full = as.numeric(prob_full_10[1, ]),
  p_reduced = as.numeric(prob_reduced_10[1, ])
)

prob_10_table

write.csv(
  prob_10_table,
  file.path(results_dir, "prawdopodobienstwa_10_obserwacji.csv"),
  row.names = FALSE
)

zapisz_wykres("09_prawdopodobienstwa_10_obserwacji.png", {
  barplot(
    rbind(prob_10_table$p_full, prob_10_table$p_reduced),
    beside = TRUE,
    names.arg = prob_10_table$klasa,
    main = "Prawdopodobieństwa klas dla 10. obserwacji testowej",
    xlab = "Klasa",
    ylab = "Prawdopodobieństwo",
    legend.text = c("Model pełny", "Model zredukowany"),
    args.legend = list(x = "topright", bty = "n")
  )
})

# ------------------------------------------------------------
# 12. Porównanie modeli osobno dla półrocza chłodnego i ciepłego
# ------------------------------------------------------------

# Odpowiednik zadania z notatnika, gdzie analizę wykonuje się osobno dla grup.
# Tutaj grupami są półrocze chłodne i ciepłe.

dopasuj_model_dla_grupy <- function(grupa_nazwa) {
  dane_grupa <- train_scaled %>%
    filter(polrocze == grupa_nazwa) %>%
    droplevels()
  
  pred_grupa <- setdiff(predyktory_reduced, "polrocze")
  
  formula_grupa <- as.formula(
    paste("warunki_poziom_next ~", paste(pred_grupa, collapse = " + "))
  )
  
  if (length(unique(dane_grupa$warunki_poziom_next)) < 2) {
    return(NULL)
  }
  
  model <- clm(
    formula_grupa,
    data = dane_grupa,
    link = "logit",
    Hess = TRUE
  )
  
  model
}

ordinal_chlodne_fit <- dopasuj_model_dla_grupy("chlodne")
ordinal_cieple_fit <- dopasuj_model_dla_grupy("cieple")

summary(ordinal_chlodne_fit)
summary(ordinal_cieple_fit)

capture.output(
  summary(ordinal_chlodne_fit),
  file = file.path(results_dir, "summary_ordinal_chlodne.txt")
)

capture.output(
  summary(ordinal_cieple_fit),
  file = file.path(results_dir, "summary_ordinal_cieple.txt")
)

porownanie_grup <- data.frame(
  grupa = c("chlodne", "cieple"),
  
  n_train = c(
    nrow(train_scaled %>% filter(polrocze == "chlodne")),
    nrow(train_scaled %>% filter(polrocze == "cieple"))
  ),
  
  logLik = c(
    as.numeric(logLik(ordinal_chlodne_fit)),
    as.numeric(logLik(ordinal_cieple_fit))
  ),
  
  AIC = c(
    AIC(ordinal_chlodne_fit),
    AIC(ordinal_cieple_fit)
  ),
  
  cond_H = c(
    ordinal_chlodne_fit$cond.H,
    ordinal_cieple_fit$cond.H
  )
)

porownanie_grup

write.csv(
  porownanie_grup,
  file.path(results_dir, "porownanie_polroczy_ordinal.csv"),
  row.names = FALSE
)

or_chlodne <- data.frame(
  grupa = "chlodne",
  predyktor = names(ordinal_chlodne_fit$beta),
  OR = exp(as.numeric(ordinal_chlodne_fit$beta))
)

or_cieple <- data.frame(
  grupa = "cieple",
  predyktor = names(ordinal_cieple_fit$beta),
  OR = exp(as.numeric(ordinal_cieple_fit$beta))
)

or_grupy <- rbind(or_chlodne, or_cieple)

or_grupy

write.csv(
  or_grupy,
  file.path(results_dir, "OR_modele_polrocza.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 13. Dodatkowe wykresy predykcyjne
# ------------------------------------------------------------

zapisz_wykres("08_rzeczywiste_vs_przewidywane_reduced.png", {
  tab <- rbind(
    Rzeczywiste = table(test_scaled$warunki_poziom_next),
    Przewidywane = table(ocena_reduced$pred_test)
  )
  
  barplot(
    tab,
    beside = TRUE,
    main = "Rzeczywiste i przewidywane klasy - model zredukowany",
    xlab = "Poziom warunków",
    ylab = "Liczba obserwacji",
    legend.text = TRUE,
    args.legend = list(x = "topright", bty = "n")
  )
})

# ------------------------------------------------------------
# 14. Komunikat końcowy
# ------------------------------------------------------------

cat("\nZakończono analizę regresji porządkowej.\n")
cat("Wykresy zapisano w folderze:", plot_dir, "\n")
cat("Tabele i summary zapisano w folderze:", results_dir, "\n")

# ------------------------------------------------------------
# NOWE WYKRESY MACIERZY POMYŁEK - ZMIANA STYLU HEATMAPY
# ------------------------------------------------------------

library(ggplot2)
library(dplyr)

rysuj_macierz_pomylek <- function(cm, tytul) {
  cm <- as.matrix(cm)
  n_total <- sum(cm)
  
  df <- as.data.frame(as.table(cm))
  colnames(df) <- c("Prediction", "Target", "n")
  
  df <- df %>%
    mutate(
      procent_total = 100 * n / n_total,
      procent_wiersz = 100 * n / rowSums(cm)[Prediction],
      procent_kolumna = 100 * n / colSums(cm)[Target],
      typ = ifelse(as.character(Prediction) == as.character(Target),
                   "Diagonal", "Off diagonal"),
      etykieta_glowna = paste0(round(procent_total), "%\n", n),
      etykieta_dodatkowa = paste0(
        "w: ", round(procent_wiersz, 1), "%\n",
        "k: ", round(procent_kolumna, 1), "%"
      )
    )
  
  df_row <- data.frame(
    Prediction = rownames(cm),
    Target = "Total",
    n = rowSums(cm)
  ) %>%
    mutate(
      procent_total = 100 * n / n_total,
      procent_wiersz = NA,
      procent_kolumna = NA,
      typ = "Total",
      etykieta_glowna = paste0(round(procent_total), "%\n", n),
      etykieta_dodatkowa = ""
    )
  
  df_col <- data.frame(
    Prediction = "Total",
    Target = colnames(cm),
    n = colSums(cm)
  ) %>%
    mutate(
      procent_total = 100 * n / n_total,
      procent_wiersz = NA,
      procent_kolumna = NA,
      typ = "Total",
      etykieta_glowna = paste0(round(procent_total), "%\n", n),
      etykieta_dodatkowa = ""
    )
  
  df_total <- data.frame(
    Prediction = "Total",
    Target = "Total",
    n = n_total,
    procent_total = 100,
    procent_wiersz = NA,
    procent_kolumna = NA,
    typ = "Grand total",
    etykieta_glowna = paste0("100%\n", n_total),
    etykieta_dodatkowa = ""
  )
  
  df_all <- bind_rows(df, df_row, df_col, df_total)
  
  df_all$Prediction <- factor(
    df_all$Prediction,
    levels = c(rev(rownames(cm)), "Total")
  )
  
  df_all$Target <- factor(
    df_all$Target,
    levels = c(colnames(cm), "Total")
  )
  
  ggplot(df_all, aes(x = Target, y = Prediction, fill = typ)) +
    geom_tile(color = "grey70", linewidth = 0.6) +
    geom_text(aes(label = etykieta_glowna), size = 4.2, lineheight = 0.9) +
    geom_text(
      aes(label = etykieta_dodatkowa),
      size = 2.4,
      nudge_y = -0.28,
      lineheight = 0.85
    ) +
    scale_fill_manual(
      values = c(
        "Diagonal" = "#3C8DBC",
        "Off diagonal" = "#DCEAF6",
        "Total" = "#F6B26B",
        "Grand total" = "#E6E6E6"
      )
    ) +
    scale_x_discrete(position = "top") +
    coord_equal() +
    labs(
      title = tytul,
      x = "Target",
      y = "Prediction"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_text(margin = margin(b = 10)),
      axis.title.y = element_text(margin = margin(r = 10))
    )
}

# Zapis wykresu modelu pełnego
png(
  filename = file.path(plot_dir, "06_confusion_test_ordinal_full.png"),
  width = 1200,
  height = 900,
  res = 140
)
print(
  rysuj_macierz_pomylek(
    ocena_full$conf_test,
    "Regresja porządkowa pełna: macierz pomyłek"
  )
)
dev.off()

# Zapis wykresu modelu zredukowanego
png(
  filename = file.path(plot_dir, "07_confusion_test_ordinal_reduced.png"),
  width = 1200,
  height = 900,
  res = 140
)
print(
  rysuj_macierz_pomylek(
    ocena_reduced$conf_test,
    "Regresja porządkowa zredukowana: macierz pomyłek"
  )
)
dev.off()