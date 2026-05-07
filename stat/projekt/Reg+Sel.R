# ============================================================
# DOBÓR PREDYKTORÓW I REGULARYZACJA MODELI LINIOWYCH
# PREDYKCJA PRĘDKOŚCI WIATRU I PORYWÓW W NASTĘPNEJ GODZINIE
# ============================================================

# ------------------------------------------------------------
# 0. Pakiety i ustawienia
# ------------------------------------------------------------

library(dplyr)
library(lubridate)
library(leaps)
library(glmnet)

plot_dir <- "plots_selection_regularization"
results_dir <- "wyniki_selection_regularization"

dir.create(plot_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)

set.seed(2025)

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

mse <- function(y, y_hat) {
  mean((y - y_hat)^2, na.rm = TRUE)
}

rmse <- function(y, y_hat) {
  sqrt(mse(y, y_hat))
}

mae <- function(y, y_hat) {
  mean(abs(y - y_hat), na.rm = TRUE)
}

r2_test <- function(y, y_hat) {
  1 - sum((y - y_hat)^2, na.rm = TRUE) /
    sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
}

metryki_regresji <- function(y, y_hat) {
  data.frame(
    MSE = mse(y, y_hat),
    RMSE = rmse(y, y_hat),
    MAE = mae(y, y_hat),
    R2 = r2_test(y, y_hat)
  )
}

predict_regsubsets <- function(object, newx, id) {
  coefs <- coef(object, id = id)
  mat <- cbind("(Intercept)" = 1, as.matrix(newx))
  mat[, names(coefs), drop = FALSE] %*% coefs
}

pobierz_zmienne_regsubsets <- function(model, id) {
  zmienne <- names(coef(model, id = id))
  setdiff(zmienne, "(Intercept)")
}

formula_zmiennych <- function(target, zmienne) {
  if (length(zmienne) == 0) {
    as.formula(paste(target, "~ 1"))
  } else {
    as.formula(paste(target, "~", paste(zmienne, collapse = " + ")))
  }
}

zapisz_wspolczynniki_lm <- function(fit, nazwa_pliku) {
  s <- coef(summary(fit))
  
  tab <- data.frame(
    parametr = rownames(s),
    estimate = s[, "Estimate"],
    std_error = s[, "Std. Error"],
    t_value = s[, "t value"],
    p_value = s[, "Pr(>|t|)"]
  )
  
  write.csv(
    tab,
    file.path(results_dir, nazwa_pliku),
    row.names = FALSE
  )
}

pobierz_zmienne_niezerowe <- function(coef_matrix) {
  tab <- data.frame(
    parametr = rownames(coef_matrix),
    coefficient = as.numeric(coef_matrix[, 1])
  )
  
  tab <- tab %>%
    dplyr::filter(parametr != "(Intercept)") %>%
    dplyr::filter(coefficient != 0)
  
  tab$parametr
}

wynik_tabelaryczny <- function(
    target,
    model_name,
    liczba_predyktorow,
    lambda_value,
    zmienne,
    y_train,
    pred_train,
    y_test,
    pred_test
) {
  met_train <- metryki_regresji(y_train, pred_train)
  met_test <- metryki_regresji(y_test, pred_test)
  
  data.frame(
    target = target,
    model = model_name,
    liczba_predyktorow = liczba_predyktorow,
    lambda = lambda_value,
    MSE_train = met_train$MSE,
    RMSE_train = met_train$RMSE,
    MAE_train = met_train$MAE,
    R2_train = met_train$R2,
    MSE_test = met_test$MSE,
    RMSE_test = met_test$RMSE,
    MAE_test = met_test$MAE,
    R2_test = met_test$R2,
    zmienne = ifelse(
      length(zmienne) == 0,
      "brak",
      paste(zmienne, collapse = ", ")
    )
  )
}

dopasuj_lm_dla_podzbioru <- function(
    target,
    zmienne,
    train_data,
    test_data,
    nazwa_modelu
) {
  f <- formula_zmiennych(target, zmienne)
  fit <- lm(f, data = train_data)
  
  pred_train <- predict(fit, newdata = train_data)
  pred_test <- predict(fit, newdata = test_data)
  
  wynik <- wynik_tabelaryczny(
    target = target,
    model_name = nazwa_modelu,
    liczba_predyktorow = length(zmienne),
    lambda_value = "nie dotyczy",
    zmienne = zmienne,
    y_train = train_data[[target]],
    pred_train = pred_train,
    y_test = test_data[[target]],
    pred_test = pred_test
  )
  
  list(
    wynik = wynik,
    fit = fit,
    pred_train = pred_train,
    pred_test = pred_test
  )
}

# ------------------------------------------------------------
# 1. Wczytanie i przygotowanie danych
# ------------------------------------------------------------

dane <- read.csv(
  "/Users/damia/OneDrive/Dokumenty/Rlab/stat/projekt/data/chalupy_hourly_10000_days.csv"
)

dane$datetime <- as.POSIXct(
  dane$datetime,
  format = "%Y-%m-%dT%H:%M"
)

dane <- dane %>%
  dplyr::arrange(datetime)

dane$data <- as.Date(dane$datetime)
dane$rok <- year(dane$datetime)
dane$miesiac <- month(dane$datetime)
dane$dzien_roku <- yday(dane$datetime)
dane$godzina <- hour(dane$datetime)

# Zmienne zależne przesunięte o jedną godzinę.
# Modele przewidują wartość w chwili t + 1 na podstawie predyktorów z chwili t.
dane$wind_speed_10m_next <- lead(dane$wind_speed_10m, 1)
dane$wind_gusts_10m_next <- lead(dane$wind_gusts_10m, 1)

# Sezonowość roczna i dobowa
dane$sin_dzien <- sin(2 * pi * dane$dzien_roku / 365)
dane$cos_dzien <- cos(2 * pi * dane$dzien_roku / 365)

dane$sin_godzina <- sin(2 * pi * dane$godzina / 24)
dane$cos_godzina <- cos(2 * pi * dane$godzina / 24)

# Kierunek wiatru jako zmienna cykliczna
dane$wind_dir_sin <- sin(2 * pi * dane$wind_direction_10m / 360)
dane$wind_dir_cos <- cos(2 * pi * dane$wind_direction_10m / 360)

# Zmienna sezonowa pomocnicza
dane$polrocze_cieple <- ifelse(
  dane$miesiac %in% c(4, 5, 6, 7, 8, 9),
  1,
  0
)

# ------------------------------------------------------------
# 2. Predyktory i zmienne zależne
# ------------------------------------------------------------

predyktory <- c(
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
  "is_day",
  "sin_dzien",
  "cos_dzien",
  "sin_godzina",
  "cos_godzina",
  "polrocze_cieple"
)

predyktory <- predyktory[predyktory %in% names(dane)]

targety <- c(
  "wind_speed_10m_next",
  "wind_gusts_10m_next"
)

# ------------------------------------------------------------
# 3. Korelacje predyktorów
# ------------------------------------------------------------

dane_korelacje <- dane %>%
  dplyr::select(all_of(predyktory)) %>%
  dplyr::filter(complete.cases(.))

macierz_korelacji <- cor(dane_korelacje)

write.csv(
  macierz_korelacji,
  file.path(results_dir, "macierz_korelacji_predyktorow.csv")
)

wysokie_korelacje <- data.frame()

for (i in 1:(ncol(macierz_korelacji) - 1)) {
  for (j in (i + 1):ncol(macierz_korelacji)) {
    wysokie_korelacje <- rbind(
      wysokie_korelacje,
      data.frame(
        zmienna_1 = colnames(macierz_korelacji)[i],
        zmienna_2 = colnames(macierz_korelacji)[j],
        korelacja = macierz_korelacji[i, j],
        abs_korelacja = abs(macierz_korelacji[i, j])
      )
    )
  }
}

wysokie_korelacje <- wysokie_korelacje %>%
  dplyr::arrange(desc(abs_korelacja))

write.csv(
  wysokie_korelacje,
  file.path(results_dir, "korelacje_predyktorow_posortowane.csv"),
  row.names = FALSE
)

zapisz_wykres("00_macierz_korelacji_predyktorow.png", {
  par(mar = c(10, 10, 4, 2))
  
  image(
    1:ncol(macierz_korelacji),
    1:ncol(macierz_korelacji),
    macierz_korelacji,
    axes = FALSE,
    main = "Macierz korelacji predyktorów",
    xlab = "",
    ylab = ""
  )
  
  axis(
    1,
    at = 1:ncol(macierz_korelacji),
    labels = colnames(macierz_korelacji),
    las = 2,
    cex.axis = 0.55
  )
  
  axis(
    2,
    at = 1:ncol(macierz_korelacji),
    labels = colnames(macierz_korelacji),
    las = 2,
    cex.axis = 0.55
  )
})

# ------------------------------------------------------------
# 4. Funkcja wykonująca analizę dla jednej zmiennej zależnej
# ------------------------------------------------------------

analizuj_target <- function(target) {
  
  cat("\n============================================================\n")
  cat("Analiza dla zmiennej:", target, "\n")
  cat("============================================================\n")
  
  suffix <- target
  
  dane_model <- dane %>%
    dplyr::select(
      data,
      datetime,
      all_of(target),
      all_of(predyktory)
    ) %>%
    dplyr::filter(complete.cases(.))
  
  train_data <- dane_model %>%
    dplyr::filter(data < as.Date("2025-01-01"))
  
  test_data <- dane_model %>%
    dplyr::filter(data >= as.Date("2025-01-01"))
  
  X_train_regsubsets <- as.matrix(train_data[, predyktory])
  y_train_regsubsets <- train_data[[target]]
  
  X_test_regsubsets <- as.matrix(test_data[, predyktory])
  y_test_regsubsets <- test_data[[target]]
  
  opis_danych <- data.frame(
    target = target,
    n_total = nrow(dane_model),
    n_train = nrow(train_data),
    n_test = nrow(test_data),
    mean_train = mean(train_data[[target]]),
    mean_test = mean(test_data[[target]]),
    sd_train = sd(train_data[[target]]),
    sd_test = sd(test_data[[target]])
  )
  
  write.csv(
    opis_danych,
    file.path(results_dir, paste0("opis_danych_", suffix, ".csv")),
    row.names = FALSE
  )
  
  formula_full <- as.formula(
    paste(target, "~", paste(predyktory, collapse = " + "))
  )
  
  nvmax <- length(predyktory)
  
  # ----------------------------------------------------------
  # 4.1. Model zerowy i pełny OLS
  # ----------------------------------------------------------
  
  pred_null_train <- rep(mean(train_data[[target]]), nrow(train_data))
  pred_null_test <- rep(mean(train_data[[target]]), nrow(test_data))
  
  wynik_null <- wynik_tabelaryczny(
    target = target,
    model_name = "Model zerowy",
    liczba_predyktorow = 0,
    lambda_value = "nie dotyczy",
    zmienne = character(0),
    y_train = train_data[[target]],
    pred_train = pred_null_train,
    y_test = test_data[[target]],
    pred_test = pred_null_test
  )
  
  lm_full <- lm(formula_full, data = train_data)
  
  pred_full_train <- predict(lm_full, newdata = train_data)
  pred_full_test <- predict(lm_full, newdata = test_data)
  
  wynik_lm_full <- wynik_tabelaryczny(
    target = target,
    model_name = "OLS full",
    liczba_predyktorow = length(predyktory),
    lambda_value = "nie dotyczy",
    zmienne = predyktory,
    y_train = train_data[[target]],
    pred_train = pred_full_train,
    y_test = test_data[[target]],
    pred_test = pred_full_test
  )
  
  capture.output(
    summary(lm_full),
    file = file.path(results_dir, paste0("summary_lm_full_", suffix, ".txt"))
  )
  
  zapisz_wspolczynniki_lm(
    lm_full,
    paste0("wspolczynniki_lm_full_", suffix, ".csv")
  )
  
  # ----------------------------------------------------------
  # 4.2. Najlepszy podzbiór predyktorów - exhaustive
  # ----------------------------------------------------------
  
  fit_best <- regsubsets(
    x = X_train_regsubsets,
    y = y_train_regsubsets,
    nvmax = nvmax,
    method = "exhaustive"
  )
  
  fit_best_sum <- summary(fit_best)
  
  tabela_kryteriow <- data.frame(
    liczba_zmiennych = 1:nvmax,
    rsq = fit_best_sum$rsq,
    adjr2 = fit_best_sum$adjr2,
    cp = fit_best_sum$cp,
    bic = fit_best_sum$bic,
    rss = fit_best_sum$rss
  )
  
  write.csv(
    tabela_kryteriow,
    file.path(results_dir, paste0("selekcja_exhaustive_kryteria_", suffix, ".csv")),
    row.names = FALSE
  )
  
  bic_min <- which.min(fit_best_sum$bic)
  cp_min <- which.min(fit_best_sum$cp)
  adjr2_max <- which.max(fit_best_sum$adjr2)
  
  zmienne_bic <- pobierz_zmienne_regsubsets(fit_best, bic_min)
  zmienne_cp <- pobierz_zmienne_regsubsets(fit_best, cp_min)
  zmienne_adjr2 <- pobierz_zmienne_regsubsets(fit_best, adjr2_max)
  
  tabela_najlepsze_exhaustive <- data.frame(
    target = target,
    metoda = "exhaustive",
    kryterium = c("BIC", "Cp", "Adjusted R2"),
    liczba_zmiennych = c(bic_min, cp_min, adjr2_max),
    wartosc_kryterium = c(
      fit_best_sum$bic[bic_min],
      fit_best_sum$cp[cp_min],
      fit_best_sum$adjr2[adjr2_max]
    ),
    zmienne = c(
      paste(zmienne_bic, collapse = ", "),
      paste(zmienne_cp, collapse = ", "),
      paste(zmienne_adjr2, collapse = ", ")
    )
  )
  
  write.csv(
    tabela_najlepsze_exhaustive,
    file.path(results_dir, paste0("najlepsze_podzbiory_exhaustive_", suffix, ".csv")),
    row.names = FALSE
  )
  
  zapisz_wykres(paste0("01_BIC_exhaustive_", suffix, ".png"), {
    plot(
      fit_best_sum$bic,
      xlab = "Liczba zmiennych",
      ylab = "BIC",
      type = "b",
      pch = 20,
      main = paste("BIC - najlepszy podzbiór:", target)
    )
    points(bic_min, fit_best_sum$bic[bic_min], col = "red", pch = 9, cex = 1.5)
  })
  
  zapisz_wykres(paste0("02_Cp_exhaustive_", suffix, ".png"), {
    plot(
      fit_best_sum$cp,
      xlab = "Liczba zmiennych",
      ylab = "Cp",
      type = "b",
      pch = 20,
      main = paste("Cp - najlepszy podzbiór:", target)
    )
    points(cp_min, fit_best_sum$cp[cp_min], col = "red", pch = 9, cex = 1.5)
  })
  
  zapisz_wykres(paste0("03_AdjR2_exhaustive_", suffix, ".png"), {
    plot(
      fit_best_sum$adjr2,
      xlab = "Liczba zmiennych",
      ylab = "Adjusted R2",
      type = "b",
      pch = 20,
      main = paste("Adjusted R2 - najlepszy podzbiór:", target)
    )
    points(adjr2_max, fit_best_sum$adjr2[adjr2_max], col = "red", pch = 9, cex = 1.5)
  })
  
  wynik_bic_obj <- dopasuj_lm_dla_podzbioru(
    target,
    zmienne_bic,
    train_data,
    test_data,
    "Best subset BIC"
  )
  
  wynik_cp_obj <- dopasuj_lm_dla_podzbioru(
    target,
    zmienne_cp,
    train_data,
    test_data,
    "Best subset Cp"
  )
  
  wynik_adjr2_obj <- dopasuj_lm_dla_podzbioru(
    target,
    zmienne_adjr2,
    train_data,
    test_data,
    "Best subset Adjusted R2"
  )
  
  zapisz_wspolczynniki_lm(
    wynik_bic_obj$fit,
    paste0("wspolczynniki_lm_best_BIC_", suffix, ".csv")
  )
  
  zapisz_wspolczynniki_lm(
    wynik_cp_obj$fit,
    paste0("wspolczynniki_lm_best_Cp_", suffix, ".csv")
  )
  
  zapisz_wspolczynniki_lm(
    wynik_adjr2_obj$fit,
    paste0("wspolczynniki_lm_best_AdjR2_", suffix, ".csv")
  )
  
  # ----------------------------------------------------------
  # 4.3. Bardziej agresywna selekcja: stała liczba predyktorów
  # ----------------------------------------------------------
  
  agresywne_k <- c(3, 5, 8, 10)
  agresywne_k <- agresywne_k[agresywne_k <= nvmax]
  
  wyniki_agresywne <- list()
  
  for (k_pred in agresywne_k) {
    zmienne_k <- pobierz_zmienne_regsubsets(fit_best, k_pred)
    
    wynik_k <- dopasuj_lm_dla_podzbioru(
      target,
      zmienne_k,
      train_data,
      test_data,
      paste0("Best subset k=", k_pred)
    )
    
    wyniki_agresywne[[paste0("k_", k_pred)]] <- wynik_k$wynik
  }
  
  wyniki_agresywne <- do.call(rbind, wyniki_agresywne)
  
  write.csv(
    wyniki_agresywne,
    file.path(results_dir, paste0("agresywne_podzbiory_", suffix, ".csv")),
    row.names = FALSE
  )
  
  # ----------------------------------------------------------
  # 4.4. Selekcja krokowa w przód i wstecz
  # ----------------------------------------------------------
  
  fit_forward <- regsubsets(
    x = X_train_regsubsets,
    y = y_train_regsubsets,
    nvmax = nvmax,
    method = "forward"
  )
  
  fit_backward <- regsubsets(
    x = X_train_regsubsets,
    y = y_train_regsubsets,
    nvmax = nvmax,
    method = "backward"
  )
  
  sum_forward <- summary(fit_forward)
  sum_backward <- summary(fit_backward)
  
  wybierz_kryteria <- function(fit, fit_sum, metoda) {
    id_bic <- which.min(fit_sum$bic)
    id_cp <- which.min(fit_sum$cp)
    id_adjr2 <- which.max(fit_sum$adjr2)
    
    data.frame(
      target = target,
      metoda = metoda,
      kryterium = c("BIC", "Cp", "Adjusted R2"),
      liczba_zmiennych = c(id_bic, id_cp, id_adjr2),
      wartosc_kryterium = c(
        fit_sum$bic[id_bic],
        fit_sum$cp[id_cp],
        fit_sum$adjr2[id_adjr2]
      ),
      zmienne = c(
        paste(pobierz_zmienne_regsubsets(fit, id_bic), collapse = ", "),
        paste(pobierz_zmienne_regsubsets(fit, id_cp), collapse = ", "),
        paste(pobierz_zmienne_regsubsets(fit, id_adjr2), collapse = ", ")
      )
    )
  }
  
  tabela_forward_backward <- rbind(
    wybierz_kryteria(fit_forward, sum_forward, "forward"),
    wybierz_kryteria(fit_backward, sum_backward, "backward")
  )
  
  write.csv(
    tabela_forward_backward,
    file.path(results_dir, paste0("selekcja_forward_backward_", suffix, ".csv")),
    row.names = FALSE
  )
  
  # ----------------------------------------------------------
  # 4.5. Metoda zbioru walidacyjnego
  # ----------------------------------------------------------
  
  set.seed(2025)
  
  n_train <- nrow(train_data)
  inner_train_id <- sample(seq_len(n_train), size = floor(0.6 * n_train))
  inner_val_id <- setdiff(seq_len(n_train), inner_train_id)
  
  fit_val <- regsubsets(
    x = X_train_regsubsets[inner_train_id, , drop = FALSE],
    y = y_train_regsubsets[inner_train_id],
    nvmax = nvmax,
    method = "exhaustive"
  )
  
  val_errors <- numeric(nvmax)
  
  for (i in 1:nvmax) {
    y_pred <- predict_regsubsets(
      fit_val,
      newx = X_train_regsubsets[inner_val_id, , drop = FALSE],
      id = i
    )
    
    val_errors[i] <- mean((as.numeric(y_pred) - y_train_regsubsets[inner_val_id])^2)
  }
  
  val_min <- which.min(val_errors)
  
  # Agresywniejsza wersja: najmniejszy model z błędem nie większym niż 1% od minimum.
  val_threshold_1pct <- val_errors[val_min] * 1.01
  val_1pct <- min(which(val_errors <= val_threshold_1pct))
  
  zmienne_val_min <- pobierz_zmienne_regsubsets(fit_best, val_min)
  zmienne_val_1pct <- pobierz_zmienne_regsubsets(fit_best, val_1pct)
  
  wynik_val_min_obj <- dopasuj_lm_dla_podzbioru(
    target,
    zmienne_val_min,
    train_data,
    test_data,
    "Validation set min"
  )
  
  wynik_val_1pct_obj <- dopasuj_lm_dla_podzbioru(
    target,
    zmienne_val_1pct,
    train_data,
    test_data,
    "Validation set 1pct"
  )
  
  tabela_val <- data.frame(
    target = target,
    liczba_zmiennych = 1:nvmax,
    validation_MSE = val_errors
  )
  
  write.csv(
    tabela_val,
    file.path(results_dir, paste0("validation_errors_", suffix, ".csv")),
    row.names = FALSE
  )
  
  validation_best <- data.frame(
    target = target,
    wariant = c("minimum", "1pct"),
    liczba_zmiennych = c(val_min, val_1pct),
    validation_MSE = c(val_errors[val_min], val_errors[val_1pct]),
    prog = c(val_errors[val_min], val_threshold_1pct),
    zmienne = c(
      paste(zmienne_val_min, collapse = ", "),
      paste(zmienne_val_1pct, collapse = ", ")
    )
  )
  
  write.csv(
    validation_best,
    file.path(results_dir, paste0("validation_best_", suffix, ".csv")),
    row.names = FALSE
  )
  
  zapisz_wykres(paste0("04_validation_MSE_", suffix, ".png"), {
    plot(
      val_errors,
      xlab = "Liczba zmiennych",
      ylab = "Walidacyjny MSE",
      type = "b",
      pch = 20,
      main = paste("Metoda zbioru walidacyjnego:", target)
    )
    points(val_min, val_errors[val_min], col = "red", pch = 9, cex = 1.5)
    points(val_1pct, val_errors[val_1pct], col = "blue", pch = 8, cex = 1.5)
    abline(h = val_threshold_1pct, lty = 2)
    legend(
      "topright",
      legend = c("Minimum", "Model 1%"),
      col = c("red", "blue"),
      pch = c(9, 8),
      bty = "n"
    )
  })
  
  # ----------------------------------------------------------
  # 4.6. 10-krotna walidacja krzyżowa
  # ----------------------------------------------------------
  
  set.seed(2025)
  
  k <- 10
  folds <- sample(rep(1:k, length.out = nrow(train_data)))
  
  cv_matrix <- matrix(NA, nrow = k, ncol = nvmax)
  
  for (j in 1:k) {
    current_train <- folds != j
    current_test <- folds == j
    
    fit_cv <- regsubsets(
      x = X_train_regsubsets[current_train, , drop = FALSE],
      y = y_train_regsubsets[current_train],
      nvmax = nvmax,
      method = "exhaustive"
    )
    
    for (i in 1:nvmax) {
      y_pred <- predict_regsubsets(
        fit_cv,
        newx = X_train_regsubsets[current_test, , drop = FALSE],
        id = i
      )
      
      cv_matrix[j, i] <- mean((as.numeric(y_pred) - y_train_regsubsets[current_test])^2)
    }
  }
  
  cv_errors <- colMeans(cv_matrix)
  cv_se <- apply(cv_matrix, 2, sd) / sqrt(k)
  
  cv_min <- which.min(cv_errors)
  
  # Agresywniejsza wersja: najmniejszy model w granicy 1 błędu standardowego.
  cv_threshold_1se <- cv_errors[cv_min] + cv_se[cv_min]
  cv_1se <- min(which(cv_errors <= cv_threshold_1se))
  
  zmienne_cv_min <- pobierz_zmienne_regsubsets(fit_best, cv_min)
  zmienne_cv_1se <- pobierz_zmienne_regsubsets(fit_best, cv_1se)
  
  wynik_cv_min_obj <- dopasuj_lm_dla_podzbioru(
    target,
    zmienne_cv_min,
    train_data,
    test_data,
    "Cross-validation min"
  )
  
  wynik_cv_1se_obj <- dopasuj_lm_dla_podzbioru(
    target,
    zmienne_cv_1se,
    train_data,
    test_data,
    "Cross-validation 1SE"
  )
  
  tabela_cv <- data.frame(
    target = target,
    liczba_zmiennych = 1:nvmax,
    CV_MSE = cv_errors,
    CV_SE = cv_se
  )
  
  write.csv(
    tabela_cv,
    file.path(results_dir, paste0("cv_errors_", suffix, ".csv")),
    row.names = FALSE
  )
  
  cv_best <- data.frame(
    target = target,
    wariant = c("minimum", "1SE"),
    liczba_zmiennych = c(cv_min, cv_1se),
    CV_MSE = c(cv_errors[cv_min], cv_errors[cv_1se]),
    CV_SE = c(cv_se[cv_min], cv_se[cv_1se]),
    prog = c(cv_errors[cv_min], cv_threshold_1se),
    zmienne = c(
      paste(zmienne_cv_min, collapse = ", "),
      paste(zmienne_cv_1se, collapse = ", ")
    )
  )
  
  write.csv(
    cv_best,
    file.path(results_dir, paste0("cv_best_", suffix, ".csv")),
    row.names = FALSE
  )
  
  zapisz_wykres(paste0("05_CV_MSE_", suffix, ".png"), {
    plot(
      cv_errors,
      xlab = "Liczba zmiennych",
      ylab = "CV MSE",
      type = "b",
      pch = 20,
      main = paste("10-krotna walidacja krzyżowa:", target)
    )
    arrows(
      x0 = 1:nvmax,
      y0 = cv_errors - cv_se,
      x1 = 1:nvmax,
      y1 = cv_errors + cv_se,
      angle = 90,
      code = 3,
      length = 0.04,
      col = "gray"
    )
    points(cv_min, cv_errors[cv_min], col = "red", pch = 9, cex = 1.5)
    points(cv_1se, cv_errors[cv_1se], col = "blue", pch = 8, cex = 1.5)
    abline(h = cv_threshold_1se, lty = 2)
    legend(
      "topright",
      legend = c("Minimum", "Model 1SE"),
      col = c("red", "blue"),
      pch = c(9, 8),
      bty = "n"
    )
  })
  
  # ----------------------------------------------------------
  # 4.7. Przygotowanie danych dla glmnet
  # ----------------------------------------------------------
  
  X_train <- model.matrix(formula_full, data = train_data)[, -1]
  y_train <- train_data[[target]]
  
  X_test <- model.matrix(formula_full, data = test_data)[, -1]
  y_test <- test_data[[target]]
  
  liczba_predyktorow_glmnet <- ncol(X_train)
  wszystkie_zmienne_glmnet <- colnames(X_train)
  
  # ----------------------------------------------------------
  # 4.8. Ridge
  # ----------------------------------------------------------
  
  fit_ridge <- glmnet(
    X_train,
    y_train,
    alpha = 0,
    nlambda = 200,
    lambda.min.ratio = 1e-5,
    thresh = 1e-12
  )
  
  zapisz_wykres(paste0("06_ridge_path_", suffix, ".png"), {
    plot(
      fit_ridge,
      xvar = "lambda",
      main = paste("Ridge - ścieżki współczynników:", target)
    )
  })
  
  ridge_coefs <- coef(fit_ridge)[-1, ]
  
  zapisz_wykres(paste0("07_ridge_L2_norm_", suffix, ".png"), {
    plot(
      sqrt(colSums(ridge_coefs^2)),
      type = "l",
      ylab = "Norma L2 współczynników",
      xlab = "Indeks lambda",
      main = paste("Norma L2 współczynników - ridge:", target)
    )
  })
  
  set.seed(2025)
  
  fit_ridge_cv <- cv.glmnet(
    X_train,
    y_train,
    alpha = 0
  )
  
  ridge_lambda_min <- fit_ridge_cv$lambda.min
  ridge_lambda_1se <- fit_ridge_cv$lambda.1se
  
  zapisz_wykres(paste0("08_ridge_cv_", suffix, ".png"), {
    plot(
      fit_ridge_cv,
      main = paste("CV ridge:", target)
    )
  })
  
  pred_ridge_min_train <- predict(fit_ridge, s = ridge_lambda_min, newx = X_train)
  pred_ridge_min_test <- predict(fit_ridge, s = ridge_lambda_min, newx = X_test)
  
  pred_ridge_1se_train <- predict(fit_ridge, s = ridge_lambda_1se, newx = X_train)
  pred_ridge_1se_test <- predict(fit_ridge, s = ridge_lambda_1se, newx = X_test)
  
  wynik_ridge_min <- wynik_tabelaryczny(
    target = target,
    model_name = "Ridge lambda.min",
    liczba_predyktorow = liczba_predyktorow_glmnet,
    lambda_value = as.character(signif(ridge_lambda_min, 8)),
    zmienne = wszystkie_zmienne_glmnet,
    y_train = y_train,
    pred_train = as.numeric(pred_ridge_min_train),
    y_test = y_test,
    pred_test = as.numeric(pred_ridge_min_test)
  )
  
  wynik_ridge_1se <- wynik_tabelaryczny(
    target = target,
    model_name = "Ridge lambda.1se",
    liczba_predyktorow = liczba_predyktorow_glmnet,
    lambda_value = as.character(signif(ridge_lambda_1se, 8)),
    zmienne = wszystkie_zmienne_glmnet,
    y_train = y_train,
    pred_train = as.numeric(pred_ridge_1se_train),
    y_test = y_test,
    pred_test = as.numeric(pred_ridge_1se_test)
  )
  
  ridge_test_mse_path <- sapply(fit_ridge$lambda, function(lam) {
    pred <- predict(fit_ridge, s = lam, newx = X_test)
    mse(y_test, as.numeric(pred))
  })
  
  zapisz_wykres(paste0("09_ridge_test_MSE_path_", suffix, ".png"), {
    plot(
      ridge_test_mse_path ~ log(fit_ridge$lambda),
      type = "l",
      xlab = "log(lambda)",
      ylab = "Test MSE",
      main = paste("Test MSE względem lambda - ridge:", target)
    )
    abline(v = log(ridge_lambda_min), lty = "dotted")
    abline(v = log(ridge_lambda_1se), lty = "dotted")
  })
  
  ridge_coef_min <- as.matrix(coef(fit_ridge, s = ridge_lambda_min))
  ridge_coef_1se <- as.matrix(coef(fit_ridge, s = ridge_lambda_1se))
  
  write.csv(
    data.frame(
      parametr = rownames(ridge_coef_min),
      coefficient_lambda_min = as.numeric(ridge_coef_min[, 1])
    ),
    file.path(results_dir, paste0("ridge_coefficients_lambda_min_", suffix, ".csv")),
    row.names = FALSE
  )
  
  write.csv(
    data.frame(
      parametr = rownames(ridge_coef_1se),
      coefficient_lambda_1se = as.numeric(ridge_coef_1se[, 1])
    ),
    file.path(results_dir, paste0("ridge_coefficients_lambda_1se_", suffix, ".csv")),
    row.names = FALSE
  )
  
  # ----------------------------------------------------------
  # 4.9. Lasso
  # ----------------------------------------------------------
  
  fit_lasso <- glmnet(
    X_train,
    y_train,
    alpha = 1,
    thresh = 1e-12
  )
  
  zapisz_wykres(paste0("10_lasso_path_", suffix, ".png"), {
    plot(
      fit_lasso,
      xvar = "lambda",
      main = paste("Lasso - ścieżki współczynników:", target)
    )
  })
  
  set.seed(2025)
  
  fit_lasso_cv <- cv.glmnet(
    X_train,
    y_train,
    alpha = 1
  )
  
  lasso_lambda_min <- fit_lasso_cv$lambda.min
  lasso_lambda_1se <- fit_lasso_cv$lambda.1se
  
  zapisz_wykres(paste0("11_lasso_cv_", suffix, ".png"), {
    plot(
      fit_lasso_cv,
      main = paste("CV lasso:", target)
    )
  })
  
  pred_lasso_min_train <- predict(fit_lasso, s = lasso_lambda_min, newx = X_train)
  pred_lasso_min_test <- predict(fit_lasso, s = lasso_lambda_min, newx = X_test)
  
  pred_lasso_1se_train <- predict(fit_lasso, s = lasso_lambda_1se, newx = X_train)
  pred_lasso_1se_test <- predict(fit_lasso, s = lasso_lambda_1se, newx = X_test)
  
  lasso_coef_min <- as.matrix(coef(fit_lasso, s = lasso_lambda_min))
  lasso_coef_1se <- as.matrix(coef(fit_lasso, s = lasso_lambda_1se))
  
  zmienne_lasso_min <- pobierz_zmienne_niezerowe(lasso_coef_min)
  zmienne_lasso_1se <- pobierz_zmienne_niezerowe(lasso_coef_1se)
  
  wynik_lasso_min <- wynik_tabelaryczny(
    target = target,
    model_name = "Lasso lambda.min",
    liczba_predyktorow = length(zmienne_lasso_min),
    lambda_value = as.character(signif(lasso_lambda_min, 8)),
    zmienne = zmienne_lasso_min,
    y_train = y_train,
    pred_train = as.numeric(pred_lasso_min_train),
    y_test = y_test,
    pred_test = as.numeric(pred_lasso_min_test)
  )
  
  wynik_lasso_1se <- wynik_tabelaryczny(
    target = target,
    model_name = "Lasso lambda.1se",
    liczba_predyktorow = length(zmienne_lasso_1se),
    lambda_value = as.character(signif(lasso_lambda_1se, 8)),
    zmienne = zmienne_lasso_1se,
    y_train = y_train,
    pred_train = as.numeric(pred_lasso_1se_train),
    y_test = y_test,
    pred_test = as.numeric(pred_lasso_1se_test)
  )
  
  lasso_test_mse_path <- sapply(fit_lasso$lambda, function(lam) {
    pred <- predict(fit_lasso, s = lam, newx = X_test)
    mse(y_test, as.numeric(pred))
  })
  
  zapisz_wykres(paste0("12_lasso_test_MSE_path_", suffix, ".png"), {
    plot(
      lasso_test_mse_path ~ log(fit_lasso$lambda),
      type = "l",
      xlab = "log(lambda)",
      ylab = "Test MSE",
      main = paste("Test MSE względem lambda - lasso:", target)
    )
    abline(v = log(lasso_lambda_min), lty = "dotted")
    abline(v = log(lasso_lambda_1se), lty = "dotted")
  })
  
  write.csv(
    data.frame(
      parametr = rownames(lasso_coef_min),
      coefficient_lambda_min = as.numeric(lasso_coef_min[, 1])
    ),
    file.path(results_dir, paste0("lasso_coefficients_lambda_min_", suffix, ".csv")),
    row.names = FALSE
  )
  
  write.csv(
    data.frame(
      parametr = rownames(lasso_coef_1se),
      coefficient_lambda_1se = as.numeric(lasso_coef_1se[, 1])
    ),
    file.path(results_dir, paste0("lasso_coefficients_lambda_1se_", suffix, ".csv")),
    row.names = FALSE
  )
  
  # ----------------------------------------------------------
  # 4.10. Porównanie metod
  # ----------------------------------------------------------
  
  porownanie_koncowe <- rbind(
    wynik_null,
    wynik_lm_full,
    wynik_bic_obj$wynik,
    wynik_cp_obj$wynik,
    wynik_adjr2_obj$wynik,
    wyniki_agresywne,
    wynik_val_min_obj$wynik,
    wynik_val_1pct_obj$wynik,
    wynik_cv_min_obj$wynik,
    wynik_cv_1se_obj$wynik,
    wynik_ridge_min,
    wynik_ridge_1se,
    wynik_lasso_min,
    wynik_lasso_1se
  )
  
  porownanie_koncowe <- porownanie_koncowe[order(porownanie_koncowe$MSE_test), ]
  
  write.csv(
    porownanie_koncowe,
    file.path(results_dir, paste0("porownanie_koncowe_", suffix, ".csv")),
    row.names = FALSE
  )
  
  najlepszy_model <- porownanie_koncowe[1, , drop = FALSE]
  
  # Najlepszy model oszczędny:
  # wybieramy modele z błędem testowym maksymalnie 1% gorszym od najlepszego,
  # a następnie wybieramy ten z najmniejszą liczbą predyktorów.
  prog_kompromisu <- najlepszy_model$MSE_test[1] * 1.01
  
  kandydaci_kompromis <- porownanie_koncowe %>%
    dplyr::filter(MSE_test <= prog_kompromisu) %>%
    dplyr::arrange(liczba_predyktorow, MSE_test)
  
  najlepszy_model_oszczedny <- kandydaci_kompromis[1, , drop = FALSE]
  
  write.csv(
    najlepszy_model,
    file.path(results_dir, paste0("najlepszy_model_", suffix, ".csv")),
    row.names = FALSE
  )
  
  write.csv(
    najlepszy_model_oszczedny,
    file.path(results_dir, paste0("najlepszy_model_oszczedny_", suffix, ".csv")),
    row.names = FALSE
  )
  
  zapisz_wykres(paste0("13_porownanie_RMSE_test_", suffix, ".png"), {
    par(mar = c(12, 5, 4, 2))
    
    barplot(
      porownanie_koncowe$RMSE_test,
      names.arg = porownanie_koncowe$model,
      las = 2,
      cex.names = 0.65,
      ylab = "RMSE test",
      main = paste("Porównanie RMSE test:", target)
    )
  })
  
  zapisz_wykres(paste0("14_porownanie_R2_test_", suffix, ".png"), {
    par(mar = c(12, 5, 4, 2))
    
    barplot(
      porownanie_koncowe$R2_test,
      names.arg = porownanie_koncowe$model,
      las = 2,
      cex.names = 0.65,
      ylab = "R2 test",
      main = paste("Porównanie R2 test:", target)
    )
    abline(h = 0, lty = 2)
  })
  
  zapisz_wykres(paste0("15_rzeczywiste_vs_pred_OLS_full_", suffix, ".png"), {
    plot(
      test_data[[target]],
      pred_full_test,
      main = paste("OLS full: rzeczywiste vs przewidywane:", target),
      xlab = "Wartości rzeczywiste",
      ylab = "Wartości przewidywane"
    )
    abline(0, 1, lwd = 2)
  })
  
  zapisz_wykres(paste0("16_rzeczywiste_vs_pred_BIC_", suffix, ".png"), {
    plot(
      test_data[[target]],
      wynik_bic_obj$pred_test,
      main = paste("Best subset BIC: rzeczywiste vs przewidywane:", target),
      xlab = "Wartości rzeczywiste",
      ylab = "Wartości przewidywane"
    )
    abline(0, 1, lwd = 2)
  })
  
  zapisz_wykres(paste0("17_szereg_test_OLS_full_", suffix, ".png"), {
    n_plot <- min(500, nrow(test_data))
    
    plot(
      test_data$datetime[1:n_plot],
      test_data[[target]][1:n_plot],
      type = "l",
      main = paste("OLS full: wartości rzeczywiste i przewidywane:", target),
      xlab = "Czas",
      ylab = target
    )
    
    lines(
      test_data$datetime[1:n_plot],
      pred_full_test[1:n_plot],
      lty = 2,
      lwd = 2
    )
    
    legend(
      "topright",
      legend = c("Rzeczywiste", "Przewidywane"),
      lty = c(1, 2),
      lwd = c(1, 2),
      bty = "n"
    )
  })
  
  zapisz_wykres(paste0("18_szereg_test_BIC_", suffix, ".png"), {
    n_plot <- min(500, nrow(test_data))
    
    plot(
      test_data$datetime[1:n_plot],
      test_data[[target]][1:n_plot],
      type = "l",
      main = paste("Best subset BIC: wartości rzeczywiste i przewidywane:", target),
      xlab = "Czas",
      ylab = target
    )
    
    lines(
      test_data$datetime[1:n_plot],
      wynik_bic_obj$pred_test[1:n_plot],
      lty = 2,
      lwd = 2
    )
    
    legend(
      "topright",
      legend = c("Rzeczywiste", "Przewidywane"),
      lty = c(1, 2),
      lwd = c(1, 2),
      bty = "n"
    )
  })
  
  cat("\n--- Opis danych ---\n")
  print(opis_danych)
  
  cat("\n--- Najlepsze podzbiory exhaustive ---\n")
  print(tabela_najlepsze_exhaustive)
  
  cat("\n--- Forward / Backward ---\n")
  print(tabela_forward_backward)
  
  cat("\n--- Validation set ---\n")
  print(validation_best)
  
  cat("\n--- Cross-validation ---\n")
  print(cv_best)
  
  cat("\n--- Lambdy ridge ---\n")
  print(data.frame(
    target = target,
    lambda_min = ridge_lambda_min,
    lambda_1se = ridge_lambda_1se,
    liczba_predyktorow = liczba_predyktorow_glmnet
  ))
  
  cat("\n--- Lambdy lasso ---\n")
  print(data.frame(
    target = target,
    lambda_min = lasso_lambda_min,
    lambda_1se = lasso_lambda_1se,
    liczba_predyktorow_lambda_min = length(zmienne_lasso_min),
    liczba_predyktorow_lambda_1se = length(zmienne_lasso_1se)
  ))
  
  cat("\n--- Najlepszy model według MSE test ---\n")
  print(najlepszy_model)
  
  cat("\n--- Najlepszy model oszczędny w granicy 1% od najlepszego MSE test ---\n")
  print(najlepszy_model_oszczedny)
  
  cat("\n--- Porównanie końcowe ---\n")
  print(porownanie_koncowe)
  
  list(
    target = target,
    opis_danych = opis_danych,
    tabela_najlepsze_exhaustive = tabela_najlepsze_exhaustive,
    tabela_forward_backward = tabela_forward_backward,
    validation_best = validation_best,
    cv_best = cv_best,
    ridge_lambdas = data.frame(
      target = target,
      lambda_min = ridge_lambda_min,
      lambda_1se = ridge_lambda_1se,
      liczba_predyktorow = liczba_predyktorow_glmnet
    ),
    lasso_lambdas = data.frame(
      target = target,
      lambda_min = lasso_lambda_min,
      lambda_1se = lasso_lambda_1se,
      liczba_predyktorow_lambda_min = length(zmienne_lasso_min),
      liczba_predyktorow_lambda_1se = length(zmienne_lasso_1se)
    ),
    porownanie_koncowe = porownanie_koncowe,
    najlepszy_model = najlepszy_model,
    najlepszy_model_oszczedny = najlepszy_model_oszczedny
  )
}

# ------------------------------------------------------------
# 5. Uruchomienie analizy dla obu zmiennych zależnych
# ------------------------------------------------------------

wyniki_lista <- lapply(targety, analizuj_target)

# ------------------------------------------------------------
# 6. Zbiorcze tabele dla obu zmiennych
# ------------------------------------------------------------

zbiorcze_opis_danych <- do.call(
  rbind,
  lapply(wyniki_lista, function(x) x$opis_danych)
)

zbiorcze_exhaustive <- do.call(
  rbind,
  lapply(wyniki_lista, function(x) x$tabela_najlepsze_exhaustive)
)

zbiorcze_forward_backward <- do.call(
  rbind,
  lapply(wyniki_lista, function(x) x$tabela_forward_backward)
)

zbiorcze_validation <- do.call(
  rbind,
  lapply(wyniki_lista, function(x) x$validation_best)
)

zbiorcze_cv <- do.call(
  rbind,
  lapply(wyniki_lista, function(x) x$cv_best)
)

zbiorcze_ridge <- do.call(
  rbind,
  lapply(wyniki_lista, function(x) x$ridge_lambdas)
)

zbiorcze_lasso <- do.call(
  rbind,
  lapply(wyniki_lista, function(x) x$lasso_lambdas)
)

zbiorcze_porownanie <- do.call(
  rbind,
  lapply(wyniki_lista, function(x) x$porownanie_koncowe)
)

zbiorcze_najlepsze <- do.call(
  rbind,
  lapply(wyniki_lista, function(x) x$najlepszy_model)
)

zbiorcze_najlepsze_oszczedne <- do.call(
  rbind,
  lapply(wyniki_lista, function(x) x$najlepszy_model_oszczedny)
)

write.csv(
  zbiorcze_opis_danych,
  file.path(results_dir, "zbiorcze_opis_danych.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_exhaustive,
  file.path(results_dir, "zbiorcze_exhaustive.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_forward_backward,
  file.path(results_dir, "zbiorcze_forward_backward.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_validation,
  file.path(results_dir, "zbiorcze_validation.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_cv,
  file.path(results_dir, "zbiorcze_cv.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_ridge,
  file.path(results_dir, "zbiorcze_ridge.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_lasso,
  file.path(results_dir, "zbiorcze_lasso.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_porownanie,
  file.path(results_dir, "zbiorcze_porownanie_koncowe.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_najlepsze,
  file.path(results_dir, "zbiorcze_najlepsze_modele.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_najlepsze_oszczedne,
  file.path(results_dir, "zbiorcze_najlepsze_modele_oszczedne.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 7. Komunikat końcowy
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ZAKOŃCZONO ANALIZĘ SELEKCJI I REGULARYZACJI\n")
cat("Predykcja dotyczy wartości w następnej godzinie: t + 1.\n")
cat("Wykresy zapisano w folderze:", plot_dir, "\n")
cat("Tabele zapisano w folderze:", results_dir, "\n")
cat("============================================================\n")

cat("\nZbiorcze wyniki exhaustive:\n")
print(zbiorcze_exhaustive)

cat("\nZbiorcze wyniki forward/backward:\n")
print(zbiorcze_forward_backward)

cat("\nZbiorcze wyniki walidacji:\n")
print(zbiorcze_validation)

cat("\nZbiorcze wyniki CV:\n")
print(zbiorcze_cv)

cat("\nZbiorcze lambdy ridge:\n")
print(zbiorcze_ridge)

cat("\nZbiorcze lambdy lasso:\n")
print(zbiorcze_lasso)

cat("\nZbiorcze najlepsze modele:\n")
print(zbiorcze_najlepsze)

cat("\nZbiorcze najlepsze modele oszczędne:\n")
print(zbiorcze_najlepsze_oszczedne)

cat("\nZbiorcze porównanie końcowe:\n")
print(zbiorcze_porownanie)