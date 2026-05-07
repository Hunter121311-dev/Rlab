# ============================================================
# MODELE NIELINIOWE DLA DANYCH METEOROLOGICZNYCH
# PREDYKCJA PRĘDKOŚCI WIATRU I PORYWÓW W NASTĘPNEJ GODZINIE
# ============================================================

# ------------------------------------------------------------
# 0. Pakiety i ustawienia
# ------------------------------------------------------------

library(dplyr)
library(lubridate)
library(splines)
library(mgcv)

plot_dir <- "plots_nonlinear"
results_dir <- "wyniki_nonlinear"

dir.create(plot_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)

set.seed(2025)

zapisz_wykres <- function(nazwa, kod) {
  png(
    filename = file.path(plot_dir, nazwa),
    width = 1300,
    height = 850,
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

buduj_formule <- function(target, skladniki) {
  stats::as.formula(
    paste(target, "~", paste(skladniki, collapse = " + ")),
    env = parent.frame()
  )
}

wynik_tabelaryczny <- function(
    target,
    model_name,
    typ_modelu,
    liczba_predyktorow,
    parametry,
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
    typ_modelu = typ_modelu,
    liczba_predyktorow = liczba_predyktorow,
    parametry = parametry,
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

# Predykcja dotyczy następnej godziny: t + 1.
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
# 4. Funkcje pomocnicze dla modeli nieliniowych
# ------------------------------------------------------------

utworz_zmienne_schodkowe <- function(train_data, test_data, zmienna, liczba_przedzialow) {
  kwantyle <- quantile(
    train_data[[zmienna]],
    probs = seq(0, 1, length.out = liczba_przedzialow + 1),
    na.rm = TRUE
  )
  
  kwantyle <- unique(as.numeric(kwantyle))
  breaks <- c(-Inf, kwantyle[-c(1, length(kwantyle))], Inf)
  
  nowa_nazwa <- paste0(zmienna, "_step_", liczba_przedzialow)
  
  train_data[[nowa_nazwa]] <- cut(
    train_data[[zmienna]],
    breaks = breaks,
    include.lowest = TRUE
  )
  
  test_data[[nowa_nazwa]] <- cut(
    test_data[[zmienna]],
    breaks = breaks,
    include.lowest = TRUE
  )
  
  list(
    train_data = train_data,
    test_data = test_data,
    zmienna_step = nowa_nazwa,
    breaks = breaks
  )
}

przygotuj_smooth_data <- function(data, xvar, yvar, n_max = 50000) {
  if (nrow(data) > n_max) {
    set.seed(2025)
    data <- data[sample(seq_len(nrow(data)), n_max), ]
  }
  
  tmp <- data.frame(
    x = data[[xvar]],
    y = data[[yvar]]
  )
  
  tmp <- tmp %>%
    dplyr::filter(complete.cases(.)) %>%
    dplyr::group_by(x) %>%
    dplyr::summarise(y = mean(y), .groups = "drop") %>%
    dplyr::arrange(x)
  
  tmp
}

utworz_grid_predykcyjny <- function(train_data, xvar, predyktory, n = 250) {
  grid <- seq(
    min(train_data[[xvar]], na.rm = TRUE),
    max(train_data[[xvar]], na.rm = TRUE),
    length.out = n
  )
  
  newdata <- data.frame(matrix(nrow = n, ncol = 0))
  
  for (zm in predyktory) {
    newdata[[zm]] <- median(train_data[[zm]], na.rm = TRUE)
  }
  
  newdata[[xvar]] <- grid
  
  list(
    grid = grid,
    newdata = newdata
  )
}

# ------------------------------------------------------------
# 5. Funkcja wykonująca analizę dla jednej zmiennej zależnej
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
  
  y_train <- train_data[[target]]
  y_test <- test_data[[target]]
  
  opis_danych <- data.frame(
    target = target,
    n_total = nrow(dane_model),
    n_train = nrow(train_data),
    n_test = nrow(test_data),
    mean_train = mean(y_train),
    mean_test = mean(y_test),
    sd_train = sd(y_train),
    sd_test = sd(y_test)
  )
  
  write.csv(
    opis_danych,
    file.path(results_dir, paste0("opis_danych_", suffix, ".csv")),
    row.names = FALSE
  )
  
  wyniki <- list()
  predykcje_test <- list()
  
  dodaj_model <- function(
    model_name,
    typ_modelu,
    liczba_predyktorow,
    parametry,
    zmienne,
    pred_train,
    pred_test
  ) {
    wyniki[[model_name]] <<- wynik_tabelaryczny(
      target = target,
      model_name = model_name,
      typ_modelu = typ_modelu,
      liczba_predyktorow = liczba_predyktorow,
      parametry = parametry,
      zmienne = zmienne,
      y_train = y_train,
      pred_train = as.numeric(pred_train),
      y_test = y_test,
      pred_test = as.numeric(pred_test)
    )
    
    predykcje_test[[model_name]] <<- as.numeric(pred_test)
  }
  
  # ----------------------------------------------------------
  # 5.1. Model zerowy i model liniowy
  # ----------------------------------------------------------
  
  pred_null_train <- rep(mean(y_train), length(y_train))
  pred_null_test <- rep(mean(y_train), length(y_test))
  
  dodaj_model(
    model_name = "Model zerowy",
    typ_modelu = "bazowy",
    liczba_predyktorow = 0,
    parametry = "średnia ze zbioru treningowego",
    zmienne = character(0),
    pred_train = pred_null_train,
    pred_test = pred_null_test
  )
  
  formula_lm_full <- buduj_formule(target, predyktory)
  
  fit_lm_full <- lm(formula_lm_full, data = train_data)
  
  pred_lm_full_train <- predict(fit_lm_full, newdata = train_data)
  pred_lm_full_test <- predict(fit_lm_full, newdata = test_data)
  
  dodaj_model(
    model_name = "OLS full",
    typ_modelu = "liniowy",
    liczba_predyktorow = length(predyktory),
    parametry = "pełny model liniowy",
    zmienne = predyktory,
    pred_train = pred_lm_full_train,
    pred_test = pred_lm_full_test
  )
  
  capture.output(
    summary(fit_lm_full),
    file = file.path(results_dir, paste0("summary_lm_full_", suffix, ".txt"))
  )
  
  zapisz_wspolczynniki_lm(
    fit_lm_full,
    paste0("wspolczynniki_lm_full_", suffix, ".csv")
  )
  
  # Zmienne, dla których modelujemy nieliniowość
  zmienne_nieliniowe <- c(
    "wind_speed_10m",
    "wind_speed_100m",
    "wind_gusts_10m"
  )
  
  zmienne_nieliniowe <- zmienne_nieliniowe[zmienne_nieliniowe %in% predyktory]
  zmienne_liniowe <- setdiff(predyktory, zmienne_nieliniowe)
  
  if (target == "wind_speed_10m_next") {
    glowny_predyktor <- "wind_speed_10m"
  } else {
    glowny_predyktor <- "wind_gusts_10m"
  }
  
  # ----------------------------------------------------------
  # 5.2. Regresja wielomianowa
  # ----------------------------------------------------------
  
  for (stopien in c(2, 3, 4, 5)) {
    skladniki_poly <- c(
      paste0("poly(", zmienne_nieliniowe, ", ", stopien, ")"),
      zmienne_liniowe
    )
    
    formula_poly <- buduj_formule(target, skladniki_poly)
    
    fit_poly <- lm(formula_poly, data = train_data)
    
    pred_poly_train <- predict(fit_poly, newdata = train_data)
    pred_poly_test <- predict(fit_poly, newdata = test_data)
    
    nazwa <- paste0("Regresja wielomianowa st. ", stopien)
    
    dodaj_model(
      model_name = nazwa,
      typ_modelu = "wielomianowy",
      liczba_predyktorow = length(predyktory),
      parametry = paste0("stopień = ", stopien),
      zmienne = predyktory,
      pred_train = pred_poly_train,
      pred_test = pred_poly_test
    )
    
    capture.output(
      summary(fit_poly),
      file = file.path(results_dir, paste0("summary_poly_", stopien, "_", suffix, ".txt"))
    )
  }
  
  # ----------------------------------------------------------
  # 5.3. Funkcje schodkowe
  # ----------------------------------------------------------
  
  for (liczba_przedzialow in c(4, 6, 8)) {
    dane_step_1 <- utworz_zmienne_schodkowe(
      train_data,
      test_data,
      "wind_speed_10m",
      liczba_przedzialow
    )
    
    train_step <- dane_step_1$train_data
    test_step <- dane_step_1$test_data
    zmienna_speed_step <- dane_step_1$zmienna_step
    
    dane_step_2 <- utworz_zmienne_schodkowe(
      train_step,
      test_step,
      "wind_gusts_10m",
      liczba_przedzialow
    )
    
    train_step <- dane_step_2$train_data
    test_step <- dane_step_2$test_data
    zmienna_gust_step <- dane_step_2$zmienna_step
    
    zmienne_step <- c(
      zmienna_speed_step,
      zmienna_gust_step,
      setdiff(predyktory, c("wind_speed_10m", "wind_gusts_10m"))
    )
    
    formula_step <- buduj_formule(target, zmienne_step)
    
    fit_step <- lm(formula_step, data = train_step)
    
    pred_step_train <- predict(fit_step, newdata = train_step)
    pred_step_test <- predict(fit_step, newdata = test_step)
    
    nazwa <- paste0("Funkcje schodkowe ", liczba_przedzialow)
    
    dodaj_model(
      model_name = nazwa,
      typ_modelu = "funkcja schodkowa",
      liczba_predyktorow = length(zmienne_step),
      parametry = paste0("liczba przedziałów = ", liczba_przedzialow),
      zmienne = zmienne_step,
      pred_train = pred_step_train,
      pred_test = pred_step_test
    )
    
    capture.output(
      summary(fit_step),
      file = file.path(results_dir, paste0("summary_step_", liczba_przedzialow, "_", suffix, ".txt"))
    )
  }
  
  # ----------------------------------------------------------
  # 5.4. Regresyjne funkcje sklejane bs()
  # ----------------------------------------------------------
  
  for (df_spline in c(4, 6, 8, 10)) {
    skladniki_bs <- c(
      paste0("bs(", zmienne_nieliniowe, ", df = ", df_spline, ")"),
      zmienne_liniowe
    )
    
    formula_bs <- buduj_formule(target, skladniki_bs)
    
    fit_bs <- lm(formula_bs, data = train_data)
    
    pred_bs_train <- predict(fit_bs, newdata = train_data)
    pred_bs_test <- predict(fit_bs, newdata = test_data)
    
    nazwa <- paste0("Funkcje sklejane bs df=", df_spline)
    
    dodaj_model(
      model_name = nazwa,
      typ_modelu = "funkcje sklejane bs",
      liczba_predyktorow = length(predyktory),
      parametry = paste0("df = ", df_spline),
      zmienne = predyktory,
      pred_train = pred_bs_train,
      pred_test = pred_bs_test
    )
    
    capture.output(
      summary(fit_bs),
      file = file.path(results_dir, paste0("summary_bs_df_", df_spline, "_", suffix, ".txt"))
    )
  }
  
  # ----------------------------------------------------------
  # 5.5. Naturalne funkcje sklejane ns()
  # ----------------------------------------------------------
  
  for (df_spline in c(4, 6, 8, 10)) {
    skladniki_ns <- c(
      paste0("ns(", zmienne_nieliniowe, ", df = ", df_spline, ")"),
      zmienne_liniowe
    )
    
    formula_ns <- buduj_formule(target, skladniki_ns)
    
    fit_ns <- lm(formula_ns, data = train_data)
    
    pred_ns_train <- predict(fit_ns, newdata = train_data)
    pred_ns_test <- predict(fit_ns, newdata = test_data)
    
    nazwa <- paste0("Naturalne funkcje sklejane ns df=", df_spline)
    
    dodaj_model(
      model_name = nazwa,
      typ_modelu = "naturalne funkcje sklejane",
      liczba_predyktorow = length(predyktory),
      parametry = paste0("df = ", df_spline),
      zmienne = predyktory,
      pred_train = pred_ns_train,
      pred_test = pred_ns_test
    )
    
    capture.output(
      summary(fit_ns),
      file = file.path(results_dir, paste0("summary_ns_df_", df_spline, "_", suffix, ".txt"))
    )
  }
  
  # ----------------------------------------------------------
  # 5.6. Wygładzające funkcje sklejane smooth.spline()
  # ----------------------------------------------------------
  
  smooth_data <- przygotuj_smooth_data(
    data = train_data,
    xvar = glowny_predyktor,
    yvar = target,
    n_max = 50000
  )
  
  for (df_smooth in c(6, 10, 15)) {
    fit_smooth <- smooth.spline(
      x = smooth_data$x,
      y = smooth_data$y,
      df = df_smooth
    )
    
    pred_smooth_train <- predict(
      fit_smooth,
      x = train_data[[glowny_predyktor]]
    )$y
    
    pred_smooth_test <- predict(
      fit_smooth,
      x = test_data[[glowny_predyktor]]
    )$y
    
    nazwa <- paste0("Smooth spline df=", df_smooth)
    
    dodaj_model(
      model_name = nazwa,
      typ_modelu = "wygładzająca funkcja sklejana",
      liczba_predyktorow = 1,
      parametry = paste0("df = ", df_smooth, ", predyktor = ", glowny_predyktor),
      zmienne = glowny_predyktor,
      pred_train = pred_smooth_train,
      pred_test = pred_smooth_test
    )
  }
  
  fit_smooth_cv <- smooth.spline(
    x = smooth_data$x,
    y = smooth_data$y,
    cv = TRUE
  )
  
  pred_smooth_cv_train <- predict(
    fit_smooth_cv,
    x = train_data[[glowny_predyktor]]
  )$y
  
  pred_smooth_cv_test <- predict(
    fit_smooth_cv,
    x = test_data[[glowny_predyktor]]
  )$y
  
  dodaj_model(
    model_name = "Smooth spline CV",
    typ_modelu = "wygładzająca funkcja sklejana",
    liczba_predyktorow = 1,
    parametry = paste0(
      "cv = TRUE, df = ",
      round(fit_smooth_cv$df, 4),
      ", predyktor = ",
      glowny_predyktor
    ),
    zmienne = glowny_predyktor,
    pred_train = pred_smooth_cv_train,
    pred_test = pred_smooth_cv_test
  )
  
  write.csv(
    data.frame(
      target = target,
      glowny_predyktor = glowny_predyktor,
      smooth_cv_df = fit_smooth_cv$df,
      smooth_cv_lambda = fit_smooth_cv$lambda,
      smooth_cv_cvcrit = fit_smooth_cv$cv.crit
    ),
    file.path(results_dir, paste0("smooth_spline_cv_parametry_", suffix, ".csv")),
    row.names = FALSE
  )
  
  # ----------------------------------------------------------
  # 5.7. Regresja lokalna loess()
  # ----------------------------------------------------------
  
  n_loess <- min(20000, nrow(train_data))
  id_loess <- sample(seq_len(nrow(train_data)), n_loess)
  
  train_loess <- train_data[id_loess, ]
  
  for (span_loess in c(0.2, 0.5, 0.8)) {
    fit_loess <- loess(
      as.formula(paste(target, "~", glowny_predyktor)),
      data = train_loess,
      span = span_loess,
      degree = 2,
      control = loess.control(surface = "interpolate")
    )
    
    pred_loess_train <- predict(
      fit_loess,
      newdata = train_data
    )
    
    pred_loess_test <- predict(
      fit_loess,
      newdata = test_data
    )
    
    pred_loess_train[is.na(pred_loess_train)] <- mean(y_train)
    pred_loess_test[is.na(pred_loess_test)] <- mean(y_train)
    
    nazwa <- paste0("Loess span=", span_loess)
    
    dodaj_model(
      model_name = nazwa,
      typ_modelu = "regresja lokalna",
      liczba_predyktorow = 1,
      parametry = paste0("span = ", span_loess, ", degree = 2, predyktor = ", glowny_predyktor),
      zmienne = glowny_predyktor,
      pred_train = pred_loess_train,
      pred_test = pred_loess_test
    )
  }
  
  # ----------------------------------------------------------
  # 5.8. Uogólnione modele addytywne GAM
  # ----------------------------------------------------------
  
  for (k_gam in c(5, 7, 9)) {
    skladniki_gam <- c(
      paste0("s(", zmienne_nieliniowe, ", k = ", k_gam, ")"),
      zmienne_liniowe
    )
    
    formula_gam <- buduj_formule(target, skladniki_gam)
    
    fit_gam <- mgcv::bam(
      formula_gam,
      data = train_data,
      method = "fREML",
      discrete = TRUE
    )
    
    pred_gam_train <- predict(fit_gam, newdata = train_data)
    pred_gam_test <- predict(fit_gam, newdata = test_data)
    
    nazwa <- paste0("GAM k=", k_gam)
    
    dodaj_model(
      model_name = nazwa,
      typ_modelu = "GAM",
      liczba_predyktorow = length(predyktory),
      parametry = paste0("k składników s() = ", k_gam),
      zmienne = predyktory,
      pred_train = pred_gam_train,
      pred_test = pred_gam_test
    )
    
    capture.output(
      summary(fit_gam),
      file = file.path(results_dir, paste0("summary_gam_k_", k_gam, "_", suffix, ".txt"))
    )
    
    zapisz_wykres(paste0("gam_partial_k_", k_gam, "_", suffix, ".png"), {
      plot(
        fit_gam,
        pages = 1,
        shade = TRUE,
        main = paste("GAM - efekty częściowe:", target)
      )
    })
  }
  
  # ----------------------------------------------------------
  # 5.9. Porównanie modeli
  # ----------------------------------------------------------
  
  porownanie_koncowe <- do.call(rbind, wyniki)
  porownanie_koncowe <- porownanie_koncowe[order(porownanie_koncowe$MSE_test), ]
  rownames(porownanie_koncowe) <- NULL
  
  write.csv(
    porownanie_koncowe,
    file.path(results_dir, paste0("porownanie_koncowe_nonlinear_", suffix, ".csv")),
    row.names = FALSE
  )
  
  najlepszy_model <- porownanie_koncowe[1, , drop = FALSE]
  
  write.csv(
    najlepszy_model,
    file.path(results_dir, paste0("najlepszy_model_nonlinear_", suffix, ".csv")),
    row.names = FALSE
  )
  
  prog_kompromisu <- najlepszy_model$MSE_test[1] * 1.01
  
  kandydaci_kompromis <- porownanie_koncowe %>%
    dplyr::filter(MSE_test <= prog_kompromisu) %>%
    dplyr::arrange(liczba_predyktorow, MSE_test)
  
  najlepszy_model_oszczedny <- kandydaci_kompromis[1, , drop = FALSE]
  
  write.csv(
    najlepszy_model_oszczedny,
    file.path(results_dir, paste0("najlepszy_model_oszczedny_nonlinear_", suffix, ".csv")),
    row.names = FALSE
  )
  
  # ----------------------------------------------------------
  # 5.10. Wykresy porównawcze
  # ----------------------------------------------------------
  
  zapisz_wykres(paste0("01_porownanie_RMSE_test_", suffix, ".png"), {
    par(mar = c(14, 5, 4, 2))
    
    barplot(
      porownanie_koncowe$RMSE_test,
      names.arg = porownanie_koncowe$model,
      las = 2,
      cex.names = 0.55,
      ylab = "RMSE test",
      main = paste("Porównanie RMSE test:", target)
    )
  })
  
  zapisz_wykres(paste0("02_porownanie_R2_test_", suffix, ".png"), {
    par(mar = c(14, 5, 4, 2))
    
    barplot(
      porownanie_koncowe$R2_test,
      names.arg = porownanie_koncowe$model,
      las = 2,
      cex.names = 0.55,
      ylab = "R2 test",
      main = paste("Porównanie R2 test:", target)
    )
    
    abline(h = 0, lty = 2)
  })
  
  najlepsza_nazwa <- najlepszy_model$model[1]
  pred_best_test <- predykcje_test[[najlepsza_nazwa]]
  
  zapisz_wykres(paste0("03_rzeczywiste_vs_pred_best_", suffix, ".png"), {
    plot(
      test_data[[target]],
      pred_best_test,
      main = paste("Najlepszy model: rzeczywiste vs przewidywane:", target),
      xlab = "Wartości rzeczywiste",
      ylab = "Wartości przewidywane"
    )
    
    abline(0, 1, lwd = 2)
  })
  
  zapisz_wykres(paste0("04_szereg_test_best_", suffix, ".png"), {
    n_plot <- min(500, nrow(test_data))
    
    plot(
      test_data$datetime[1:n_plot],
      test_data[[target]][1:n_plot],
      type = "l",
      main = paste("Najlepszy model: wartości rzeczywiste i przewidywane:", target),
      xlab = "Czas",
      ylab = target
    )
    
    lines(
      test_data$datetime[1:n_plot],
      pred_best_test[1:n_plot],
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
  
  # Krzywe nieliniowe względem głównego predyktora
  grid_info <- utworz_grid_predykcyjny(
    train_data = train_data,
    xvar = glowny_predyktor,
    predyktory = predyktory,
    n = 250
  )
  
  grid_x <- grid_info$grid
  grid_newdata <- grid_info$newdata
  
  fit_poly_3_plot <- lm(
    buduj_formule(
      target,
      c(paste0("poly(", zmienne_nieliniowe, ", 3)"), zmienne_liniowe)
    ),
    data = train_data
  )
  
  fit_bs_6_plot <- lm(
    buduj_formule(
      target,
      c(paste0("bs(", zmienne_nieliniowe, ", df = 6)"), zmienne_liniowe)
    ),
    data = train_data
  )
  
  fit_ns_6_plot <- lm(
    buduj_formule(
      target,
      c(paste0("ns(", zmienne_nieliniowe, ", df = 6)"), zmienne_liniowe)
    ),
    data = train_data
  )
  
  fit_gam_7_plot <- mgcv::bam(
    buduj_formule(
      target,
      c(paste0("s(", zmienne_nieliniowe, ", k = 7)"), zmienne_liniowe)
    ),
    data = train_data,
    method = "fREML",
    discrete = TRUE
  )
  
  pred_lm_grid <- predict(fit_lm_full, newdata = grid_newdata)
  pred_poly_grid <- predict(fit_poly_3_plot, newdata = grid_newdata)
  pred_bs_grid <- predict(fit_bs_6_plot, newdata = grid_newdata)
  pred_ns_grid <- predict(fit_ns_6_plot, newdata = grid_newdata)
  pred_gam_grid <- predict(fit_gam_7_plot, newdata = grid_newdata)
  pred_smooth_grid <- predict(fit_smooth_cv, x = grid_x)$y
  
  fit_loess_plot <- loess(
    as.formula(paste(target, "~", glowny_predyktor)),
    data = train_loess,
    span = 0.5,
    degree = 2,
    control = loess.control(surface = "interpolate")
  )
  
  pred_loess_grid <- predict(
    fit_loess_plot,
    newdata = data.frame(setNames(list(grid_x), glowny_predyktor))
  )
  
  zapisz_wykres(paste0("05_krzywe_nieliniowe_", suffix, ".png"), {
    n_scatter <- min(4000, nrow(train_data))
    id_scatter <- sample(seq_len(nrow(train_data)), n_scatter)
    
    plot(
      train_data[[glowny_predyktor]][id_scatter],
      train_data[[target]][id_scatter],
      pch = 16,
      col = rgb(0, 0, 0, 0.18),
      xlab = glowny_predyktor,
      ylab = target,
      main = paste("Porównanie krzywych nieliniowych:", target)
    )
    
    ord <- order(grid_x)
    
    lines(grid_x[ord], pred_lm_grid[ord], col = "black", lwd = 2)
    lines(grid_x[ord], pred_poly_grid[ord], col = "red", lwd = 2)
    lines(grid_x[ord], pred_bs_grid[ord], col = "blue", lwd = 2)
    lines(grid_x[ord], pred_ns_grid[ord], col = "darkgreen", lwd = 2)
    lines(grid_x[ord], pred_gam_grid[ord], col = "purple", lwd = 2)
    lines(grid_x[ord], pred_smooth_grid[ord], col = "orange", lwd = 2)
    
    if (!all(is.na(pred_loess_grid))) {
      lines(grid_x[ord], pred_loess_grid[ord], col = "brown", lwd = 2)
    }
    
    legend(
      "topleft",
      legend = c(
        "OLS",
        "Wielomian st. 3",
        "bs df=6",
        "ns df=6",
        "GAM k=7",
        "smooth.spline CV",
        "loess span=0.5"
      ),
      col = c(
        "black",
        "red",
        "blue",
        "darkgreen",
        "purple",
        "orange",
        "brown"
      ),
      lty = 1,
      lwd = 2,
      bty = "n",
      cex = 0.8
    )
  })
  
  # ----------------------------------------------------------
  # 5.11. Wyniki w konsoli
  # ----------------------------------------------------------
  
  cat("\n--- Opis danych ---\n")
  print(opis_danych)
  
  cat("\n--- Najlepszy model według MSE test ---\n")
  print(najlepszy_model)
  
  cat("\n--- Najlepszy model oszczędny w granicy 1% od najlepszego MSE test ---\n")
  print(najlepszy_model_oszczedny)
  
  cat("\n--- Porównanie końcowe ---\n")
  print(porownanie_koncowe)
  
  list(
    target = target,
    opis_danych = opis_danych,
    porownanie_koncowe = porownanie_koncowe,
    najlepszy_model = najlepszy_model,
    najlepszy_model_oszczedny = najlepszy_model_oszczedny
  )
}

# ------------------------------------------------------------
# 6. Uruchomienie analizy dla obu zmiennych zależnych
# ------------------------------------------------------------

wyniki_lista <- lapply(targety, analizuj_target)

# ------------------------------------------------------------
# 7. Zbiorcze tabele dla obu zmiennych
# ------------------------------------------------------------

zbiorcze_opis_danych <- do.call(
  rbind,
  lapply(wyniki_lista, function(x) x$opis_danych)
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

rownames(zbiorcze_porownanie) <- NULL
rownames(zbiorcze_najlepsze) <- NULL
rownames(zbiorcze_najlepsze_oszczedne) <- NULL

write.csv(
  zbiorcze_opis_danych,
  file.path(results_dir, "zbiorcze_opis_danych.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_porownanie,
  file.path(results_dir, "zbiorcze_porownanie_koncowe_nonlinear.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_najlepsze,
  file.path(results_dir, "zbiorcze_najlepsze_modele_nonlinear.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_najlepsze_oszczedne,
  file.path(results_dir, "zbiorcze_najlepsze_modele_oszczedne_nonlinear.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 8. Komunikat końcowy
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ZAKOŃCZONO ANALIZĘ MODELI NIELINIOWYCH\n")
cat("Predykcja dotyczy wartości w następnej godzinie: t + 1.\n")
cat("Wykresy zapisano w folderze:", plot_dir, "\n")
cat("Tabele zapisano w folderze:", results_dir, "\n")
cat("============================================================\n")

cat("\nZbiorcze najlepsze modele:\n")
print(zbiorcze_najlepsze)

cat("\nZbiorcze najlepsze modele oszczędne:\n")
print(zbiorcze_najlepsze_oszczedne)

cat("\nZbiorcze porównanie końcowe:\n")
print(zbiorcze_porownanie)