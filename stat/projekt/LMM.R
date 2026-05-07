# ============================================================
# MODELE LINIOWE Z EFEKTAMI MIESZANYMI
# PREDYKCJA PRĘDKOŚCI WIATRU I PORYWÓW W NASTĘPNEJ GODZINIE
# ============================================================

# ------------------------------------------------------------
# 0. Pakiety i ustawienia
# ------------------------------------------------------------

library(dplyr)
library(lubridate)
library(lme4)
library(ggplot2)

plot_dir <- "plots_lmm"
results_dir <- "wyniki_lmm"

dir.create(plot_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)

set.seed(2025)

# ------------------------------------------------------------
# 0.1. Funkcje pomocnicze
# ------------------------------------------------------------

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

zapisz_gg <- function(nazwa, wykres, szerokosc = 10, wysokosc = 6) {
  ggsave(
    filename = file.path(plot_dir, nazwa),
    plot = wykres,
    width = szerokosc,
    height = wysokosc,
    dpi = 140
  )
}

bezpieczna_nazwa <- function(x) {
  x <- gsub("ą", "a", x)
  x <- gsub("ć", "c", x)
  x <- gsub("ę", "e", x)
  x <- gsub("ł", "l", x)
  x <- gsub("ń", "n", x)
  x <- gsub("ó", "o", x)
  x <- gsub("ś", "s", x)
  x <- gsub("ż", "z", x)
  x <- gsub("ź", "z", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

formatuj_liczbe <- function(x, digits = 6) {
  ifelse(
    is.na(x),
    "nie obliczono",
    format(round(x, digits), nsmall = digits, trim = TRUE)
  )
}

# ------------------------------------------------------------
# 0.2. Funkcje dla modeli LMM
# ------------------------------------------------------------

oblicz_udzial_wariancji_losowej <- function(fit) {
  vc <- as.data.frame(VarCorr(fit))
  
  var_losowa <- sum(vc$vcov[vc$grp != "Residual"], na.rm = TRUE)
  var_resztowa <- vc$vcov[vc$grp == "Residual"][1]
  
  if (is.na(var_resztowa) || (var_losowa + var_resztowa) == 0) {
    return(NA_real_)
  }
  
  var_losowa / (var_losowa + var_resztowa)
}

oblicz_r2_lmm <- function(fit, data_model) {
  pred_fixed <- predict(
    fit,
    newdata = data_model,
    re.form = NA,
    allow.new.levels = TRUE
  )
  
  vc <- as.data.frame(VarCorr(fit))
  
  var_fixed <- var(as.numeric(pred_fixed), na.rm = TRUE)
  var_random <- sum(vc$vcov[vc$grp != "Residual"], na.rm = TRUE)
  var_residual <- sigma(fit)^2
  
  total <- var_fixed + var_random + var_residual
  
  if (is.na(total) || total == 0) {
    return(data.frame(
      R2_marginal = NA_real_,
      R2_conditional = NA_real_
    ))
  }
  
  data.frame(
    R2_marginal = var_fixed / total,
    R2_conditional = (var_fixed + var_random) / total
  )
}

wynik_modelu <- function(
    target,
    model_name,
    typ_modelu,
    efekty_losowe,
    liczba_predyktorow_stalych,
    fit,
    y_train,
    pred_train,
    y_test,
    pred_test,
    train_data,
    model_mieszany
) {
  met_train <- metryki_regresji(y_train, pred_train)
  met_test <- metryki_regresji(y_test, pred_test)
  
  if (model_mieszany) {
    udzial_wariancji_losowej <- oblicz_udzial_wariancji_losowej(fit)
    r2_lmm <- oblicz_r2_lmm(fit, train_data)
    singular <- ifelse(isSingular(fit), "tak", "nie")
  } else {
    udzial_wariancji_losowej <- NA_real_
    r2_lmm <- data.frame(
      R2_marginal = NA_real_,
      R2_conditional = NA_real_
    )
    singular <- "nie dotyczy"
  }
  
  data.frame(
    target = target,
    model = model_name,
    typ_modelu = typ_modelu,
    efekty_losowe = efekty_losowe,
    liczba_predyktorow_stalych = liczba_predyktorow_stalych,
    AIC = AIC(fit),
    BIC = BIC(fit),
    logLik = as.numeric(logLik(fit)),
    singular = singular,
    udzial_wariancji_losowej = ifelse(
      model_mieszany,
      formatuj_liczbe(udzial_wariancji_losowej),
      "nie dotyczy"
    ),
    R2_marginal_LMM = ifelse(
      model_mieszany,
      formatuj_liczbe(r2_lmm$R2_marginal),
      "nie dotyczy"
    ),
    R2_conditional_LMM = ifelse(
      model_mieszany,
      formatuj_liczbe(r2_lmm$R2_conditional),
      "nie dotyczy"
    ),
    MSE_train = met_train$MSE,
    RMSE_train = met_train$RMSE,
    MAE_train = met_train$MAE,
    R2_train = met_train$R2,
    MSE_test = met_test$MSE,
    RMSE_test = met_test$RMSE,
    MAE_test = met_test$MAE,
    R2_test = met_test$R2
  )
}

zapisz_parametry_lm <- function(fit, target, model_name) {
  s <- coef(summary(fit))
  
  tab <- data.frame(
    target = target,
    model = model_name,
    parametr = rownames(s),
    estimate = s[, "Estimate"],
    std_error = s[, "Std. Error"],
    t_value = s[, "t value"],
    p_value = s[, "Pr(>|t|)"]
  )
  
  write.csv(
    tab,
    file.path(
      results_dir,
      paste0("parametry_", bezpieczna_nazwa(model_name), "_", target, ".csv")
    ),
    row.names = FALSE
  )
}

zapisz_parametry_lmm <- function(fit, target, model_name) {
  fe <- fixef(fit)
  
  tab_fe <- data.frame(
    target = target,
    model = model_name,
    parametr = names(fe),
    estimate = as.numeric(fe)
  )
  
  write.csv(
    tab_fe,
    file.path(
      results_dir,
      paste0("efekty_stale_", bezpieczna_nazwa(model_name), "_", target, ".csv")
    ),
    row.names = FALSE
  )
  
  vc <- as.data.frame(VarCorr(fit))
  vc$target <- target
  vc$model <- model_name
  
  write.csv(
    vc,
    file.path(
      results_dir,
      paste0("skladowe_wariancji_", bezpieczna_nazwa(model_name), "_", target, ".csv")
    ),
    row.names = FALSE
  )
  
  re <- ranef(fit)
  
  for (nazwa_grupy in names(re)) {
    tab_re <- data.frame(
      poziom = rownames(re[[nazwa_grupy]]),
      re[[nazwa_grupy]]
    )
    
    tab_re$target <- target
    tab_re$model <- model_name
    tab_re$grupa <- nazwa_grupy
    
    write.csv(
      tab_re,
      file.path(
        results_dir,
        paste0(
          "efekty_losowe_",
          bezpieczna_nazwa(model_name),
          "_",
          nazwa_grupy,
          "_",
          target,
          ".csv"
        )
      ),
      row.names = FALSE
    )
  }
}

# Zapis macierzy korelacji estymatorów parametrów.
# To jest macierz, której dotyczył komunikat:
# "Correlation matrix not shown by default, as p = 20 > 12".
zapisz_korelacje_estymatorow <- function(fit, target, model_name) {
  macierz <- tryCatch(
    cov2cor(as.matrix(vcov(fit))),
    error = function(e) NULL
  )
  
  if (is.null(macierz)) {
    write.csv(
      data.frame(
        target = target,
        model = model_name,
        komunikat = "Nie udało się obliczyć macierzy korelacji estymatorów."
      ),
      file.path(
        results_dir,
        paste0(
          "korelacje_estymatorow_",
          bezpieczna_nazwa(model_name),
          "_",
          target,
          ".csv"
        )
      ),
      row.names = FALSE
    )
    
    return(invisible(NULL))
  }
  
  tab <- data.frame(
    parametr = rownames(macierz),
    macierz,
    check.names = FALSE
  )
  
  write.csv(
    tab,
    file.path(
      results_dir,
      paste0(
        "korelacje_estymatorow_",
        bezpieczna_nazwa(model_name),
        "_",
        target,
        ".csv"
      )
    ),
    row.names = FALSE
  )
}

skaluj_predyktory <- function(train_data, test_data, predyktory) {
  parametry_skalowania <- data.frame()
  predyktory_z <- character(0)
  
  for (zm in predyktory) {
    srednia <- mean(train_data[[zm]], na.rm = TRUE)
    odchylenie <- sd(train_data[[zm]], na.rm = TRUE)
    
    if (is.na(odchylenie) || odchylenie == 0) {
      odchylenie <- 1
    }
    
    nowa_nazwa <- paste0(zm, "_z")
    
    train_data[[nowa_nazwa]] <- (train_data[[zm]] - srednia) / odchylenie
    test_data[[nowa_nazwa]] <- (test_data[[zm]] - srednia) / odchylenie
    
    predyktory_z <- c(predyktory_z, nowa_nazwa)
    
    parametry_skalowania <- rbind(
      parametry_skalowania,
      data.frame(
        zmienna = zm,
        zmienna_skalowana = nowa_nazwa,
        srednia_train = srednia,
        sd_train = odchylenie
      )
    )
  }
  
  list(
    train_data = train_data,
    test_data = test_data,
    predyktory_z = predyktory_z,
    parametry_skalowania = parametry_skalowania
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

# Sezonowość roczna i dobowa.
dane$sin_dzien <- sin(2 * pi * dane$dzien_roku / 365)
dane$cos_dzien <- cos(2 * pi * dane$dzien_roku / 365)

dane$sin_godzina <- sin(2 * pi * dane$godzina / 24)
dane$cos_godzina <- cos(2 * pi * dane$godzina / 24)

# Kierunek wiatru jako zmienna cykliczna.
dane$wind_dir_sin <- sin(2 * pi * dane$wind_direction_10m / 360)
dane$wind_dir_cos <- cos(2 * pi * dane$wind_direction_10m / 360)

# Zmienna sezonowa pomocnicza.
dane$polrocze_cieple <- ifelse(
  dane$miesiac %in% c(4, 5, 6, 7, 8, 9),
  1,
  0
)

# Zmienne grupujące dla efektów losowych.
dane$miesiac_f <- factor(dane$miesiac, levels = 1:12)
dane$dzien_roku_f <- factor(dane$dzien_roku, levels = 1:366)

dane$sezon_f <- dplyr::case_when(
  dane$miesiac %in% c(12, 1, 2) ~ "zima",
  dane$miesiac %in% c(3, 4, 5) ~ "wiosna",
  dane$miesiac %in% c(6, 7, 8) ~ "lato",
  TRUE ~ "jesien"
)

dane$sezon_f <- factor(
  dane$sezon_f,
  levels = c("zima", "wiosna", "lato", "jesien")
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
      miesiac_f,
      dzien_roku_f,
      sezon_f,
      all_of(target),
      all_of(predyktory)
    ) %>%
    dplyr::filter(complete.cases(.))
  
  train_data <- dane_model %>%
    dplyr::filter(data < as.Date("2025-01-01"))
  
  test_data <- dane_model %>%
    dplyr::filter(data >= as.Date("2025-01-01"))
  
  skalowanie <- skaluj_predyktory(
    train_data = train_data,
    test_data = test_data,
    predyktory = predyktory
  )
  
  train_data <- skalowanie$train_data
  test_data <- skalowanie$test_data
  predyktory_z <- skalowanie$predyktory_z
  
  write.csv(
    skalowanie$parametry_skalowania,
    file.path(results_dir, paste0("parametry_skalowania_", suffix, ".csv")),
    row.names = FALSE
  )
  
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
  
  if (target == "wind_speed_10m_next") {
    glowny_predyktor_z <- "wind_speed_10m_z"
    glowny_predyktor_raw <- "wind_speed_10m"
  } else {
    glowny_predyktor_z <- "wind_gusts_10m_z"
    glowny_predyktor_raw <- "wind_gusts_10m"
  }
  
  fixed_full <- paste(predyktory_z, collapse = " + ")
  
  wyniki <- list()
  modele <- list()
  predykcje_test <- list()
  status_modeli <- data.frame()
  
  control_lmer <- lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
  
  # ----------------------------------------------------------
  # 4.1. Model zerowy
  # ----------------------------------------------------------
  
  pred_null_train <- rep(mean(y_train), length(y_train))
  pred_null_test <- rep(mean(y_train), length(y_test))
  
  wynik_null <- data.frame(
    target = target,
    model = "Model zerowy",
    typ_modelu = "bazowy",
    efekty_losowe = "brak",
    liczba_predyktorow_stalych = 0,
    AIC = "nie dotyczy",
    BIC = "nie dotyczy",
    logLik = "nie dotyczy",
    singular = "nie dotyczy",
    udzial_wariancji_losowej = "nie dotyczy",
    R2_marginal_LMM = "nie dotyczy",
    R2_conditional_LMM = "nie dotyczy",
    MSE_train = metryki_regresji(y_train, pred_null_train)$MSE,
    RMSE_train = metryki_regresji(y_train, pred_null_train)$RMSE,
    MAE_train = metryki_regresji(y_train, pred_null_train)$MAE,
    R2_train = metryki_regresji(y_train, pred_null_train)$R2,
    MSE_test = metryki_regresji(y_test, pred_null_test)$MSE,
    RMSE_test = metryki_regresji(y_test, pred_null_test)$RMSE,
    MAE_test = metryki_regresji(y_test, pred_null_test)$MAE,
    R2_test = metryki_regresji(y_test, pred_null_test)$R2
  )
  
  wyniki[["Model zerowy"]] <- wynik_null
  predykcje_test[["Model zerowy"]] <- pred_null_test
  
  # ----------------------------------------------------------
  # 4.2. Model liniowy bez efektów losowych
  # ----------------------------------------------------------
  
  formula_lm <- as.formula(
    paste(target, "~", fixed_full)
  )
  
  fit_lm <- lm(formula_lm, data = train_data)
  
  pred_lm_train <- predict(fit_lm, newdata = train_data)
  pred_lm_test <- predict(fit_lm, newdata = test_data)
  
  wynik_lm <- wynik_modelu(
    target = target,
    model_name = "OLS full",
    typ_modelu = "LM",
    efekty_losowe = "brak",
    liczba_predyktorow_stalych = length(predyktory_z),
    fit = fit_lm,
    y_train = y_train,
    pred_train = pred_lm_train,
    y_test = y_test,
    pred_test = pred_lm_test,
    train_data = train_data,
    model_mieszany = FALSE
  )
  
  wyniki[["OLS full"]] <- wynik_lm
  modele[["OLS full"]] <- fit_lm
  predykcje_test[["OLS full"]] <- pred_lm_test
  
  zapisz_parametry_lm(fit_lm, target, "OLS full")
  zapisz_korelacje_estymatorow(fit_lm, target, "OLS full")
  
  capture.output(
    print(summary(fit_lm, correlation = TRUE)),
    file = file.path(results_dir, paste0("summary_OLS_full_", suffix, ".txt"))
  )
  
  # ----------------------------------------------------------
  # 4.3. Specyfikacje modeli LMM
  # ----------------------------------------------------------
  
  specyfikacje_lmm <- list(
    list(
      model = "LMM RI miesiac bez predyktorow",
      typ = "RI",
      efekty = "(1 | miesiac)",
      liczba_pred = 0,
      formula = as.formula(
        paste(target, "~ 1 + (1 | miesiac_f)")
      )
    ),
    list(
      model = "LMM RI dzien_roku bez predyktorow",
      typ = "RI",
      efekty = "(1 | dzien_roku)",
      liczba_pred = 0,
      formula = as.formula(
        paste(target, "~ 1 + (1 | dzien_roku_f)")
      )
    ),
    list(
      model = "LMM RIFS sezon",
      typ = "RIFS",
      efekty = "(1 | sezon)",
      liczba_pred = length(predyktory_z),
      formula = as.formula(
        paste(target, "~", fixed_full, "+ (1 | sezon_f)")
      )
    ),
    list(
      model = "LMM RIFS miesiac",
      typ = "RIFS",
      efekty = "(1 | miesiac)",
      liczba_pred = length(predyktory_z),
      formula = as.formula(
        paste(target, "~", fixed_full, "+ (1 | miesiac_f)")
      )
    ),
    list(
      model = "LMM RIFS dzien_roku",
      typ = "RIFS",
      efekty = "(1 | dzien_roku)",
      liczba_pred = length(predyktory_z),
      formula = as.formula(
        paste(target, "~", fixed_full, "+ (1 | dzien_roku_f)")
      )
    ),
    list(
      model = "LMM RIFS miesiac plus dzien_roku",
      typ = "RIFS",
      efekty = "(1 | miesiac) + (1 | dzien_roku)",
      liczba_pred = length(predyktory_z),
      formula = as.formula(
        paste(
          target,
          "~",
          fixed_full,
          "+ (1 | miesiac_f) + (1 | dzien_roku_f)"
        )
      )
    ),
    list(
      model = "LMM FIRS miesiac",
      typ = "FIRS",
      efekty = paste0("(0 + ", glowny_predyktor_z, " | miesiac)"),
      liczba_pred = length(predyktory_z),
      formula = as.formula(
        paste(
          target,
          "~",
          fixed_full,
          "+ (0 +",
          glowny_predyktor_z,
          "| miesiac_f)"
        )
      )
    ),
    list(
      model = "LMM RSI miesiac",
      typ = "RSI",
      efekty = paste0("(1 + ", glowny_predyktor_z, " | miesiac)"),
      liczba_pred = length(predyktory_z),
      formula = as.formula(
        paste(
          target,
          "~",
          fixed_full,
          "+ (1 +",
          glowny_predyktor_z,
          "| miesiac_f)"
        )
      )
    ),
    list(
      model = "LMM RSI-i miesiac",
      typ = "RSI-i",
      efekty = paste0("(1 + ", glowny_predyktor_z, " || miesiac)"),
      liczba_pred = length(predyktory_z),
      formula = as.formula(
        paste(
          target,
          "~",
          fixed_full,
          "+ (1 +",
          glowny_predyktor_z,
          "|| miesiac_f)"
        )
      )
    )
  )
  
  # ----------------------------------------------------------
  # 4.4. Dopasowanie modeli LMM
  # ----------------------------------------------------------
  
  for (spec in specyfikacje_lmm) {
    
    cat("\nDopasowanie modelu:", spec$model, "\n")
    
    fit <- tryCatch(
      lmer(
        spec$formula,
        data = train_data,
        REML = FALSE,
        control = control_lmer
      ),
      error = function(e) e
    )
    
    if (inherits(fit, "error")) {
      status_modeli <- rbind(
        status_modeli,
        data.frame(
          target = target,
          model = spec$model,
          status = "blad",
          komunikat = fit$message
        )
      )
      
      next
    }
    
    pred_train <- predict(
      fit,
      newdata = train_data,
      allow.new.levels = TRUE
    )
    
    pred_test <- predict(
      fit,
      newdata = test_data,
      allow.new.levels = TRUE
    )
    
    wynik <- wynik_modelu(
      target = target,
      model_name = spec$model,
      typ_modelu = spec$typ,
      efekty_losowe = spec$efekty,
      liczba_predyktorow_stalych = spec$liczba_pred,
      fit = fit,
      y_train = y_train,
      pred_train = pred_train,
      y_test = y_test,
      pred_test = pred_test,
      train_data = train_data,
      model_mieszany = TRUE
    )
    
    wyniki[[spec$model]] <- wynik
    modele[[spec$model]] <- fit
    predykcje_test[[spec$model]] <- pred_test
    
    status_modeli <- rbind(
      status_modeli,
      data.frame(
        target = target,
        model = spec$model,
        status = "dopasowano",
        komunikat = ifelse(isSingular(fit), "model osobliwy", "brak")
      )
    )
    
    zapisz_parametry_lmm(fit, target, spec$model)
    zapisz_korelacje_estymatorow(fit, target, spec$model)
    
    capture.output(
      print(summary(fit, correlation = TRUE), correlation = TRUE),
      file = file.path(
        results_dir,
        paste0("summary_", bezpieczna_nazwa(spec$model), "_", suffix, ".txt")
      )
    )
  }
  
  write.csv(
    status_modeli,
    file.path(results_dir, paste0("status_modeli_lmm_", suffix, ".csv")),
    row.names = FALSE
  )
  
  # ----------------------------------------------------------
  # 4.5. Porównanie modeli
  # ----------------------------------------------------------
  
  porownanie_koncowe <- do.call(rbind, wyniki)
  porownanie_koncowe <- porownanie_koncowe[order(porownanie_koncowe$MSE_test), ]
  rownames(porownanie_koncowe) <- NULL
  
  write.csv(
    porownanie_koncowe,
    file.path(results_dir, paste0("porownanie_koncowe_lmm_", suffix, ".csv")),
    row.names = FALSE
  )
  
  najlepszy_model <- porownanie_koncowe[1, , drop = FALSE]
  
  write.csv(
    najlepszy_model,
    file.path(results_dir, paste0("najlepszy_model_lmm_", suffix, ".csv")),
    row.names = FALSE
  )
  
  prog_kompromisu <- najlepszy_model$MSE_test[1] * 1.01
  
  kandydaci_kompromis <- porownanie_koncowe %>%
    dplyr::filter(MSE_test <= prog_kompromisu) %>%
    dplyr::arrange(liczba_predyktorow_stalych, MSE_test)
  
  najlepszy_model_oszczedny <- kandydaci_kompromis[1, , drop = FALSE]
  
  write.csv(
    najlepszy_model_oszczedny,
    file.path(results_dir, paste0("najlepszy_model_oszczedny_lmm_", suffix, ".csv")),
    row.names = FALSE
  )
  
  # ----------------------------------------------------------
  # 4.6. Wykresy porównawcze
  # ----------------------------------------------------------
  
  wykres_rmse <- ggplot(
    porownanie_koncowe,
    aes(x = reorder(model, RMSE_test), y = RMSE_test)
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste("Porównanie RMSE test:", target),
      x = "Model",
      y = "RMSE test"
    ) +
    theme_minimal()
  
  zapisz_gg(
    paste0("01_porownanie_RMSE_test_lmm_", suffix, ".png"),
    wykres_rmse,
    szerokosc = 11,
    wysokosc = 7
  )
  
  wykres_r2 <- ggplot(
    porownanie_koncowe,
    aes(x = reorder(model, R2_test), y = R2_test)
  ) +
    geom_col() +
    coord_flip() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = paste("Porównanie R2 test:", target),
      x = "Model",
      y = "R2 test"
    ) +
    theme_minimal()
  
  zapisz_gg(
    paste0("02_porownanie_R2_test_lmm_", suffix, ".png"),
    wykres_r2,
    szerokosc = 11,
    wysokosc = 7
  )
  
  najlepsza_nazwa <- najlepszy_model$model[1]
  pred_best_test <- predykcje_test[[najlepsza_nazwa]]
  
  zapisz_wykres(paste0("03_rzeczywiste_vs_pred_best_lmm_", suffix, ".png"), {
    plot(
      y_test,
      pred_best_test,
      main = paste("Najlepszy LMM: rzeczywiste vs przewidywane:", target),
      xlab = "Wartości rzeczywiste",
      ylab = "Wartości przewidywane"
    )
    abline(0, 1, lwd = 2)
  })
  
  zapisz_wykres(paste0("04_szereg_test_best_lmm_", suffix, ".png"), {
    n_plot <- min(500, nrow(test_data))
    
    plot(
      test_data$datetime[1:n_plot],
      y_test[1:n_plot],
      type = "l",
      main = paste("Najlepszy LMM: wartości rzeczywiste i przewidywane:", target),
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
  
  # ----------------------------------------------------------
  # 4.7. Wykresy efektów losowych
  # ----------------------------------------------------------
  
  if ("LMM RIFS miesiac" %in% names(modele)) {
    fit_miesiac <- modele[["LMM RIFS miesiac"]]
    re_miesiac <- ranef(fit_miesiac)$miesiac_f
    
    tab_re_miesiac <- data.frame(
      miesiac = rownames(re_miesiac),
      efekt_losowy = re_miesiac[, "(Intercept)"]
    )
    
    wykres_re_miesiac <- ggplot(
      tab_re_miesiac,
      aes(x = reorder(miesiac, efekt_losowy), y = efekt_losowy)
    ) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste("Losowe wyrazy wolne dla miesięcy:", target),
        x = "Miesiąc",
        y = "Efekt losowy"
      ) +
      theme_minimal()
    
    zapisz_gg(
      paste0("05_efekty_losowe_miesiac_lmm_", suffix, ".png"),
      wykres_re_miesiac,
      szerokosc = 8,
      wysokosc = 5
    )
  }
  
  if ("LMM RSI-i miesiac" %in% names(modele)) {
    fit_rsii <- modele[["LMM RSI-i miesiac"]]
    re_rsii <- ranef(fit_rsii)$miesiac_f
    
    tab_re_rsii <- data.frame(
      miesiac = rownames(re_rsii),
      re_rsii
    )
    
    nazwy_kolumn <- colnames(tab_re_rsii)
    kolumna_slope <- nazwy_kolumn[grepl(glowny_predyktor_z, nazwy_kolumn)]
    
    if (length(kolumna_slope) > 0) {
      wykres_slope <- ggplot(
        tab_re_rsii,
        aes(x = reorder(miesiac, .data[[kolumna_slope[1]]]), y = .data[[kolumna_slope[1]]])
      ) +
        geom_col() +
        coord_flip() +
        labs(
          title = paste("Losowe nachylenia dla miesięcy:", target),
          x = "Miesiąc",
          y = paste("Efekt losowy dla", glowny_predyktor_raw)
        ) +
        theme_minimal()
      
      zapisz_gg(
        paste0("06_efekty_losowe_nachylenia_miesiac_lmm_", suffix, ".png"),
        wykres_slope,
        szerokosc = 8,
        wysokosc = 5
      )
    }
  }
  
  # ----------------------------------------------------------
  # 4.8. Wykres zależności w grupach miesięcznych
  # ----------------------------------------------------------
  
  n_scatter <- min(6000, nrow(train_data))
  id_scatter <- sample(seq_len(nrow(train_data)), n_scatter)
  
  train_scatter <- train_data[id_scatter, ]
  
  if (najlepsza_nazwa %in% names(modele)) {
    fit_best <- modele[[najlepsza_nazwa]]
    
    train_scatter$pred_best <- predict(
      fit_best,
      newdata = train_scatter,
      allow.new.levels = TRUE
    )
  } else {
    train_scatter$pred_best <- predict(
      fit_lm,
      newdata = train_scatter
    )
  }
  
  wykres_grupy <- ggplot(
    train_scatter,
    aes(x = .data[[glowny_predyktor_raw]], y = .data[[target]])
  ) +
    geom_point(alpha = 0.25) +
    geom_point(aes(y = pred_best), alpha = 0.25) +
    facet_wrap(~ miesiac_f) +
    labs(
      title = paste("Dopasowanie najlepszego modelu według miesięcy:", target),
      x = glowny_predyktor_raw,
      y = target
    ) +
    theme_minimal()
  
  zapisz_gg(
    paste0("07_dopasowanie_wedlug_miesiecy_lmm_", suffix, ".png"),
    wykres_grupy,
    szerokosc = 12,
    wysokosc = 8
  )
  
  # ----------------------------------------------------------
  # 4.9. Wyniki w konsoli
  # ----------------------------------------------------------
  
  cat("\n--- Opis danych ---\n")
  print(opis_danych)
  
  cat("\n--- Status modeli ---\n")
  print(status_modeli)
  
  cat("\n--- Najlepszy model według MSE test ---\n")
  print(najlepszy_model)
  
  cat("\n--- Najlepszy model oszczędny w granicy 1% od najlepszego MSE test ---\n")
  print(najlepszy_model_oszczedny)
  
  cat("\n--- Porównanie końcowe ---\n")
  print(porownanie_koncowe)
  
  list(
    target = target,
    opis_danych = opis_danych,
    status_modeli = status_modeli,
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

zbiorcze_status_modeli <- do.call(
  rbind,
  lapply(wyniki_lista, function(x) x$status_modeli)
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
  file.path(results_dir, "zbiorcze_opis_danych_lmm.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_status_modeli,
  file.path(results_dir, "zbiorcze_status_modeli_lmm.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_porownanie,
  file.path(results_dir, "zbiorcze_porownanie_koncowe_lmm.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_najlepsze,
  file.path(results_dir, "zbiorcze_najlepsze_modele_lmm.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_najlepsze_oszczedne,
  file.path(results_dir, "zbiorcze_najlepsze_modele_oszczedne_lmm.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 7. Komunikat końcowy
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ZAKOŃCZONO ANALIZĘ MODELI LMM\n")
cat("Predykcja dotyczy wartości w następnej godzinie: t + 1.\n")
cat("Wykresy zapisano w folderze:", plot_dir, "\n")
cat("Tabele zapisano w folderze:", results_dir, "\n")
cat("Macierz korelacji predyktorów zapisano jako: macierz_korelacji_predyktorow.csv\n")
cat("Macierze korelacji estymatorów zapisano jako pliki: korelacje_estymatorow_*.csv\n")
cat("Pełne summary modeli z korelacjami zapisano jako pliki: summary_*.txt\n")
cat("============================================================\n")

cat("\nZbiorcze najlepsze modele:\n")
print(zbiorcze_najlepsze)

cat("\nZbiorcze najlepsze modele oszczędne:\n")
print(zbiorcze_najlepsze_oszczedne)

cat("\nZbiorcze porównanie końcowe:\n")
print(zbiorcze_porownanie)