# ============================================================
# UOGÓLNIONE MODELE LINIOWE Z EFEKTAMI MIESZANYMI
# KLASYFIKACJA WARUNKÓW WIATROWYCH W NASTĘPNEJ GODZINIE
# ============================================================

# ------------------------------------------------------------
# 0. Pakiety i ustawienia
# ------------------------------------------------------------

library(dplyr)
library(lubridate)
library(lme4)
library(ggplot2)

plot_dir <- "plots_glmm"
results_dir <- "wyniki_glmm"

dir.create(plot_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)

set.seed(2025)

# ------------------------------------------------------------
# 0.1. Funkcje pomocnicze
# ------------------------------------------------------------

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

log_loss <- function(y, p) {
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE)
}

brier_score <- function(y, p) {
  mean((y - p)^2, na.rm = TRUE)
}

auc_roc <- function(y, p) {
  ok <- complete.cases(y, p)
  y <- y[ok]
  p <- p[ok]
  
  n1 <- sum(y == 1)
  n0 <- sum(y == 0)
  
  if (n1 == 0 || n0 == 0) {
    return(NA_real_)
  }
  
  r <- rank(p, ties.method = "average")
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

metryki_klasyfikacji <- function(y, p, prog = 0.5) {
  pred <- ifelse(p >= prog, 1, 0)
  
  TP <- sum(pred == 1 & y == 1, na.rm = TRUE)
  TN <- sum(pred == 0 & y == 0, na.rm = TRUE)
  FP <- sum(pred == 1 & y == 0, na.rm = TRUE)
  FN <- sum(pred == 0 & y == 1, na.rm = TRUE)
  
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  sensitivity <- ifelse((TP + FN) == 0, NA_real_, TP / (TP + FN))
  specificity <- ifelse((TN + FP) == 0, NA_real_, TN / (TN + FP))
  precision <- ifelse((TP + FP) == 0, NA_real_, TP / (TP + FP))
  f1 <- ifelse(
    is.na(precision) | is.na(sensitivity) | (precision + sensitivity) == 0,
    NA_real_,
    2 * precision * sensitivity / (precision + sensitivity)
  )
  
  data.frame(
    threshold = prog,
    accuracy = accuracy,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    F1 = f1,
    TP = TP,
    TN = TN,
    FP = FP,
    FN = FN
  )
}

wybierz_prog_youden <- function(y, p) {
  progi <- seq(0.05, 0.95, by = 0.01)
  
  wyniki <- lapply(progi, function(pr) {
    met <- metryki_klasyfikacji(y, p, prog = pr)
    data.frame(
      threshold = pr,
      youden = met$sensitivity + met$specificity - 1
    )
  })
  
  wyniki <- do.call(rbind, wyniki)
  
  if (all(is.na(wyniki$youden))) {
    return(0.5)
  }
  
  wyniki$threshold[which.max(wyniki$youden)]
}

dane_roc <- function(y, p) {
  ok <- complete.cases(y, p)
  y <- y[ok]
  p <- p[ok]
  
  tab <- data.frame(y = y, p = p)
  tab <- tab %>%
    arrange(desc(p))
  
  n1 <- sum(tab$y == 1)
  n0 <- sum(tab$y == 0)
  
  if (n1 == 0 || n0 == 0) {
    return(data.frame(FPR = NA_real_, TPR = NA_real_))
  }
  
  tab$TP <- cumsum(tab$y == 1)
  tab$FP <- cumsum(tab$y == 0)
  
  data.frame(
    FPR = tab$FP / n0,
    TPR = tab$TP / n1
  )
}

wynik_modelu_klasyfikacji <- function(
    target,
    model_name,
    typ_modelu,
    efekty_losowe,
    liczba_predyktorow_stalych,
    fit,
    model_mieszany,
    y_train,
    p_train,
    y_test,
    p_test
) {
  prog_train <- wybierz_prog_youden(y_train, p_train)
  
  met_train <- metryki_klasyfikacji(y_train, p_train, prog = prog_train)
  met_test <- metryki_klasyfikacji(y_test, p_test, prog = prog_train)
  
  if (is.null(fit)) {
    AIC_value <- "nie dotyczy"
    BIC_value <- "nie dotyczy"
    logLik_value <- "nie dotyczy"
    singular <- "nie dotyczy"
  } else {
    AIC_value <- as.character(AIC(fit))
    BIC_value <- as.character(BIC(fit))
    logLik_value <- as.character(as.numeric(logLik(fit)))
    
    singular <- ifelse(
      model_mieszany,
      ifelse(isSingular(fit), "tak", "nie"),
      "nie dotyczy"
    )
  }
  
  data.frame(
    target = target,
    model = model_name,
    typ_modelu = typ_modelu,
    efekty_losowe = efekty_losowe,
    liczba_predyktorow_stalych = liczba_predyktorow_stalych,
    AIC = AIC_value,
    BIC = BIC_value,
    logLik = logLik_value,
    singular = singular,
    prog_decyzyjny = prog_train,
    
    AUC_train = auc_roc(y_train, p_train),
    Brier_train = brier_score(y_train, p_train),
    LogLoss_train = log_loss(y_train, p_train),
    Accuracy_train = met_train$accuracy,
    Sensitivity_train = met_train$sensitivity,
    Specificity_train = met_train$specificity,
    Precision_train = met_train$precision,
    F1_train = met_train$F1,
    
    AUC_test = auc_roc(y_test, p_test),
    Brier_test = brier_score(y_test, p_test),
    LogLoss_test = log_loss(y_test, p_test),
    Accuracy_test = met_test$accuracy,
    Sensitivity_test = met_test$sensitivity,
    Specificity_test = met_test$specificity,
    Precision_test = met_test$precision,
    F1_test = met_test$F1,
    
    TP_test = met_test$TP,
    TN_test = met_test$TN,
    FP_test = met_test$FP,
    FN_test = met_test$FN
  )
}

zapisz_parametry_modelu <- function(fit, target, model_name) {
  s <- coef(summary(fit))
  
  tab <- data.frame(
    target = target,
    model = model_name,
    parametr = rownames(s),
    estimate = s[, "Estimate"],
    std_error = s[, "Std. Error"],
    statistic = s[, ncol(s) - 1],
    p_value = s[, ncol(s)]
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
# 2. Definicja zmiennych binarnych
# ------------------------------------------------------------

# Progi podane w węzłach i przeliczone na km/h.
# W razie potrzeby można je zmienić zgodnie z założeniami rozdziału.
wezel_na_kmh <- 1.852

prog_wind_speed_kt <- 15
prog_wind_gusts_kt <- 25

prog_wind_speed_kmh <- prog_wind_speed_kt * wezel_na_kmh
prog_wind_gusts_kmh <- prog_wind_gusts_kt * wezel_na_kmh

dane$wind_speed_10m_next_event <- ifelse(
  dane$wind_speed_10m_next >= prog_wind_speed_kmh,
  1,
  0
)

dane$wind_gusts_10m_next_event <- ifelse(
  dane$wind_gusts_10m_next >= prog_wind_gusts_kmh,
  1,
  0
)

opis_progowych_zmiennych <- data.frame(
  target = c("wind_speed_10m_next_event", "wind_gusts_10m_next_event"),
  zmienna_ciagla = c("wind_speed_10m_next", "wind_gusts_10m_next"),
  prog_wezly = c(prog_wind_speed_kt, prog_wind_gusts_kt),
  prog_kmh = c(prog_wind_speed_kmh, prog_wind_gusts_kmh)
)

write.csv(
  opis_progowych_zmiennych,
  file.path(results_dir, "opis_progowych_zmiennych.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 3. Predyktory i zmienne zależne
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
  "wind_speed_10m_next_event",
  "wind_gusts_10m_next_event"
)

# ------------------------------------------------------------
# 4. Korelacje predyktorów
# ------------------------------------------------------------

dane_korelacje <- dane %>%
  dplyr::select(all_of(predyktory))

dane_korelacje <- dane_korelacje[
  stats::complete.cases(dane_korelacje),
  ,
  drop = FALSE
]

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
      miesiac_f,
      dzien_roku_f,
      sezon_f,
      all_of(target),
      all_of(predyktory)
    )
  
  dane_model <- dane_model[
    stats::complete.cases(dane_model),
    ,
    drop = FALSE
  ]
  
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
    liczba_klasy_1_train = sum(y_train == 1),
    liczba_klasy_0_train = sum(y_train == 0),
    udzial_klasy_1_train = mean(y_train),
    liczba_klasy_1_test = sum(y_test == 1),
    liczba_klasy_0_test = sum(y_test == 0),
    udzial_klasy_1_test = mean(y_test)
  )
  
  write.csv(
    opis_danych,
    file.path(results_dir, paste0("opis_danych_", suffix, ".csv")),
    row.names = FALSE
  )
  
  if (target == "wind_speed_10m_next_event") {
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
  predykcje_train <- list()
  status_modeli <- data.frame()
  
  control_glmer <- glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 200000)
  )
  
  # ----------------------------------------------------------
  # 5.1. Model zerowy
  # ----------------------------------------------------------
  
  p_null_train <- rep(mean(y_train), length(y_train))
  p_null_test <- rep(mean(y_train), length(y_test))
  
  wynik_null <- wynik_modelu_klasyfikacji(
    target = target,
    model_name = "Model zerowy",
    typ_modelu = "bazowy",
    efekty_losowe = "brak",
    liczba_predyktorow_stalych = 0,
    fit = NULL,
    model_mieszany = FALSE,
    y_train = y_train,
    p_train = p_null_train,
    y_test = y_test,
    p_test = p_null_test
  )
  
  wyniki[["Model zerowy"]] <- wynik_null
  predykcje_train[["Model zerowy"]] <- p_null_train
  predykcje_test[["Model zerowy"]] <- p_null_test
  
  # ----------------------------------------------------------
  # 5.2. Model GLM bez efektów losowych
  # ----------------------------------------------------------
  
  formula_glm <- as.formula(
    paste(target, "~", fixed_full)
  )
  
  fit_glm <- glm(
    formula_glm,
    data = train_data,
    family = binomial(link = "logit")
  )
  
  p_glm_train <- predict(
    fit_glm,
    newdata = train_data,
    type = "response"
  )
  
  p_glm_test <- predict(
    fit_glm,
    newdata = test_data,
    type = "response"
  )
  
  wynik_glm <- wynik_modelu_klasyfikacji(
    target = target,
    model_name = "GLM full",
    typ_modelu = "GLM",
    efekty_losowe = "brak",
    liczba_predyktorow_stalych = length(predyktory_z),
    fit = fit_glm,
    model_mieszany = FALSE,
    y_train = y_train,
    p_train = p_glm_train,
    y_test = y_test,
    p_test = p_glm_test
  )
  
  wyniki[["GLM full"]] <- wynik_glm
  modele[["GLM full"]] <- fit_glm
  predykcje_train[["GLM full"]] <- p_glm_train
  predykcje_test[["GLM full"]] <- p_glm_test
  
  zapisz_parametry_modelu(fit_glm, target, "GLM full")
  zapisz_korelacje_estymatorow(fit_glm, target, "GLM full")
  
  capture.output(
    print(summary(fit_glm, correlation = TRUE)),
    file = file.path(results_dir, paste0("summary_GLM_full_", suffix, ".txt"))
  )
  
  # ----------------------------------------------------------
  # 5.3. Specyfikacje modeli GLMM
  # ----------------------------------------------------------
  
  specyfikacje_glmm <- list(
    list(
      model = "GLMM RI miesiac bez predyktorow",
      typ = "RI",
      efekty = "(1 | miesiac)",
      liczba_pred = 0,
      formula = as.formula(
        paste(target, "~ 1 + (1 | miesiac_f)")
      )
    ),
    list(
      model = "GLMM RI dzien_roku bez predyktorow",
      typ = "RI",
      efekty = "(1 | dzien_roku)",
      liczba_pred = 0,
      formula = as.formula(
        paste(target, "~ 1 + (1 | dzien_roku_f)")
      )
    ),
    list(
      model = "GLMM RIFS sezon",
      typ = "RIFS",
      efekty = "(1 | sezon)",
      liczba_pred = length(predyktory_z),
      formula = as.formula(
        paste(target, "~", fixed_full, "+ (1 | sezon_f)")
      )
    ),
    list(
      model = "GLMM RIFS miesiac",
      typ = "RIFS",
      efekty = "(1 | miesiac)",
      liczba_pred = length(predyktory_z),
      formula = as.formula(
        paste(target, "~", fixed_full, "+ (1 | miesiac_f)")
      )
    ),
    list(
      model = "GLMM RIFS dzien_roku",
      typ = "RIFS",
      efekty = "(1 | dzien_roku)",
      liczba_pred = length(predyktory_z),
      formula = as.formula(
        paste(target, "~", fixed_full, "+ (1 | dzien_roku_f)")
      )
    ),
    list(
      model = "GLMM RIFS miesiac plus dzien_roku",
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
      model = "GLMM FIRS miesiac",
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
      model = "GLMM RSI miesiac",
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
      model = "GLMM RSI-i miesiac",
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
  # 5.4. Dopasowanie modeli GLMM
  # ----------------------------------------------------------
  
  for (spec in specyfikacje_glmm) {
    
    cat("\nDopasowanie modelu:", spec$model, "\n")
    
    fit <- tryCatch(
      glmer(
        spec$formula,
        data = train_data,
        family = binomial(link = "logit"),
        control = control_glmer,
        nAGQ = 0
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
    
    p_train <- predict(
      fit,
      newdata = train_data,
      type = "response",
      allow.new.levels = TRUE
    )
    
    p_test <- predict(
      fit,
      newdata = test_data,
      type = "response",
      allow.new.levels = TRUE
    )
    
    wynik <- wynik_modelu_klasyfikacji(
      target = target,
      model_name = spec$model,
      typ_modelu = spec$typ,
      efekty_losowe = spec$efekty,
      liczba_predyktorow_stalych = spec$liczba_pred,
      fit = fit,
      model_mieszany = TRUE,
      y_train = y_train,
      p_train = p_train,
      y_test = y_test,
      p_test = p_test
    )
    
    wyniki[[spec$model]] <- wynik
    modele[[spec$model]] <- fit
    predykcje_train[[spec$model]] <- p_train
    predykcje_test[[spec$model]] <- p_test
    
    status_modeli <- rbind(
      status_modeli,
      data.frame(
        target = target,
        model = spec$model,
        status = "dopasowano",
        komunikat = ifelse(isSingular(fit), "model osobliwy", "brak")
      )
    )
    
    zapisz_parametry_modelu(fit, target, spec$model)
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
    file.path(results_dir, paste0("status_modeli_glmm_", suffix, ".csv")),
    row.names = FALSE
  )
  
  # ----------------------------------------------------------
  # 5.5. Porównanie modeli
  # ----------------------------------------------------------
  
  porownanie_koncowe <- do.call(rbind, wyniki)
  
  porownanie_koncowe <- porownanie_koncowe %>%
    dplyr::arrange(desc(AUC_test), Brier_test, LogLoss_test)
  
  rownames(porownanie_koncowe) <- NULL
  
  write.csv(
    porownanie_koncowe,
    file.path(results_dir, paste0("porownanie_koncowe_glmm_", suffix, ".csv")),
    row.names = FALSE
  )
  
  najlepszy_model <- porownanie_koncowe[1, , drop = FALSE]
  
  write.csv(
    najlepszy_model,
    file.path(results_dir, paste0("najlepszy_model_glmm_", suffix, ".csv")),
    row.names = FALSE
  )
  
  prog_kompromisu <- najlepszy_model$AUC_test[1] * 0.99
  
  kandydaci_kompromis <- porownanie_koncowe %>%
    dplyr::filter(AUC_test >= prog_kompromisu) %>%
    dplyr::arrange(liczba_predyktorow_stalych, Brier_test)
  
  najlepszy_model_oszczedny <- kandydaci_kompromis[1, , drop = FALSE]
  
  write.csv(
    najlepszy_model_oszczedny,
    file.path(results_dir, paste0("najlepszy_model_oszczedny_glmm_", suffix, ".csv")),
    row.names = FALSE
  )
  
  # ----------------------------------------------------------
  # 5.6. Wykresy porównawcze
  # ----------------------------------------------------------
  
  wykres_auc <- ggplot(
    porownanie_koncowe,
    aes(x = reorder(model, AUC_test), y = AUC_test)
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste("Porównanie AUC test:", target),
      x = "Model",
      y = "AUC test"
    ) +
    theme_minimal()
  
  zapisz_gg(
    paste0("01_porownanie_AUC_test_glmm_", suffix, ".png"),
    wykres_auc,
    szerokosc = 11,
    wysokosc = 7
  )
  
  wykres_f1 <- ggplot(
    porownanie_koncowe,
    aes(x = reorder(model, F1_test), y = F1_test)
  ) +
    geom_col() +
    coord_flip() +
    labs(
      title = paste("Porównanie F1 test:", target),
      x = "Model",
      y = "F1 test"
    ) +
    theme_minimal()
  
  zapisz_gg(
    paste0("02_porownanie_F1_test_glmm_", suffix, ".png"),
    wykres_f1,
    szerokosc = 11,
    wysokosc = 7
  )
  
  najlepsza_nazwa <- najlepszy_model$model[1]
  p_best_test <- predykcje_test[[najlepsza_nazwa]]
  
  roc_best <- dane_roc(y_test, p_best_test)
  
  wykres_roc <- ggplot(
    roc_best,
    aes(x = FPR, y = TPR)
  ) +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(
      title = paste("Krzywa ROC najlepszego modelu:", target),
      subtitle = paste("Model:", najlepsza_nazwa),
      x = "False Positive Rate",
      y = "True Positive Rate"
    ) +
    theme_minimal()
  
  zapisz_gg(
    paste0("03_ROC_best_glmm_", suffix, ".png"),
    wykres_roc,
    szerokosc = 7,
    wysokosc = 6
  )
  
  dane_kalibracja <- data.frame(
    y = y_test,
    p = p_best_test
  )
  
  dane_kalibracja$grupa <- cut(
    dane_kalibracja$p,
    breaks = seq(0, 1, by = 0.1),
    include.lowest = TRUE
  )
  
  kalibracja <- dane_kalibracja %>%
    dplyr::group_by(grupa) %>%
    dplyr::summarise(
      srednie_p = mean(p, na.rm = TRUE),
      czestosc_klasy_1 = mean(y, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  write.csv(
    kalibracja,
    file.path(results_dir, paste0("kalibracja_best_glmm_", suffix, ".csv")),
    row.names = FALSE
  )
  
  wykres_kalibracja <- ggplot(
    kalibracja,
    aes(x = srednie_p, y = czestosc_klasy_1)
  ) +
    geom_point(aes(size = n)) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(
      title = paste("Kalibracja najlepszego modelu:", target),
      subtitle = paste("Model:", najlepsza_nazwa),
      x = "Średnie prawdopodobieństwo predykcji",
      y = "Rzeczywista częstość klasy 1",
      size = "Liczba obserwacji"
    ) +
    theme_minimal()
  
  zapisz_gg(
    paste0("04_kalibracja_best_glmm_", suffix, ".png"),
    wykres_kalibracja,
    szerokosc = 7,
    wysokosc = 6
  )
  
  zapisz_wykres(paste0("05_szereg_prawdopodobienstw_best_glmm_", suffix, ".png"), {
    n_plot <- min(500, nrow(test_data))
    
    plot(
      test_data$datetime[1:n_plot],
      y_test[1:n_plot],
      type = "h",
      ylim = c(0, 1),
      main = paste("Najlepszy GLMM: klasa rzeczywista i prawdopodobieństwo:", target),
      xlab = "Czas",
      ylab = "Klasa / prawdopodobieństwo"
    )
    
    lines(
      test_data$datetime[1:n_plot],
      p_best_test[1:n_plot],
      lwd = 2
    )
    
    legend(
      "topright",
      legend = c("Klasa rzeczywista", "Prawdopodobieństwo predykcji"),
      lty = c(1, 1),
      lwd = c(1, 2),
      bty = "n"
    )
  })
  
  # ----------------------------------------------------------
  # 5.7. Wykresy efektów losowych
  # ----------------------------------------------------------
  
  if ("GLMM RIFS miesiac" %in% names(modele)) {
    fit_miesiac <- modele[["GLMM RIFS miesiac"]]
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
      paste0("06_efekty_losowe_miesiac_glmm_", suffix, ".png"),
      wykres_re_miesiac,
      szerokosc = 8,
      wysokosc = 5
    )
  }
  
  if ("GLMM RSI-i miesiac" %in% names(modele)) {
    fit_rsii <- modele[["GLMM RSI-i miesiac"]]
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
        aes(
          x = reorder(miesiac, .data[[kolumna_slope[1]]]),
          y = .data[[kolumna_slope[1]]]
        )
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
        paste0("07_efekty_losowe_nachylenia_miesiac_glmm_", suffix, ".png"),
        wykres_slope,
        szerokosc = 8,
        wysokosc = 5
      )
    }
  }
  
  # ----------------------------------------------------------
  # 5.8. Wyniki w konsoli
  # ----------------------------------------------------------
  
  cat("\n--- Opis danych ---\n")
  print(opis_danych)
  
  cat("\n--- Status modeli ---\n")
  print(status_modeli)
  
  cat("\n--- Najlepszy model według AUC test ---\n")
  print(najlepszy_model)
  
  cat("\n--- Najlepszy model oszczędny w granicy 1% od najlepszego AUC test ---\n")
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
  file.path(results_dir, "zbiorcze_opis_danych_glmm.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_status_modeli,
  file.path(results_dir, "zbiorcze_status_modeli_glmm.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_porownanie,
  file.path(results_dir, "zbiorcze_porownanie_koncowe_glmm.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_najlepsze,
  file.path(results_dir, "zbiorcze_najlepsze_modele_glmm.csv"),
  row.names = FALSE
)

write.csv(
  zbiorcze_najlepsze_oszczedne,
  file.path(results_dir, "zbiorcze_najlepsze_modele_oszczedne_glmm.csv"),
  row.names = FALSE
)

# ------------------------------------------------------------
# 8. Komunikat końcowy
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("ZAKOŃCZONO ANALIZĘ MODELI GLMM\n")
cat("Predykcja dotyczy klasy zdarzenia w następnej godzinie: t + 1.\n")
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