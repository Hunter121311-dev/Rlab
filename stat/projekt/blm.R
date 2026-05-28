# ============================================================
# BAGGING, LASY LOSOWE I MARS DLA DANYCH METEOROLOGICZNYCH
# Zwykly skrypt R, bez R Markdown
# Wersja szybka: ranger, 500 drzew, 14 watkow, czytelny postep etapow
# ============================================================

# install.packages(c("dplyr", "lubridate", "rpart", "rpart.plot", "ranger", "earth", "plotmo"))

library(dplyr)
library(lubridate)
library(rpart)
library(rpart.plot)
library(ranger)
library(earth)
library(plotmo)

set.seed(2025)

# ------------------------------------------------------------
# 0. Ustawienia
# ------------------------------------------------------------

data_path <- "data/chalupy_hourly_10000_days.csv"
if (!file.exists(data_path)) {
  data_path <- "/Users/damia/OneDrive/Dokumenty/Rlab/stat/projekt/data/chalupy_hourly_10000_days.csv"
}
if (!file.exists(data_path)) {
  stop("Nie znaleziono pliku danych. Zmien zmienna data_path na wlasciwa sciezke.")
}

plot_dir <- "plots_bagging_rf_mars_ranger_progress"
results_dir <- "wyniki_bagging_rf_mars_ranger_progress"
dir.create(plot_dir, showWarnings = FALSE)
dir.create(results_dir, showWarnings = FALSE)

ntree_rf <- 500
liczba_watkow <- 14
liczba_watkow <- min(liczba_watkow, max(1, parallel::detectCores(logical = TRUE)))
mars_nk <- 60

cat("\n============================================================\n")
cat("START SKRYPTU\n")
cat("ranger: ", ntree_rf, " drzew na bagging/las losowy\n", sep = "")
cat("liczba watkow: ", liczba_watkow, "\n", sep = "")
cat("razem w calym skrypcie: 8 modeli zespolowych * 500 = 4000 drzew\n")
cat("============================================================\n\n")
flush.console()

plan_obliczen <- data.frame(
  element = c(
    "targety_regresyjne",
    "targety_klasyfikacyjne",
    "modele_zespolowe_na_target",
    "liczba_lasow_lacznie",
    "liczba_drzew_na_las",
    "liczba_drzew_lacznie",
    "liczba_watkow_ranger"
  ),
  wartosc = c(2, 2, 2, 8, ntree_rf, 8 * ntree_rf, liczba_watkow)
)
write.csv(plan_obliczen, file.path(results_dir, "plan_obliczen.csv"), row.names = FALSE)
print(plan_obliczen)

# ------------------------------------------------------------
# 0.1. Funkcje pomocnicze
# ------------------------------------------------------------

czas_start_global <- Sys.time()

stamp <- function() format(Sys.time(), "%H:%M:%S")

log_msg <- function(...) {
  cat("[", stamp(), "] ", paste(..., collapse = ""), "\n", sep = "")
  flush.console()
}

czasuj <- function(etykieta, expr) {
  log_msg("START: ", etykieta)
  t0 <- Sys.time()
  wynik <- force(expr)
  t1 <- Sys.time()
  log_msg("KONIEC: ", etykieta, " | czas: ", round(as.numeric(difftime(t1, t0, units = "mins")), 2), " min")
  wynik
}

bezpieczna_nazwa <- function(x) {
  x <- gsub("ą", "a", x); x <- gsub("ć", "c", x); x <- gsub("ę", "e", x)
  x <- gsub("ł", "l", x); x <- gsub("ń", "n", x); x <- gsub("ó", "o", x)
  x <- gsub("ś", "s", x); x <- gsub("ż", "z", x); x <- gsub("ź", "z", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

zapisz_wykres <- function(nazwa, kod) {
  png(file.path(plot_dir, nazwa), width = 1300, height = 850, res = 140)
  on.exit(dev.off(), add = TRUE)
  eval(substitute(kod), envir = parent.frame())
}

mse <- function(y, y_hat) mean((y - y_hat)^2, na.rm = TRUE)
rmse <- function(y, y_hat) sqrt(mse(y, y_hat))
mae <- function(y, y_hat) mean(abs(y - y_hat), na.rm = TRUE)
r2_test <- function(y, y_hat) {
  1 - sum((y - y_hat)^2, na.rm = TRUE) / sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
}
metryki_regresji <- function(y, y_hat) {
  data.frame(MSE = mse(y, y_hat), RMSE = rmse(y, y_hat), MAE = mae(y, y_hat), R2 = r2_test(y, y_hat))
}

auc_roc <- function(y, p) {
  ok <- complete.cases(y, p)
  y <- y[ok]; p <- p[ok]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(p, ties.method = "average")
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
log_loss <- function(y, p) {
  eps <- 1e-15
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p), na.rm = TRUE)
}
brier_score <- function(y, p) mean((y - p)^2, na.rm = TRUE)

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
  F1 <- ifelse(is.na(precision) | is.na(sensitivity) | (precision + sensitivity) == 0,
               NA_real_, 2 * precision * sensitivity / (precision + sensitivity))
  data.frame(threshold = prog, accuracy = accuracy, sensitivity = sensitivity,
             specificity = specificity, precision = precision, F1 = F1,
             TP = TP, TN = TN, FP = FP, FN = FN)
}

wybierz_prog_youden <- function(y, p) {
  progi <- seq(0.05, 0.95, by = 0.01)
  wyniki <- lapply(progi, function(pr) {
    met <- metryki_klasyfikacji(y, p, prog = pr)
    data.frame(threshold = pr, youden = met$sensitivity + met$specificity - 1)
  })
  wyniki <- do.call(rbind, wyniki)
  if (all(is.na(wyniki$youden))) return(0.5)
  wyniki$threshold[which.max(wyniki$youden)]
}

buduj_formule <- function(target, predyktory) {
  as.formula(paste(target, "~", paste(predyktory, collapse = " + ")))
}

przygotuj_df_model <- function(data, target, predyktory) {
  # Tylko target + predyktory. Bez data/datetime. To omija problemy ranger/earth z dodatkowymi kolumnami.
  df <- as.data.frame(data[, c(target, predyktory), drop = FALSE])
  for (nm in names(df)) {
    if (is.logical(df[[nm]])) df[[nm]] <- as.integer(df[[nm]])
  }
  df
}

przygotuj_x <- function(data, predyktory) {
  x <- as.data.frame(data[, predyktory, drop = FALSE])
  for (nm in names(x)) {
    if (is.logical(x[[nm]])) x[[nm]] <- as.integer(x[[nm]])
  }
  x
}

predict_ranger_reg <- function(model, newdata, predyktory) {
  x <- przygotuj_x(newdata, predyktory)
  as.numeric(predict(model, data = x, num.threads = liczba_watkow)$predictions)
}

predict_ranger_prob <- function(model, newdata, predyktory) {
  x <- przygotuj_x(newdata, predyktory)
  pr <- predict(model, data = x, num.threads = liczba_watkow)$predictions
  if (is.vector(pr)) return(as.numeric(pr))
  if ("Tak" %in% colnames(pr)) return(as.numeric(pr[, "Tak"]))
  as.numeric(pr[, ncol(pr)])
}

zapisz_importance <- function(imp, nazwa_pliku, model_name, target) {
  tab <- data.frame(zmienna = names(imp), importance = as.numeric(imp), model = model_name, target = target)
  tab <- tab[order(-tab$importance), ]
  write.csv(tab, file.path(results_dir, nazwa_pliku), row.names = FALSE)
  invisible(tab)
}

# MARS przez interfejs x/y, a nie formula.
# To jest celowe: omija blad model.matrix typu:
# length of 'dimnames' [2] not equal to array extent.
train_mars_reg <- function(x_train, y_train, degree) {
  earth(x = as.matrix(x_train), y = y_train, degree = degree, nk = mars_nk)
}
train_mars_clf <- function(x_train, y_train01, degree) {
  earth(x = as.matrix(x_train), y = y_train01, degree = degree, nk = mars_nk, glm = list(family = binomial))
}
predict_mars <- function(model, x_new, type = "response") {
  as.numeric(predict(model, newdata = as.matrix(x_new), type = type))
}

# ------------------------------------------------------------
# 1. Wczytanie i przygotowanie danych
# ------------------------------------------------------------

log_msg("Wczytywanie danych: ", data_path)
dane <- read.csv(data_path)
dane$datetime <- as.POSIXct(dane$datetime, format = "%Y-%m-%dT%H:%M")
dane <- dane %>% arrange(datetime)

dane$data <- as.Date(dane$datetime)
dane$rok <- year(dane$datetime)
dane$miesiac <- month(dane$datetime)
dane$dzien_roku <- yday(dane$datetime)
dane$godzina <- hour(dane$datetime)

dane$wind_speed_10m_next <- lead(dane$wind_speed_10m, 1)
dane$wind_gusts_10m_next <- lead(dane$wind_gusts_10m, 1)

dane$sin_dzien <- sin(2 * pi * dane$dzien_roku / 365)
dane$cos_dzien <- cos(2 * pi * dane$dzien_roku / 365)
dane$sin_godzina <- sin(2 * pi * dane$godzina / 24)
dane$cos_godzina <- cos(2 * pi * dane$godzina / 24)

dane$wind_dir_sin <- sin(2 * pi * dane$wind_direction_10m / 360)
dane$wind_dir_cos <- cos(2 * pi * dane$wind_direction_10m / 360)
dane$polrocze_cieple <- ifelse(dane$miesiac %in% c(4, 5, 6, 7, 8, 9), 1, 0)

wezel_na_kmh <- 1.852
prog_wind_speed_kmh <- 15 * wezel_na_kmh
prog_wind_gusts_kmh <- 25 * wezel_na_kmh

dane$wind_speed_10m_next_event <- ifelse(dane$wind_speed_10m_next >= prog_wind_speed_kmh, 1, 0)
dane$wind_gusts_10m_next_event <- ifelse(dane$wind_gusts_10m_next >= prog_wind_gusts_kmh, 1, 0)

predyktory <- c(
  "wind_speed_10m", "wind_speed_100m", "wind_gusts_10m",
  "wind_dir_sin", "wind_dir_cos",
  "temperature_2m", "relative_humidity_2m", "dew_point_2m", "apparent_temperature",
  "precipitation", "cloud_cover", "pressure_msl", "shortwave_radiation", "is_day",
  "sin_dzien", "cos_dzien", "sin_godzina", "cos_godzina", "polrocze_cieple"
)
predyktory <- predyktory[predyktory %in% names(dane)]
targety_regresja <- c("wind_speed_10m_next", "wind_gusts_10m_next")
targety_klasyfikacja <- c("wind_speed_10m_next_event", "wind_gusts_10m_next_event")

write.csv(data.frame(predyktor = predyktory), file.path(results_dir, "lista_predyktorow.csv"), row.names = FALSE)

# ------------------------------------------------------------
# 2. Modele regresyjne
# ------------------------------------------------------------

analizuj_regresje <- function(target) {
  suffix <- bezpieczna_nazwa(target)
  p <- length(predyktory)
  formula_full <- buduj_formule(target, predyktory)

  dane_model <- dane %>%
    select(data, datetime, all_of(target), all_of(predyktory)) %>%
    filter(complete.cases(.))

  train_data <- dane_model %>% filter(data < as.Date("2025-01-01"))
  test_data <- dane_model %>% filter(data >= as.Date("2025-01-01"))
  y_train <- train_data[[target]]
  y_test <- test_data[[target]]

  x_train <- przygotuj_x(train_data, predyktory)
  x_test <- przygotuj_x(test_data, predyktory)
  df_train_ranger <- przygotuj_df_model(train_data, target, predyktory)

  cat("\n============================================================\n")
  cat("REGRESJA: ", target, "\n", sep = "")
  cat("train: ", nrow(train_data), " | test: ", nrow(test_data), " | predyktory: ", p, "\n", sep = "")
  cat("Bagging i las losowy: ranger, ", ntree_rf, " drzew, ", liczba_watkow, " watkow\n", sep = "")
  cat("============================================================\n")
  flush.console()

  opis_danych <- data.frame(target = target, n_total = nrow(dane_model), n_train = nrow(train_data),
                            n_test = nrow(test_data), mean_train = mean(y_train), mean_test = mean(y_test),
                            sd_train = sd(y_train), sd_test = sd(y_test))
  write.csv(opis_danych, file.path(results_dir, paste0("opis_danych_regresja_", suffix, ".csv")), row.names = FALSE)

  wyniki <- list()
  predykcje <- data.frame(datetime = test_data$datetime, rzeczywiste = y_test)

  dodaj_wynik <- function(model_name, typ_modelu, liczba_predyktorow, parametry, pred_train, pred_test) {
    met_train <- metryki_regresji(y_train, pred_train)
    met_test <- metryki_regresji(y_test, pred_test)
    wyniki[[model_name]] <<- data.frame(
      target = target, model = model_name, typ_modelu = typ_modelu,
      liczba_predyktorow = liczba_predyktorow, parametry = parametry,
      MSE_train = met_train$MSE, RMSE_train = met_train$RMSE,
      MAE_train = met_train$MAE, R2_train = met_train$R2,
      MSE_test = met_test$MSE, RMSE_test = met_test$RMSE,
      MAE_test = met_test$MAE, R2_test = met_test$R2
    )
    predykcje[[bezpieczna_nazwa(model_name)]] <<- as.numeric(pred_test)
  }

  pred_null_train <- rep(mean(y_train), length(y_train))
  pred_null_test <- rep(mean(y_train), length(y_test))
  dodaj_wynik("Model zerowy", "bazowy", 0, "srednia ze zbioru treningowego", pred_null_train, pred_null_test)

  tree_big <- czasuj(paste("Drzewo duze", target), rpart(formula_full, data = train_data, method = "anova", control = rpart.control(cp = 0.0005, xval = 10)))
  pred_tree_big_train <- predict(tree_big, newdata = train_data)
  pred_tree_big_test <- predict(tree_big, newdata = test_data)
  dodaj_wynik("Drzewo duze", "drzewo regresyjne", p, "rpart, cp = 0.0005, xval = 10", pred_tree_big_train, pred_tree_big_test)
  capture.output(printcp(tree_big), file = file.path(results_dir, paste0("cp_tree_big_", suffix, ".txt")))

  n_min <- which.min(tree_big$cptable[, "xerror"])
  cp_min <- tree_big$cptable[n_min, "CP"]
  tree_min <- prune(tree_big, cp = cp_min)
  dodaj_wynik("Drzewo minimum CV", "drzewo regresyjne", p, paste0("cp = ", signif(cp_min, 5)),
              predict(tree_min, train_data), predict(tree_min, test_data))

  limit <- sum(tree_big$cptable[n_min, c("xerror", "xstd")])
  n_1se <- min(which(tree_big$cptable[, "xerror"] < limit))
  cp_1se <- if (n_1se > 1) tree_big$cptable[n_1se - 1, "CP"] else tree_big$cptable[n_1se, "CP"]
  tree_1se <- prune(tree_big, cp = cp_1se)
  dodaj_wynik("Drzewo 1SE", "drzewo regresyjne", p, paste0("cp = ", signif(cp_1se, 5)),
              predict(tree_1se, train_data), predict(tree_1se, test_data))

  zapisz_wykres(paste0("tree_1se_", suffix, ".png"), {
    rpart.plot(tree_1se, digits = 4, main = paste("Drzewo 1SE:", target))
  })

  bag <- czasuj(paste("Bagging ranger", target), ranger(
    dependent.variable.name = target,
    data = df_train_ranger,
    num.trees = ntree_rf,
    mtry = p,
    min.node.size = 10,
    importance = "impurity",
    num.threads = liczba_watkow,
    verbose = TRUE,
    seed = 2025
  ))
  pred_bag_train <- predict_ranger_reg(bag, train_data, predyktory)
  pred_bag_test <- predict_ranger_reg(bag, test_data, predyktory)
  dodaj_wynik("Bagging", "ranger", p,
              paste0("mtry = ", p, ", ntree = ", ntree_rf, ", threads = ", liczba_watkow),
              pred_bag_train, pred_bag_test)
  imp_bag <- zapisz_importance(bag$variable.importance, paste0("importance_bagging_", suffix, ".csv"), "Bagging", target)

  mtry_rf <- max(1, min(p, floor(p / 3)))
  rf <- czasuj(paste("Las losowy ranger", target), ranger(
    dependent.variable.name = target,
    data = df_train_ranger,
    num.trees = ntree_rf,
    mtry = mtry_rf,
    min.node.size = 10,
    importance = "impurity",
    num.threads = liczba_watkow,
    verbose = TRUE,
    seed = 2025
  ))
  pred_rf_train <- predict_ranger_reg(rf, train_data, predyktory)
  pred_rf_test <- predict_ranger_reg(rf, test_data, predyktory)
  dodaj_wynik("Las losowy", "ranger", p,
              paste0("mtry = ", mtry_rf, ", ntree = ", ntree_rf, ", threads = ", liczba_watkow),
              pred_rf_train, pred_rf_test)
  imp_rf <- zapisz_importance(rf$variable.importance, paste0("importance_rf_", suffix, ".csv"), "Las losowy", target)

  mars1 <- czasuj(paste("MARS degree = 1", target), train_mars_reg(x_train, y_train, degree = 1))
  pred_mars1_train <- predict_mars(mars1, x_train)
  pred_mars1_test <- predict_mars(mars1, x_test)
  dodaj_wynik("MARS degree = 1", "MARS", p, paste0("degree = 1, nk = ", mars_nk), pred_mars1_train, pred_mars1_test)
  capture.output(summary(mars1), file = file.path(results_dir, paste0("summary_mars1_", suffix, ".txt")))

  mars2 <- czasuj(paste("MARS degree = 2", target), train_mars_reg(x_train, y_train, degree = 2))
  pred_mars2_train <- predict_mars(mars2, x_train)
  pred_mars2_test <- predict_mars(mars2, x_test)
  dodaj_wynik("MARS degree = 2", "MARS", p, paste0("degree = 2, nk = ", mars_nk), pred_mars2_train, pred_mars2_test)
  capture.output(summary(mars2), file = file.path(results_dir, paste0("summary_mars2_", suffix, ".txt")))

  predykcje$Bagging <- pred_bag_test
  predykcje$Las_losowy <- pred_rf_test
  predykcje$MARS_degree_1 <- pred_mars1_test
  predykcje$MARS_degree_2 <- pred_mars2_test
  write.csv(predykcje, file.path(results_dir, paste0("predykcje_regresja_", suffix, ".csv")), row.names = FALSE)

  start_day <- as.Date("2025-06-02")
  end_day <- start_day + 2
  wykres_df <- predykcje[as.Date(predykcje$datetime) >= start_day & as.Date(predykcje$datetime) <= end_day, ]
  zapisz_wykres(paste0("predykcje_regresja_", suffix, ".png"), {
    plot(wykres_df$datetime, wykres_df$rzeczywiste, pch = 16, xlab = "Czas", ylab = target,
         main = paste("Wartosci rzeczywiste i predykcje:", target))
    lines(wykres_df$datetime, wykres_df$Bagging, lwd = 2)
    lines(wykres_df$datetime, wykres_df$Las_losowy, lwd = 2, lty = 2)
    lines(wykres_df$datetime, wykres_df$MARS_degree_1, lwd = 2, lty = 3)
    lines(wykres_df$datetime, wykres_df$MARS_degree_2, lwd = 2, lty = 4)
    legend("topright", legend = c("rzeczywiste", "bagging", "las losowy", "MARS 1", "MARS 2"),
           pch = c(16, NA, NA, NA, NA), lty = c(NA, 1, 2, 3, 4), bty = "n")
  })

  wyniki_df <- do.call(rbind, wyniki) %>% arrange(MSE_test)
  write.csv(wyniki_df, file.path(results_dir, paste0("wyniki_regresja_", suffix, ".csv")), row.names = FALSE)
  print(wyniki_df)

  list(wyniki = wyniki_df, modele = list(tree_big = tree_big, tree_min = tree_min, tree_1se = tree_1se,
                                         bag = bag, rf = rf, mars1 = mars1, mars2 = mars2))
}

# ------------------------------------------------------------
# 3. Modele klasyfikacyjne
# ------------------------------------------------------------

analizuj_klasyfikacje <- function(target) {
  suffix <- bezpieczna_nazwa(target)
  p <- length(predyktory)
  formula_full <- buduj_formule(target, predyktory)

  dane_model <- dane %>%
    select(data, datetime, all_of(target), all_of(predyktory)) %>%
    filter(complete.cases(.))
  dane_model[[target]] <- factor(ifelse(dane_model[[target]] == 1, "Tak", "Nie"), levels = c("Nie", "Tak"))

  train_data <- dane_model %>% filter(data < as.Date("2025-01-01"))
  test_data <- dane_model %>% filter(data >= as.Date("2025-01-01"))
  y_train_factor <- train_data[[target]]
  y_test_factor <- test_data[[target]]
  y_train <- ifelse(y_train_factor == "Tak", 1, 0)
  y_test <- ifelse(y_test_factor == "Tak", 1, 0)

  x_train <- przygotuj_x(train_data, predyktory)
  x_test <- przygotuj_x(test_data, predyktory)
  df_train_ranger <- przygotuj_df_model(train_data, target, predyktory)

  cat("\n============================================================\n")
  cat("KLASYFIKACJA: ", target, "\n", sep = "")
  cat("train: ", nrow(train_data), " | test: ", nrow(test_data), " | predyktory: ", p, "\n", sep = "")
  cat("Bagging i las losowy: ranger, ", ntree_rf, " drzew, ", liczba_watkow, " watkow\n", sep = "")
  cat("============================================================\n")
  flush.console()

  wyniki <- list()
  predykcje <- data.frame(datetime = test_data$datetime, rzeczywiste = y_test)

  dodaj_wynik <- function(model_name, typ_modelu, liczba_predyktorow, parametry, p_train, p_test) {
    prog <- wybierz_prog_youden(y_train, p_train)
    met_train <- metryki_klasyfikacji(y_train, p_train, prog = prog)
    met_test <- metryki_klasyfikacji(y_test, p_test, prog = prog)
    wyniki[[model_name]] <<- data.frame(
      target = target, model = model_name, typ_modelu = typ_modelu,
      liczba_predyktorow = liczba_predyktorow, parametry = parametry,
      prog_decyzyjny = prog,
      AUC_train = auc_roc(y_train, p_train), Brier_train = brier_score(y_train, p_train),
      LogLoss_train = log_loss(y_train, p_train), accuracy_train = met_train$accuracy,
      sensitivity_train = met_train$sensitivity, specificity_train = met_train$specificity,
      precision_train = met_train$precision, F1_train = met_train$F1,
      AUC_test = auc_roc(y_test, p_test), Brier_test = brier_score(y_test, p_test),
      LogLoss_test = log_loss(y_test, p_test), accuracy_test = met_test$accuracy,
      sensitivity_test = met_test$sensitivity, specificity_test = met_test$specificity,
      precision_test = met_test$precision, F1_test = met_test$F1,
      TP_test = met_test$TP, TN_test = met_test$TN, FP_test = met_test$FP, FN_test = met_test$FN
    )
    predykcje[[bezpieczna_nazwa(model_name)]] <<- as.numeric(p_test)
  }

  p_null_train <- rep(mean(y_train), length(y_train))
  p_null_test <- rep(mean(y_train), length(y_test))
  dodaj_wynik("Model zerowy", "bazowy", 0, "udzial klasy pozytywnej w zbiorze treningowym", p_null_train, p_null_test)

  glm_full <- czasuj(paste("GLM full", target), glm(formula_full, data = train_data, family = binomial))
  dodaj_wynik("GLM full", "regresja logistyczna", p, "pelny model logistyczny",
              as.numeric(predict(glm_full, train_data, type = "response")),
              as.numeric(predict(glm_full, test_data, type = "response")))
  capture.output(summary(glm_full), file = file.path(results_dir, paste0("summary_glm_", suffix, ".txt")))

  tree_big <- czasuj(paste("Drzewo duze", target), rpart(formula_full, data = train_data, method = "class", control = rpart.control(cp = 0.0005, xval = 10)))
  dodaj_wynik("Drzewo duze", "drzewo klasyfikacyjne", p, "rpart, cp = 0.0005, xval = 10",
              predict(tree_big, train_data, type = "prob")[, "Tak"],
              predict(tree_big, test_data, type = "prob")[, "Tak"])

  n_min <- which.min(tree_big$cptable[, "xerror"])
  cp_min <- tree_big$cptable[n_min, "CP"]
  tree_min <- prune(tree_big, cp = cp_min)
  dodaj_wynik("Drzewo minimum CV", "drzewo klasyfikacyjne", p, paste0("cp = ", signif(cp_min, 5)),
              predict(tree_min, train_data, type = "prob")[, "Tak"],
              predict(tree_min, test_data, type = "prob")[, "Tak"])

  limit <- sum(tree_big$cptable[n_min, c("xerror", "xstd")])
  n_1se <- min(which(tree_big$cptable[, "xerror"] < limit))
  cp_1se <- if (n_1se > 1) tree_big$cptable[n_1se - 1, "CP"] else tree_big$cptable[n_1se, "CP"]
  tree_1se <- prune(tree_big, cp = cp_1se)
  dodaj_wynik("Drzewo 1SE", "drzewo klasyfikacyjne", p, paste0("cp = ", signif(cp_1se, 5)),
              predict(tree_1se, train_data, type = "prob")[, "Tak"],
              predict(tree_1se, test_data, type = "prob")[, "Tak"])

  bag <- czasuj(paste("Bagging ranger", target), ranger(
    dependent.variable.name = target,
    data = df_train_ranger,
    num.trees = ntree_rf,
    mtry = p,
    min.node.size = 10,
    importance = "impurity",
    probability = TRUE,
    num.threads = liczba_watkow,
    verbose = TRUE,
    seed = 2025
  ))
  p_bag_train <- predict_ranger_prob(bag, train_data, predyktory)
  p_bag_test <- predict_ranger_prob(bag, test_data, predyktory)
  dodaj_wynik("Bagging", "ranger", p,
              paste0("mtry = ", p, ", ntree = ", ntree_rf, ", threads = ", liczba_watkow),
              p_bag_train, p_bag_test)
  zapisz_importance(bag$variable.importance, paste0("importance_bagging_", suffix, ".csv"), "Bagging", target)

  mtry_rf <- max(1, min(p, floor(sqrt(p))))
  rf <- czasuj(paste("Las losowy ranger", target), ranger(
    dependent.variable.name = target,
    data = df_train_ranger,
    num.trees = ntree_rf,
    mtry = mtry_rf,
    min.node.size = 10,
    importance = "impurity",
    probability = TRUE,
    num.threads = liczba_watkow,
    verbose = TRUE,
    seed = 2025
  ))
  p_rf_train <- predict_ranger_prob(rf, train_data, predyktory)
  p_rf_test <- predict_ranger_prob(rf, test_data, predyktory)
  dodaj_wynik("Las losowy", "ranger", p,
              paste0("mtry = ", mtry_rf, ", ntree = ", ntree_rf, ", threads = ", liczba_watkow),
              p_rf_train, p_rf_test)
  zapisz_importance(rf$variable.importance, paste0("importance_rf_", suffix, ".csv"), "Las losowy", target)

  mars1 <- czasuj(paste("MARS degree = 1", target), train_mars_clf(x_train, y_train, degree = 1))
  p_mars1_train <- predict_mars(mars1, x_train, type = "response")
  p_mars1_test <- predict_mars(mars1, x_test, type = "response")
  dodaj_wynik("MARS degree = 1", "MARS", p, paste0("degree = 1, nk = ", mars_nk), p_mars1_train, p_mars1_test)
  capture.output(summary(mars1), file = file.path(results_dir, paste0("summary_mars1_", suffix, ".txt")))

  mars2 <- czasuj(paste("MARS degree = 2", target), train_mars_clf(x_train, y_train, degree = 2))
  p_mars2_train <- predict_mars(mars2, x_train, type = "response")
  p_mars2_test <- predict_mars(mars2, x_test, type = "response")
  dodaj_wynik("MARS degree = 2", "MARS", p, paste0("degree = 2, nk = ", mars_nk), p_mars2_train, p_mars2_test)
  capture.output(summary(mars2), file = file.path(results_dir, paste0("summary_mars2_", suffix, ".txt")))

  predykcje$Bagging <- p_bag_test
  predykcje$Las_losowy <- p_rf_test
  predykcje$MARS_degree_1 <- p_mars1_test
  predykcje$MARS_degree_2 <- p_mars2_test
  write.csv(predykcje, file.path(results_dir, paste0("predykcje_klasyfikacja_", suffix, ".csv")), row.names = FALSE)

  wyniki_df <- do.call(rbind, wyniki) %>% arrange(desc(AUC_test))
  write.csv(wyniki_df, file.path(results_dir, paste0("wyniki_klasyfikacja_", suffix, ".csv")), row.names = FALSE)
  print(wyniki_df)

  list(wyniki = wyniki_df, modele = list(glm = glm_full, tree_big = tree_big, tree_min = tree_min,
                                         tree_1se = tree_1se, bag = bag, rf = rf, mars1 = mars1, mars2 = mars2))
}

# ------------------------------------------------------------
# 4. Uruchomienie analiz
# ------------------------------------------------------------

log_msg("START: modele regresyjne")
wyniki_regresja_lista <- lapply(targety_regresja, analizuj_regresje)
names(wyniki_regresja_lista) <- targety_regresja
wyniki_regresja <- do.call(rbind, lapply(wyniki_regresja_lista, function(x) x$wyniki))
write.csv(wyniki_regresja, file.path(results_dir, "wyniki_regresja_lacznie.csv"), row.names = FALSE)
log_msg("KONIEC: modele regresyjne")

log_msg("START: modele klasyfikacyjne")
wyniki_klasyfikacja_lista <- lapply(targety_klasyfikacja, analizuj_klasyfikacje)
names(wyniki_klasyfikacja_lista) <- targety_klasyfikacja
wyniki_klasyfikacja <- do.call(rbind, lapply(wyniki_klasyfikacja_lista, function(x) x$wyniki))
write.csv(wyniki_klasyfikacja, file.path(results_dir, "wyniki_klasyfikacja_lacznie.csv"), row.names = FALSE)
log_msg("KONIEC: modele klasyfikacyjne")

czas_koniec_global <- Sys.time()
log_msg("KONIEC SKRYPTU | calkowity czas: ", round(as.numeric(difftime(czas_koniec_global, czas_start_global, units = "mins")), 2), " min")

