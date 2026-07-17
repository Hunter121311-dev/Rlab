# ============================================================
# BOOSTING 11* DLA DANYCH METEOROLOGICZNYCH
# Pelny wymiar notatnikow 11*, ale mniejsze zespoly jak w skrypcie lasow
#
# Zwykly skrypt R, bez R Markdown i bez argumentow komendy.
# Zakres:
#   - 2 targety regresyjne
#   - 2 targety klasyfikacyjne
#   - XGBoost + GBM3 dla kazdego targetu = 8 modeli glownych
#   - CV dla XGBoost i GBM3 na pelnym zbiorze treningowym
#   - PDP 1D i 2D dla kazdego modelu
#   - diagnostyka XGBoost i GBM3
#   - zapis metryk, predykcji, importance, wykresow, modeli i logu
#
# Rozmiar zespolow: 500 drzew/iteracji, analogicznie do skryptu lasow.
# ============================================================

# install.packages(c("dplyr", "lubridate", "xgboost", "pdp"))
# install.packages("remotes")
# remotes::install_github("gbm-developers/gbm3")

library(dplyr)
library(lubridate)
library(xgboost)

set.seed(2026)
options(stringsAsFactors = FALSE)

# ------------------------------------------------------------
# 0. Ustawienia - zwykly skrypt, bez argumentow komendy
# ------------------------------------------------------------

data_path <- "data/chalupy_hourly_10000_days.csv"
if (!file.exists(data_path)) {
  data_path <- "/Users/damia/OneDrive/Dokumenty/Rlab/stat/projekt/data/chalupy_hourly_10000_days.csv"
}
if (!file.exists(data_path)) {
  stop("Nie znaleziono pliku danych. Zmien zmienna data_path na wlasciwa sciezke.")
}

plot_dir <- "plots_boosting_11_pelny_wymiar_male_lasy"
results_dir <- "wyniki_boosting_11_pelny_wymiar_male_lasy"
models_dir <- file.path(results_dir, "modele")
logs_dir <- file.path(results_dir, "logi")
for (d in c(plot_dir, results_dir, models_dir, logs_dir)) {
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
}

# Jak w skrypcie lasow losowych.
liczba_watkow <- 14L
liczba_watkow <- min(liczba_watkow, max(1L, parallel::detectCores(logical = TRUE)))

# Mniejsze "lasy" boostingowe: 500 drzew/iteracji.
liczba_drzew_boosting <- 500L

# XGBoost - ustawienia szybkie, ale z pelnym CV/PDP/diagnostyka.
xgb_nrounds_max <- liczba_drzew_boosting
xgb_early_stopping <- 30L
xgb_print_every <- 25L
valid_frac <- 0.20
xgb_eta <- 0.05
xgb_max_depth <- 4L
xgb_min_child_weight <- 10
xgb_subsample <- 0.80
xgb_colsample_bytree <- 0.80
xgb_max_bin <- 256L

# CV w pelnym wymiarze danych treningowych.
# Jezeli ma byc literalnie jak pokaz GBM3 z notatnika, ustaw 10L.
# Domyslnie 5L to kompromis: pelne dane, ale bez mnozenia czasu x10.
xgb_cv_folds <- 5L
xgb_cv_nrounds <- liczba_drzew_boosting
xgb_cv_early_stopping <- 30L

gbm3_cv_folds <- 5L
gbm3_cv_trees <- liczba_drzew_boosting

# GBM3 - obecny dla tych samych targetow, 500 drzew.
uruchom_gbm3 <- TRUE
liczba_drzew_gbm3 <- liczba_drzew_boosting
gbm3_interaction_depth <- 2L
gbm3_shrinkage <- 0.01
gbm3_bag_fraction <- 0.70

# PDP pelnowymiarowe: bez probkowania, 1D i 2D dla dwoch najwazniejszych predyktorow.
uruchom_pdp <- TRUE
pdp_grid_resolution_1d <- 20L
pdp_grid_resolution_2d <- 12L
pdp_2d_chull <- TRUE

# Diagnostyka XGBoost: dump nie jest ciety do kilku drzew, ale zapisujemy do pliku.
uruchom_diagnostyke <- TRUE
zapisz_pelny_dump_xgb <- TRUE

# Cache - po przerwaniu nie licz od nowa.
zapisz_modele <- TRUE
zapisz_importance <- TRUE
zapisz_wykresy <- TRUE
uzyj_cache_modeli <- TRUE

# Progi eventow jak w skrypcie od lasow losowych.
wezel_na_kmh <- 1.852
prog_wind_speed_kmh <- 15 * wezel_na_kmh
prog_wind_gusts_kmh <- 25 * wezel_na_kmh

# Wykres predykcji dla krotkiego przedzialu testowego.
start_day_plot <- as.Date("2025-06-02")
ile_dni_plot <- 2L

# ------------------------------------------------------------
# 0.1. Log do konsoli i pliku
# ------------------------------------------------------------

log_file <- file.path(logs_dir, "boosting_11_pelny_wymiar_male_lasy_log.txt")
log_con <- file(log_file, open = "wt")
sink(log_con, split = TRUE)
on.exit({
  try(sink(), silent = TRUE)
  try(close(log_con), silent = TRUE)
}, add = TRUE)

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

cat("\n============================================================\n")
cat("BOOSTING 11* - PELNY WYMIAR, MALE LASY 500 DRZEW\n")
cat("Zwykly skrypt R, bez argumentow komendy\n")
cat("Start: ", format(Sys.time()), "\n", sep = "")
cat("Dane: ", data_path, "\n", sep = "")
cat("Wyniki: ", results_dir, "\n", sep = "")
cat("Wykresy: ", plot_dir, "\n", sep = "")
cat("Log: ", log_file, "\n", sep = "")
cat("============================================================\n\n")
flush.console()

# ------------------------------------------------------------
# 0.2. Funkcje pomocnicze
# ------------------------------------------------------------

bezpieczna_nazwa <- function(x) {
  x <- gsub("ą", "a", x); x <- gsub("ć", "c", x); x <- gsub("ę", "e", x)
  x <- gsub("ł", "l", x); x <- gsub("ń", "n", x); x <- gsub("ó", "o", x)
  x <- gsub("ś", "s", x); x <- gsub("ż", "z", x); x <- gsub("ź", "z", x)
  x <- gsub("Ą", "A", x); x <- gsub("Ć", "C", x); x <- gsub("Ę", "E", x)
  x <- gsub("Ł", "L", x); x <- gsub("Ń", "N", x); x <- gsub("Ó", "O", x)
  x <- gsub("Ś", "S", x); x <- gsub("Ż", "Z", x); x <- gsub("Ź", "Z", x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

zapisz_csv <- function(x, filename, row.names = FALSE) {
  path <- file.path(results_dir, filename)
  write.csv(x, path, row.names = row.names)
  cat("[SAVED_FILE] ", path, "\n", sep = "")
  flush.console()
  invisible(path)
}

zapisz_txt <- function(expr, filename) {
  path <- file.path(results_dir, filename)
  capture.output(force(expr), file = path)
  cat("[SAVED_FILE] ", path, "\n", sep = "")
  flush.console()
  invisible(path)
}

zapisz_model <- function(x, filename) {
  if (!zapisz_modele) return(invisible(NULL))
  path <- file.path(models_dir, filename)
  saveRDS(x, path)
  cat("[SAVED_FILE] ", path, "\n", sep = "")
  flush.console()
  invisible(path)
}

wczytaj_model_cache <- function(filename) {
  path <- file.path(models_dir, filename)
  if (uzyj_cache_modeli && file.exists(path)) {
    log_msg("CACHE: wczytuje model ", path)
    return(readRDS(path))
  }
  NULL
}

zapisz_wykres <- function(nazwa, kod, width = 1300, height = 850, res = 140) {
  if (!zapisz_wykresy) return(invisible(NULL))
  path <- file.path(plot_dir, nazwa)
  png(path, width = width, height = height, res = res)
  on.exit(dev.off(), add = TRUE)
  ok <- tryCatch({
    eval(substitute(kod), envir = parent.frame())
    TRUE
  }, error = function(e) {
    plot.new()
    title(main = paste("Blad wykresu:", nazwa))
    text(0.5, 0.5, conditionMessage(e))
    log_msg("BLAD WYKRESU: ", nazwa, " | ", conditionMessage(e))
    FALSE
  })
  cat("[SAVED_FILE] ", path, ifelse(ok, "", " [z ostrzezeniem]"), "\n", sep = "")
  flush.console()
  invisible(path)
}

mse <- function(y, y_hat) mean((y - y_hat)^2, na.rm = TRUE)
rmse <- function(y, y_hat) sqrt(mse(y, y_hat))
mae <- function(y, y_hat) mean(abs(y - y_hat), na.rm = TRUE)
r2_test <- function(y, y_hat) {
  denom <- sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
  if (denom == 0) return(NA_real_)
  1 - sum((y - y_hat)^2, na.rm = TRUE) / denom
}
metryki_regresji <- function(y, y_hat) {
  data.frame(MSE = mse(y, y_hat), RMSE = rmse(y, y_hat), MAE = mae(y, y_hat), R2 = r2_test(y, y_hat))
}

auc_roc <- function(y, p) {
  ok <- complete.cases(y, p)
  y <- y[ok]; p <- p[ok]
  n1 <- as.numeric(sum(y == 1))
  n0 <- as.numeric(sum(y == 0))
  if (n1 == 0 || n0 == 0) return(NA_real_)
  r <- rank(p, ties.method = "average")
  (sum(as.numeric(r[y == 1])) - n1 * (n1 + 1) / 2) / (n1 * n0)
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

przygotuj_x <- function(data, predyktory) {
  x <- as.data.frame(data[, predyktory, drop = FALSE])
  for (nm in names(x)) if (is.logical(x[[nm]])) x[[nm]] <- as.integer(x[[nm]])
  x
}

uzgodnij_kolumny <- function(mm, reference_cols) {
  missing_cols <- setdiff(reference_cols, colnames(mm))
  if (length(missing_cols) > 0) {
    zeros <- matrix(0, nrow = nrow(mm), ncol = length(missing_cols))
    colnames(zeros) <- missing_cols
    mm <- cbind(mm, zeros)
  }
  extra_cols <- setdiff(colnames(mm), reference_cols)
  if (length(extra_cols) > 0) mm <- mm[, setdiff(colnames(mm), extra_cols), drop = FALSE]
  mm[, reference_cols, drop = FALSE]
}

model_matrix_xgb <- function(data, predyktory, reference_cols = NULL) {
  x <- przygotuj_x(data, predyktory)
  mm <- model.matrix(~ . - 1, data = x)
  if (!is.null(reference_cols)) mm <- uzgodnij_kolumny(mm, reference_cols)
  storage.mode(mm) <- "double"
  mm
}

xgb_best_iteration_safe <- function(model) {
  best_iter <- model$best_iteration
  if (!is.null(best_iter) && length(best_iter) == 1L && !is.na(best_iter) && best_iter > 0) return(as.integer(best_iter))
  eval_log <- tryCatch(as.data.frame(model$evaluation_log), error = function(e) NULL)
  if (!is.null(eval_log) && nrow(eval_log) > 0) return(as.integer(nrow(eval_log)))
  niter <- tryCatch(model$niter, error = function(e) NULL)
  if (!is.null(niter) && length(niter) == 1L && !is.na(niter) && niter > 0) return(as.integer(niter))
  NA_integer_
}

predict_xgb_best <- function(model, dmat) {
  best_iter <- xgb_best_iteration_safe(model)
  if (!is.na(best_iter) && best_iter > 0) {
    pred <- tryCatch(
      predict(model, dmat, iterationrange = c(1, best_iter + 1L)),
      error = function(e1) {
        tryCatch(
          predict(model, dmat, iterationrange = c(1, best_iter)),
          error = function(e2) tryCatch(predict(model, dmat, ntreelimit = best_iter), error = function(e3) predict(model, dmat))
        )
      }
    )
    return(as.numeric(pred))
  }
  as.numeric(predict(model, dmat))
}

podziel_train_valid_chrono <- function(train_data, valid_frac = 0.2) {
  train_data <- train_data[order(train_data$datetime), ]
  n <- nrow(train_data)
  n_valid <- max(1L, floor(valid_frac * n))
  n_fit <- n - n_valid
  if (n_fit < 100) stop("Za malo danych treningowych do podzialu train/valid.")
  list(fit = train_data[seq_len(n_fit), , drop = FALSE],
       valid = train_data[(n_fit + 1L):n, , drop = FALSE])
}

buduj_formule <- function(target, predyktory) {
  as.formula(paste(target, "~", paste(predyktory, collapse = " + ")))
}

make_gbm3_params <- function(num_trees, n_train, n_features) {
  gbm3::training_params(
    num_trees = num_trees,
    interaction_depth = gbm3_interaction_depth,
    shrinkage = gbm3_shrinkage,
    num_train = n_train,
    num_features = n_features,
    bag_fraction = gbm3_bag_fraction
  )
}

predict_gbm3_response <- function(model, newdata, n_trees) {
  p <- tryCatch(
    predict(model, newdata, n.trees = n_trees, type = "response"),
    error = function(e1) tryCatch(predict(model, newdata, n.trees = n_trees), error = function(e2) predict(model, newdata))
  )
  as.numeric(p)
}

# ------------------------------------------------------------
# 0.3. CV, PDP, diagnostyka
# ------------------------------------------------------------

zapisz_learning_curve_xgb <- function(eval_log, suffix, metryka) {
  if (!zapisz_wykresy || is.null(eval_log) || nrow(eval_log) == 0) return(invisible(NULL))
  train_col <- paste0("train_", metryka)
  valid_col <- paste0("valid_", metryka)
  if (!all(c(train_col, valid_col) %in% names(eval_log))) return(invisible(NULL))
  zapisz_wykres(paste0("xgb_learning_curve_", suffix, ".png"), {
    plot(eval_log[[train_col]], type = "l", xlab = "Iteracja", ylab = metryka,
         main = paste("XGBoost learning curve:", suffix))
    lines(eval_log[[valid_col]], lty = 2)
    legend("topright", legend = c("train", "valid"), lty = c(1, 2), bty = "n")
  })
}

zapisz_cv_curve_xgb <- function(eval_log, suffix, metryka) {
  if (!zapisz_wykresy || is.null(eval_log) || nrow(eval_log) == 0) return(invisible(NULL))
  train_col <- paste0("train_", metryka, "_mean")
  test_col <- paste0("test_", metryka, "_mean")
  if (!all(c(train_col, test_col) %in% names(eval_log))) return(invisible(NULL))
  zapisz_wykres(paste0("xgb_cv_curve_", suffix, ".png"), {
    plot(eval_log[[train_col]], type = "l", xlab = "Iteracja", ylab = metryka,
         main = paste("XGBoost CV:", suffix))
    lines(eval_log[[test_col]], lty = 2)
    legend("topright", legend = c("train CV", "test CV"), lty = c(1, 2), bty = "n")
  })
}

zapisz_importance_xgb <- function(model, feature_names, filename, model_name, target, top_n = 12L) {
  if (!zapisz_importance) return(NULL)
  imp <- xgboost::xgb.importance(feature_names = feature_names, model = model)
  if (nrow(imp) == 0) return(NULL)
  imp$model <- model_name
  imp$target <- target
  zapisz_csv(imp, filename)
  cat("\nTop importance - ", model_name, " - ", target, ":\n", sep = "")
  print(head(imp, top_n))
  invisible(imp)
}

zapisz_importance_gbm3 <- function(model, filename, model_name, target, top_n = 12L) {
  if (!zapisz_importance) return(NULL)
  imp <- tryCatch(
    suppressWarnings(summary(model, plotit = FALSE)),
    error = function(e1) tryCatch(suppressWarnings(summary(model)), error = function(e2) NULL)
  )
  if (is.null(imp)) return(NULL)
  if (is.data.frame(imp)) {
    imp$model <- model_name
    imp$target <- target
    zapisz_csv(imp, filename, row.names = FALSE)
    cat("\nTop importance - ", model_name, " - ", target, ":\n", sep = "")
    print(head(imp, top_n))
    return(invisible(imp))
  }
  zapisz_txt(print(imp), sub("\\.csv$", ".txt", filename))
  invisible(imp)
}

wybierz_top_features <- function(importance_obj, fallback, n = 2L) {
  if (is.null(importance_obj)) return(head(fallback, n))
  if (is.data.frame(importance_obj) && nrow(importance_obj) > 0) {
    for (col in c("Feature", "var", "Variable")) {
      if (col %in% names(importance_obj)) {
        vals <- as.character(importance_obj[[col]])
        vals <- vals[vals %in% fallback]
        if (length(vals) > 0) return(unique(c(vals, fallback))[seq_len(min(n, length(unique(c(vals, fallback)))) )])
      }
    }
  }
  head(fallback, n)
}

zapisz_diagnostyke_xgb <- function(model, suffix) {
  if (!uruchom_diagnostyke) return(invisible(NULL))
  zapisz_txt(print(model), paste0("diagnostyka_xgb_print_", suffix, ".txt"))
  tree_dt <- tryCatch(xgboost::xgb.model.dt.tree(model = model), error = function(e) NULL)
  if (!is.null(tree_dt)) zapisz_csv(as.data.frame(tree_dt), paste0("diagnostyka_xgb_tree_table_", suffix, ".csv"))
  dump <- tryCatch(xgboost::xgb.dump(model, with_stats = TRUE), error = function(e) character())
  if (length(dump) > 0 && zapisz_pelny_dump_xgb) {
    path <- file.path(results_dir, paste0("diagnostyka_xgb_dump_wszystkie_drzewa_", suffix, ".txt"))
    writeLines(dump, path)
    cat("[SAVED_FILE] ", path, "\n", sep = "")
  }
  if (zapisz_wykresy) {
    try(zapisz_wykres(paste0("xgb_deepness_", suffix, ".png"), { xgboost::xgb.plot.deepness(model) }), silent = TRUE)
  }
}

zapisz_diagnostyke_gbm3 <- function(model, suffix) {
  if (!uruchom_diagnostyke) return(invisible(NULL))
  zapisz_txt(print(model), paste0("diagnostyka_gbm3_print_", suffix, ".txt"))
  if (zapisz_wykresy) {
    try(zapisz_wykres(paste0("gbm3_performance_", suffix, ".png"), { plot(gbm3::gbmt_performance(model)) }), silent = TRUE)
  }
}

zrob_cv_xgb <- function(target, train_data, predyktory, typ) {
  suffix <- paste0(typ, "_", bezpieczna_nazwa(target))
  log_msg("CV XGBoost pelne dane: ", target, " | typ = ", typ)
  if (typ == "regresja") {
    y <- train_data[[target]]
    metric <- "rmse"
    params <- list(
      objective = "reg:squarederror", eval_metric = metric,
      eta = xgb_eta, max_depth = xgb_max_depth, min_child_weight = xgb_min_child_weight,
      subsample = xgb_subsample, colsample_bytree = xgb_colsample_bytree,
      tree_method = "hist", max_bin = xgb_max_bin, nthread = liczba_watkow, seed = 2026
    )
    stratified <- FALSE
  } else {
    y <- as.integer(train_data[[target]] == 1)
    metric <- "logloss"
    pos <- sum(y == 1); neg <- sum(y == 0)
    scale_pos_weight <- ifelse(pos > 0, neg / pos, 1)
    params <- list(
      objective = "binary:logistic", eval_metric = metric,
      eta = xgb_eta, max_depth = xgb_max_depth, min_child_weight = xgb_min_child_weight,
      subsample = xgb_subsample, colsample_bytree = xgb_colsample_bytree,
      tree_method = "hist", max_bin = xgb_max_bin, nthread = liczba_watkow,
      scale_pos_weight = scale_pos_weight, seed = 2026
    )
    stratified <- TRUE
  }
  x <- model_matrix_xgb(train_data, predyktory)
  d <- xgb.DMatrix(data = x, label = y)
  cv <- czasuj(paste("CV XGBoost pelne dane", target), xgb.cv(
    params = params, data = d, nrounds = xgb_cv_nrounds, nfold = xgb_cv_folds,
    early_stopping_rounds = xgb_cv_early_stopping, maximize = FALSE,
    stratified = stratified, verbose = 1, print_every_n = xgb_print_every
  ))
  eval_log <- as.data.frame(cv$evaluation_log)
  zapisz_csv(eval_log, paste0("cv_xgb_", suffix, ".csv"))
  zapisz_cv_curve_xgb(eval_log, suffix, metric)
  best_iter <- if (!is.null(cv$best_iteration) && length(cv$best_iteration) > 0) cv$best_iteration else nrow(eval_log)
  summary_df <- data.frame(target = target, model = "XGBoost", typ = typ,
                           cv_folds = xgb_cv_folds, cv_rows = nrow(train_data),
                           cv_trees_max = xgb_cv_nrounds, best_iteration_cv = best_iter)
  zapisz_csv(summary_df, paste0("cv_xgb_summary_", suffix, ".csv"))
  cat("\n[CHAPTER_CREATOR_BEGIN]\n")
  cat("Pelna walidacja krzyzowa XGBoost dla targetu: ", target, "\n", sep = "")
  print(summary_df)
  if (nrow(eval_log) > 0) print(tail(eval_log, 3))
  cat("[CHAPTER_CREATOR_END]\n")
  invisible(summary_df)
}

zrob_cv_gbm3_regresja <- function(target, train_data, predyktory) {
  if (!HAS_GBM3) return(NULL)
  suffix <- paste0("regresja_", bezpieczna_nazwa(target))
  log_msg("CV GBM3 pelne dane regresja: ", target)
  mu <- mean(train_data[[target]], na.rm = TRUE)
  sig <- sd(train_data[[target]], na.rm = TRUE)
  if (is.na(sig) || sig == 0) sig <- 1
  target_scaled <- paste0(target, "_scaled_cv")
  cv_data <- train_data
  cv_data[[target_scaled]] <- (cv_data[[target]] - mu) / sig
  form <- buduj_formule(target_scaled, predyktory)
  params <- make_gbm3_params(gbm3_cv_trees, nrow(cv_data), length(predyktory))
  model_cv <- czasuj(paste("CV GBM3 pelne dane regresja", target), gbm3::gbmt(
    form, data = cv_data, train_params = params,
    distribution = gbm3::gbm_dist("Gaussian"), cv_folds = gbm3_cv_folds
  ))
  zapisz_model(model_cv, paste0("gbm3_cv_regresja_", bezpieczna_nazwa(target), ".rds"))
  zapisz_diagnostyke_gbm3(model_cv, paste0("cv_regresja_", bezpieczna_nazwa(target)))
  summary_df <- data.frame(target = target, model = "GBM3", typ = "regresja",
                           cv_folds = gbm3_cv_folds, cv_rows = nrow(cv_data),
                           cv_trees = gbm3_cv_trees, target_scaled = TRUE)
  zapisz_csv(summary_df, paste0("cv_gbm3_summary_", suffix, ".csv"))
  cat("\n[CHAPTER_CREATOR_BEGIN]\n")
  cat("Pelna walidacja krzyzowa GBM3 dla targetu: ", target, "\n", sep = "")
  print(summary_df)
  print(model_cv)
  cat("[CHAPTER_CREATOR_END]\n")
  invisible(summary_df)
}

zrob_cv_gbm3_klasyfikacja <- function(target, train_data, predyktory) {
  if (!HAS_GBM3) return(NULL)
  suffix <- paste0("klasyfikacja_", bezpieczna_nazwa(target))
  log_msg("CV GBM3 pelne dane klasyfikacja: ", target)
  cv_data <- train_data
  cv_data[[target]] <- factor(ifelse(cv_data[[target]] == 1, "Tak", "Nie"), levels = c("Nie", "Tak"))
  form <- buduj_formule(target, predyktory)
  params <- make_gbm3_params(gbm3_cv_trees, nrow(cv_data), length(predyktory))
  model_cv <- czasuj(paste("CV GBM3 pelne dane klasyfikacja", target), gbm3::gbmt(
    form, data = cv_data, train_params = params,
    distribution = gbm3::gbm_dist("Bernoulli"), cv_folds = gbm3_cv_folds
  ))
  zapisz_model(model_cv, paste0("gbm3_cv_klasyfikacja_", bezpieczna_nazwa(target), ".rds"))
  zapisz_diagnostyke_gbm3(model_cv, paste0("cv_klasyfikacja_", bezpieczna_nazwa(target)))
  summary_df <- data.frame(target = target, model = "GBM3", typ = "klasyfikacja",
                           cv_folds = gbm3_cv_folds, cv_rows = nrow(cv_data), cv_trees = gbm3_cv_trees)
  zapisz_csv(summary_df, paste0("cv_gbm3_summary_", suffix, ".csv"))
  cat("\n[CHAPTER_CREATOR_BEGIN]\n")
  cat("Pelna walidacja krzyzowa GBM3 dla targetu: ", target, "\n", sep = "")
  print(summary_df)
  print(model_cv)
  cat("[CHAPTER_CREATOR_END]\n")
  invisible(summary_df)
}

zapisz_pdp_xgb <- function(model, train_data, predyktory, feature_cols, top_features, suffix, typ) {
  if (!uruchom_pdp) return(invisible(NULL))
  if (!requireNamespace("pdp", quietly = TRUE)) {
    log_msg("Pakiet pdp nie jest zainstalowany - pomijam PDP XGBoost: ", suffix)
    return(invisible(NULL))
  }
  top_features <- top_features[top_features %in% predyktory]
  if (length(top_features) == 0) top_features <- predyktory[1]
  pdp_data <- train_data[, predyktory, drop = FALSE]
  pred_fun <- function(object, newdata) {
    mm <- model_matrix_xgb(newdata, predyktory, reference_cols = feature_cols)
    d <- xgb.DMatrix(data = mm)
    predict_xgb_best(object, d)
  }
  # PDP 1D
  f1 <- top_features[1]
  pd1 <- tryCatch(pdp::partial(model, pred.var = f1, train = pdp_data, pred.fun = pred_fun,
                               grid.resolution = pdp_grid_resolution_1d, type = "regression"),
                  error = function(e) { log_msg("BLAD PDP 1D XGBoost ", suffix, ": ", conditionMessage(e)); NULL })
  if (!is.null(pd1)) {
    zapisz_csv(as.data.frame(pd1), paste0("pdp_1d_xgb_", suffix, "_", bezpieczna_nazwa(f1), ".csv"))
    zapisz_wykres(paste0("pdp_1d_xgb_", suffix, "_", bezpieczna_nazwa(f1), ".png"), {
      plot(pd1, type = "l", main = paste("PDP 1D XGBoost:", suffix, "|", f1),
           xlab = f1, ylab = ifelse(typ == "klasyfikacja", "P(klasa pozytywna)", "predykcja"))
    })
  }
  # PDP 2D
  if (length(top_features) >= 2) {
    f2 <- top_features[2]
    pd2 <- tryCatch(pdp::partial(model, pred.var = c(f1, f2), train = pdp_data, pred.fun = pred_fun,
                                 grid.resolution = pdp_grid_resolution_2d, type = "regression", chull = pdp_2d_chull),
                    error = function(e) { log_msg("BLAD PDP 2D XGBoost ", suffix, ": ", conditionMessage(e)); NULL })
    if (!is.null(pd2)) {
      zapisz_csv(as.data.frame(pd2), paste0("pdp_2d_xgb_", suffix, "_", bezpieczna_nazwa(f1), "_", bezpieczna_nazwa(f2), ".csv"))
      zapisz_wykres(paste0("pdp_2d_xgb_", suffix, "_", bezpieczna_nazwa(f1), "_", bezpieczna_nazwa(f2), ".png"), {
        plot(pd2, main = paste("PDP 2D XGBoost:", suffix, "|", f1, "+", f2))
      })
    }
  }
}

zapisz_pdp_gbm3 <- function(model, train_data, predyktory, top_features, suffix, typ, n_trees, backtransform = NULL) {
  if (!uruchom_pdp) return(invisible(NULL))
  if (!requireNamespace("pdp", quietly = TRUE)) {
    log_msg("Pakiet pdp nie jest zainstalowany - pomijam PDP GBM3: ", suffix)
    return(invisible(NULL))
  }
  top_features <- top_features[top_features %in% predyktory]
  if (length(top_features) == 0) top_features <- predyktory[1]
  pdp_data <- train_data[, predyktory, drop = FALSE]
  pred_fun <- function(object, newdata) {
    p <- predict_gbm3_response(object, newdata, n_trees)
    if (!is.null(backtransform)) p <- p * backtransform$sigma + backtransform$mu
    p
  }
  f1 <- top_features[1]
  pd1 <- tryCatch(pdp::partial(model, pred.var = f1, train = pdp_data, pred.fun = pred_fun,
                               grid.resolution = pdp_grid_resolution_1d, type = "regression"),
                  error = function(e) { log_msg("BLAD PDP 1D GBM3 ", suffix, ": ", conditionMessage(e)); NULL })
  if (!is.null(pd1)) {
    zapisz_csv(as.data.frame(pd1), paste0("pdp_1d_gbm3_", suffix, "_", bezpieczna_nazwa(f1), ".csv"))
    zapisz_wykres(paste0("pdp_1d_gbm3_", suffix, "_", bezpieczna_nazwa(f1), ".png"), {
      plot(pd1, type = "l", main = paste("PDP 1D GBM3:", suffix, "|", f1),
           xlab = f1, ylab = ifelse(typ == "klasyfikacja", "P(klasa pozytywna)", "predykcja"))
    })
  }
  if (length(top_features) >= 2) {
    f2 <- top_features[2]
    pd2 <- tryCatch(pdp::partial(model, pred.var = c(f1, f2), train = pdp_data, pred.fun = pred_fun,
                                 grid.resolution = pdp_grid_resolution_2d, type = "regression", chull = pdp_2d_chull),
                    error = function(e) { log_msg("BLAD PDP 2D GBM3 ", suffix, ": ", conditionMessage(e)); NULL })
    if (!is.null(pd2)) {
      zapisz_csv(as.data.frame(pd2), paste0("pdp_2d_gbm3_", suffix, "_", bezpieczna_nazwa(f1), "_", bezpieczna_nazwa(f2), ".csv"))
      zapisz_wykres(paste0("pdp_2d_gbm3_", suffix, "_", bezpieczna_nazwa(f1), "_", bezpieczna_nazwa(f2), ".png"), {
        plot(pd2, main = paste("PDP 2D GBM3:", suffix, "|", f1, "+", f2))
      })
    }
  }
}

# ------------------------------------------------------------
# 1. Wczytanie i przygotowanie danych - jak w skrypcie lasow
# ------------------------------------------------------------

log_msg("Wczytywanie danych: ", data_path)
dane <- read.csv(data_path)
if (!("datetime" %in% names(dane))) stop("Brak kolumny datetime w danych.")

dane$datetime <- as.POSIXct(dane$datetime, format = "%Y-%m-%dT%H:%M")
dane <- dane %>% arrange(datetime)

dane$data <- as.Date(dane$datetime)
dane$rok <- year(dane$datetime)
dane$miesiac <- month(dane$datetime)
dane$dzien_roku <- yday(dane$datetime)
dane$godzina <- hour(dane$datetime)

wymagane_kolumny <- c("wind_speed_10m", "wind_gusts_10m")
brak <- setdiff(wymagane_kolumny, names(dane))
if (length(brak) > 0) stop("Brak wymaganych kolumn: ", paste(brak, collapse = ", "))

dane$wind_speed_10m_next <- lead(dane$wind_speed_10m, 1)
dane$wind_gusts_10m_next <- lead(dane$wind_gusts_10m, 1)

dane$sin_dzien <- sin(2 * pi * dane$dzien_roku / 365)
dane$cos_dzien <- cos(2 * pi * dane$dzien_roku / 365)
dane$sin_godzina <- sin(2 * pi * dane$godzina / 24)
dane$cos_godzina <- cos(2 * pi * dane$godzina / 24)

if ("wind_direction_10m" %in% names(dane)) {
  dane$wind_dir_sin <- sin(2 * pi * dane$wind_direction_10m / 360)
  dane$wind_dir_cos <- cos(2 * pi * dane$wind_direction_10m / 360)
}

dane$polrocze_cieple <- ifelse(dane$miesiac %in% c(4, 5, 6, 7, 8, 9), 1, 0)

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

HAS_GBM3 <- uruchom_gbm3 && requireNamespace("gbm3", quietly = TRUE)
if (uruchom_gbm3 && !HAS_GBM3) log_msg("Pakiet gbm3 nie jest zainstalowany - pomijam GBM3.")
HAS_PDP <- uruchom_pdp && requireNamespace("pdp", quietly = TRUE)
if (uruchom_pdp && !HAS_PDP) log_msg("Pakiet pdp nie jest zainstalowany - pomijam PDP.")

plan_obliczen <- data.frame(
  element = c(
    "targety_regresyjne", "targety_klasyfikacyjne", "modele_glowne_na_target",
    "liczba_modeli_glownych_lacznie", "liczba_drzew_na_model_glowny",
    "maksymalna_liczba_drzew_modeli_glownych", "liczba_watkow_xgboost",
    "xgb_cv_pelne_dane", "xgb_cv_folds", "gbm3_wlaczone", "gbm3_cv_pelne_dane",
    "gbm3_cv_folds", "pdp_1d_pelne_dane", "pdp_2d_pelne_dane", "diagnostyka_pelna"
  ),
  wartosc = c(
    length(targety_regresja), length(targety_klasyfikacja), ifelse(HAS_GBM3, 2, 1),
    (length(targety_regresja) + length(targety_klasyfikacja)) * ifelse(HAS_GBM3, 2, 1),
    liczba_drzew_boosting,
    (length(targety_regresja) + length(targety_klasyfikacja)) * ifelse(HAS_GBM3, 2, 1) * liczba_drzew_boosting,
    liczba_watkow, TRUE, xgb_cv_folds, HAS_GBM3, HAS_GBM3, gbm3_cv_folds,
    HAS_PDP, HAS_PDP, uruchom_diagnostyke
  )
)
zapisz_csv(plan_obliczen, "plan_obliczen.csv")
zapisz_csv(data.frame(predyktor = predyktory), "lista_predyktorow.csv")

cat("\n[CHAPTER_CREATOR_BEGIN]\n")
cat("Opis konfiguracji obliczen - pelny wymiar na malych lasach:\n")
print(plan_obliczen)
cat("\nPredyktory uzyte w modelach:\n")
print(data.frame(predyktor = predyktory))
cat("\nTargety regresyjne:\n")
print(targety_regresja)
cat("\nTargety klasyfikacyjne:\n")
print(targety_klasyfikacja)
cat("\nLiczba obserwacji po wczytaniu: ", nrow(dane), "\n", sep = "")
cat("[CHAPTER_CREATOR_END]\n\n")
flush.console()

# ------------------------------------------------------------
# 2. XGBoost - regresja
# ------------------------------------------------------------

analizuj_regresje_xgb <- function(target) {
  suffix <- bezpieczna_nazwa(target)
  p <- length(predyktory)

  dane_model <- dane %>%
    select(data, datetime, all_of(target), all_of(predyktory)) %>%
    filter(complete.cases(.)) %>%
    arrange(datetime)

  train_data <- dane_model %>% filter(data < as.Date("2025-01-01"))
  test_data <- dane_model %>% filter(data >= as.Date("2025-01-01"))
  split <- podziel_train_valid_chrono(train_data, valid_frac = valid_frac)
  fit_data <- split$fit
  valid_data <- split$valid

  zrob_cv_xgb(target, train_data, predyktory, typ = "regresja")

  y_fit <- fit_data[[target]]
  y_valid <- valid_data[[target]]
  y_train <- train_data[[target]]
  y_test <- test_data[[target]]

  x_fit <- model_matrix_xgb(fit_data, predyktory)
  feature_cols <- colnames(x_fit)
  x_valid <- model_matrix_xgb(valid_data, predyktory, reference_cols = feature_cols)
  x_train <- model_matrix_xgb(train_data, predyktory, reference_cols = feature_cols)
  x_test <- model_matrix_xgb(test_data, predyktory, reference_cols = feature_cols)

  dfit <- xgb.DMatrix(data = x_fit, label = y_fit)
  dvalid <- xgb.DMatrix(data = x_valid, label = y_valid)
  dtrain_all <- xgb.DMatrix(data = x_train, label = y_train)
  dtest <- xgb.DMatrix(data = x_test, label = y_test)

  cat("\n============================================================\n")
  cat("REGRESJA XGBOOST: ", target, "\n", sep = "")
  cat("train: ", nrow(train_data), " | fit: ", nrow(fit_data), " | valid: ", nrow(valid_data),
      " | test: ", nrow(test_data), " | predyktory: ", p, "\n", sep = "")
  cat("XGBoost: max ", xgb_nrounds_max, " rund, early stopping = ", xgb_early_stopping,
      ", watki = ", liczba_watkow, ", tree_method = hist\n", sep = "")
  cat("============================================================\n")
  flush.console()

  params <- list(
    objective = "reg:squarederror", eval_metric = "rmse",
    eta = xgb_eta, max_depth = xgb_max_depth, min_child_weight = xgb_min_child_weight,
    subsample = xgb_subsample, colsample_bytree = xgb_colsample_bytree,
    tree_method = "hist", max_bin = xgb_max_bin, nthread = liczba_watkow, seed = 2026
  )

  model_file <- paste0("xgb_regresja_", suffix, ".rds")
  model <- wczytaj_model_cache(model_file)
  if (is.null(model)) {
    model <- czasuj(paste("XGBoost regresja", target), xgb.train(
      params = params, data = dfit, nrounds = xgb_nrounds_max,
      evals = list(train = dfit, valid = dvalid),
      early_stopping_rounds = xgb_early_stopping, maximize = FALSE,
      verbose = 1, print_every_n = xgb_print_every
    ))
    zapisz_model(model, model_file)
    saveRDS(feature_cols, file.path(models_dir, paste0("xgb_regresja_", suffix, "_kolumny_model_matrix.rds")))
  }

  pred_null_train <- rep(mean(y_fit), length(y_train))
  pred_null_test <- rep(mean(y_fit), length(y_test))
  pred_train <- predict_xgb_best(model, dtrain_all)
  pred_valid <- predict_xgb_best(model, dvalid)
  pred_test <- predict_xgb_best(model, dtest)

  met_null_train <- metryki_regresji(y_train, pred_null_train)
  met_null_test <- metryki_regresji(y_test, pred_null_test)
  met_train <- metryki_regresji(y_train, pred_train)
  met_valid <- metryki_regresji(y_valid, pred_valid)
  met_test <- metryki_regresji(y_test, pred_test)

  wyniki_df <- rbind(
    data.frame(target = target, model = "Model zerowy", typ_modelu = "bazowy", best_iteration = NA_integer_,
               MSE_train = met_null_train$MSE, RMSE_train = met_null_train$RMSE, MAE_train = met_null_train$MAE, R2_train = met_null_train$R2,
               MSE_valid = NA_real_, RMSE_valid = NA_real_, MAE_valid = NA_real_, R2_valid = NA_real_,
               MSE_test = met_null_test$MSE, RMSE_test = met_null_test$RMSE, MAE_test = met_null_test$MAE, R2_test = met_null_test$R2),
    data.frame(target = target, model = "XGBoost", typ_modelu = "xgboost", best_iteration = xgb_best_iteration_safe(model),
               MSE_train = met_train$MSE, RMSE_train = met_train$RMSE, MAE_train = met_train$MAE, R2_train = met_train$R2,
               MSE_valid = met_valid$MSE, RMSE_valid = met_valid$RMSE, MAE_valid = met_valid$MAE, R2_valid = met_valid$R2,
               MSE_test = met_test$MSE, RMSE_test = met_test$RMSE, MAE_test = met_test$MAE, R2_test = met_test$R2)
  )

  zapisz_csv(wyniki_df, paste0("wyniki_regresja_xgb_", suffix, ".csv"))
  cat("\n[CHAPTER_CREATOR_BEGIN]\n")
  cat("Wyniki regresji XGBoost dla targetu: ", target, "\n", sep = "")
  print(wyniki_df)
  cat("[CHAPTER_CREATOR_END]\n")

  predykcje <- data.frame(datetime = test_data$datetime, rzeczywiste = y_test,
                          xgboost = pred_test, model_zerowy = pred_null_test,
                          residuum_xgboost = y_test - pred_test)
  zapisz_csv(predykcje, paste0("predykcje_regresja_xgb_", suffix, ".csv"))

  eval_log <- as.data.frame(model$evaluation_log)
  zapisz_csv(eval_log, paste0("evaluation_log_regresja_xgb_", suffix, ".csv"))
  zapisz_learning_curve_xgb(eval_log, paste0("regresja_", suffix), "rmse")
  imp <- zapisz_importance_xgb(model, feature_cols, paste0("importance_regresja_xgb_", suffix, ".csv"), "XGBoost", target)
  top_features <- wybierz_top_features(imp, predyktory, 2L)
  zapisz_pdp_xgb(model, train_data, predyktory, feature_cols, top_features, paste0("regresja_", suffix), "regresja")
  zapisz_diagnostyke_xgb(model, paste0("regresja_", suffix))

  start_day <- start_day_plot
  if (!any(as.Date(predykcje$datetime) >= start_day & as.Date(predykcje$datetime) <= start_day + ile_dni_plot)) {
    start_day <- as.Date(min(predykcje$datetime))
  }
  end_day <- start_day + ile_dni_plot
  wykres_df <- predykcje[as.Date(predykcje$datetime) >= start_day & as.Date(predykcje$datetime) <= end_day, ]
  zapisz_wykres(paste0("predykcje_regresja_xgb_", suffix, ".png"), {
    plot(wykres_df$datetime, wykres_df$rzeczywiste, pch = 16, xlab = "Czas", ylab = target,
         main = paste("XGBoost: rzeczywiste i predykcja -", target))
    lines(wykres_df$datetime, wykres_df$xgboost, lwd = 2)
    lines(wykres_df$datetime, wykres_df$model_zerowy, lwd = 2, lty = 2)
    legend("topright", legend = c("rzeczywiste", "XGBoost", "model zerowy"),
           pch = c(16, NA, NA), lty = c(NA, 1, 2), bty = "n")
  })

  list(wyniki = wyniki_df, model = model)
}

# ------------------------------------------------------------
# 3. XGBoost - klasyfikacja
# ------------------------------------------------------------

analizuj_klasyfikacje_xgb <- function(target) {
  suffix <- bezpieczna_nazwa(target)
  p <- length(predyktory)

  dane_model <- dane %>%
    select(data, datetime, all_of(target), all_of(predyktory)) %>%
    filter(complete.cases(.)) %>%
    arrange(datetime)
  dane_model[[target]] <- as.integer(dane_model[[target]] == 1)

  train_data <- dane_model %>% filter(data < as.Date("2025-01-01"))
  test_data <- dane_model %>% filter(data >= as.Date("2025-01-01"))
  split <- podziel_train_valid_chrono(train_data, valid_frac = valid_frac)
  fit_data <- split$fit
  valid_data <- split$valid

  zrob_cv_xgb(target, train_data, predyktory, typ = "klasyfikacja")

  y_fit <- fit_data[[target]]
  y_valid <- valid_data[[target]]
  y_train <- train_data[[target]]
  y_test <- test_data[[target]]

  x_fit <- model_matrix_xgb(fit_data, predyktory)
  feature_cols <- colnames(x_fit)
  x_valid <- model_matrix_xgb(valid_data, predyktory, reference_cols = feature_cols)
  x_train <- model_matrix_xgb(train_data, predyktory, reference_cols = feature_cols)
  x_test <- model_matrix_xgb(test_data, predyktory, reference_cols = feature_cols)

  dfit <- xgb.DMatrix(data = x_fit, label = y_fit)
  dvalid <- xgb.DMatrix(data = x_valid, label = y_valid)
  dtrain_all <- xgb.DMatrix(data = x_train, label = y_train)
  dtest <- xgb.DMatrix(data = x_test, label = y_test)

  pos <- sum(y_fit == 1)
  neg <- sum(y_fit == 0)
  scale_pos_weight <- ifelse(pos > 0, neg / pos, 1)

  cat("\n============================================================\n")
  cat("KLASYFIKACJA XGBOOST: ", target, "\n", sep = "")
  cat("train: ", nrow(train_data), " | fit: ", nrow(fit_data), " | valid: ", nrow(valid_data),
      " | test: ", nrow(test_data), " | predyktory: ", p, "\n", sep = "")
  cat("XGBoost: max ", xgb_nrounds_max, " rund, early stopping = ", xgb_early_stopping,
      ", watki = ", liczba_watkow, ", tree_method = hist\n", sep = "")
  cat("============================================================\n")
  flush.console()

  rozklad <- data.frame(target = target, zbior = c("fit", "valid", "train", "test"),
                        n = c(length(y_fit), length(y_valid), length(y_train), length(y_test)),
                        pozytywne = c(sum(y_fit == 1), sum(y_valid == 1), sum(y_train == 1), sum(y_test == 1)),
                        udzial_pozytywnych = c(mean(y_fit == 1), mean(y_valid == 1), mean(y_train == 1), mean(y_test == 1)))
  zapisz_csv(rozklad, paste0("rozklad_klas_xgb_", suffix, ".csv"))
  print(rozklad)

  params <- list(
    objective = "binary:logistic", eval_metric = "logloss",
    eta = xgb_eta, max_depth = xgb_max_depth, min_child_weight = xgb_min_child_weight,
    subsample = xgb_subsample, colsample_bytree = xgb_colsample_bytree,
    tree_method = "hist", max_bin = xgb_max_bin, nthread = liczba_watkow,
    scale_pos_weight = scale_pos_weight, seed = 2026
  )

  model_file <- paste0("xgb_klasyfikacja_", suffix, ".rds")
  model <- wczytaj_model_cache(model_file)
  if (is.null(model)) {
    model <- czasuj(paste("XGBoost klasyfikacja", target), xgb.train(
      params = params, data = dfit, nrounds = xgb_nrounds_max,
      evals = list(train = dfit, valid = dvalid),
      early_stopping_rounds = xgb_early_stopping, maximize = FALSE,
      verbose = 1, print_every_n = xgb_print_every
    ))
    zapisz_model(model, model_file)
    saveRDS(feature_cols, file.path(models_dir, paste0("xgb_klasyfikacja_", suffix, "_kolumny_model_matrix.rds")))
  }

  p_null_train <- rep(mean(y_fit), length(y_train))
  p_null_test <- rep(mean(y_fit), length(y_test))
  p_train <- predict_xgb_best(model, dtrain_all)
  p_valid <- predict_xgb_best(model, dvalid)
  p_test <- predict_xgb_best(model, dtest)

  prog <- wybierz_prog_youden(y_valid, p_valid)
  met_null_train <- metryki_klasyfikacji(y_train, p_null_train, prog = 0.5)
  met_null_test <- metryki_klasyfikacji(y_test, p_null_test, prog = 0.5)
  met_train <- metryki_klasyfikacji(y_train, p_train, prog = prog)
  met_valid <- metryki_klasyfikacji(y_valid, p_valid, prog = prog)
  met_test <- metryki_klasyfikacji(y_test, p_test, prog = prog)

  wyniki_df <- rbind(
    data.frame(target = target, model = "Model zerowy", typ_modelu = "bazowy", best_iteration = NA_integer_, prog_decyzyjny = 0.5,
               AUC_train = auc_roc(y_train, p_null_train), Brier_train = brier_score(y_train, p_null_train), LogLoss_train = log_loss(y_train, p_null_train), accuracy_train = met_null_train$accuracy,
               AUC_valid = NA_real_, Brier_valid = NA_real_, LogLoss_valid = NA_real_, accuracy_valid = NA_real_,
               AUC_test = auc_roc(y_test, p_null_test), Brier_test = brier_score(y_test, p_null_test), LogLoss_test = log_loss(y_test, p_null_test), accuracy_test = met_null_test$accuracy,
               sensitivity_test = met_null_test$sensitivity, specificity_test = met_null_test$specificity, precision_test = met_null_test$precision, F1_test = met_null_test$F1,
               TP_test = met_null_test$TP, TN_test = met_null_test$TN, FP_test = met_null_test$FP, FN_test = met_null_test$FN),
    data.frame(target = target, model = "XGBoost", typ_modelu = "xgboost", best_iteration = xgb_best_iteration_safe(model), prog_decyzyjny = prog,
               AUC_train = auc_roc(y_train, p_train), Brier_train = brier_score(y_train, p_train), LogLoss_train = log_loss(y_train, p_train), accuracy_train = met_train$accuracy,
               AUC_valid = auc_roc(y_valid, p_valid), Brier_valid = brier_score(y_valid, p_valid), LogLoss_valid = log_loss(y_valid, p_valid), accuracy_valid = met_valid$accuracy,
               AUC_test = auc_roc(y_test, p_test), Brier_test = brier_score(y_test, p_test), LogLoss_test = log_loss(y_test, p_test), accuracy_test = met_test$accuracy,
               sensitivity_test = met_test$sensitivity, specificity_test = met_test$specificity, precision_test = met_test$precision, F1_test = met_test$F1,
               TP_test = met_test$TP, TN_test = met_test$TN, FP_test = met_test$FP, FN_test = met_test$FN)
  )

  zapisz_csv(wyniki_df, paste0("wyniki_klasyfikacja_xgb_", suffix, ".csv"))
  cat("\n[CHAPTER_CREATOR_BEGIN]\n")
  cat("Wyniki klasyfikacji XGBoost dla targetu: ", target, "\n", sep = "")
  print(wyniki_df)
  cat("[CHAPTER_CREATOR_END]\n")

  predykcje <- data.frame(datetime = test_data$datetime, rzeczywiste = y_test,
                          p_xgboost = p_test, p_model_zerowy = p_null_test,
                          prog_decyzyjny = prog, pred_xgboost = ifelse(p_test >= prog, 1, 0))
  zapisz_csv(predykcje, paste0("predykcje_klasyfikacja_xgb_", suffix, ".csv"))

  cm <- data.frame(actual = c(0, 0, 1, 1), predicted = c(0, 1, 0, 1),
                   n = c(met_test$TN, met_test$FP, met_test$FN, met_test$TP))
  zapisz_csv(cm, paste0("confusion_matrix_klasyfikacja_xgb_", suffix, ".csv"))

  eval_log <- as.data.frame(model$evaluation_log)
  zapisz_csv(eval_log, paste0("evaluation_log_klasyfikacja_xgb_", suffix, ".csv"))
  zapisz_learning_curve_xgb(eval_log, paste0("klasyfikacja_", suffix), "logloss")
  imp <- zapisz_importance_xgb(model, feature_cols, paste0("importance_klasyfikacja_xgb_", suffix, ".csv"), "XGBoost", target)
  top_features <- wybierz_top_features(imp, predyktory, 2L)
  zapisz_pdp_xgb(model, train_data, predyktory, feature_cols, top_features, paste0("klasyfikacja_", suffix), "klasyfikacja")
  zapisz_diagnostyke_xgb(model, paste0("klasyfikacja_", suffix))

  zapisz_wykres(paste0("prawdopodobienstwa_klasyfikacja_xgb_", suffix, ".png"), {
    plot(predykcje$datetime, predykcje$p_xgboost, type = "l", xlab = "Czas",
         ylab = "P(klasa pozytywna)", main = paste("XGBoost: prawdopodobienstwa -", target))
    abline(h = prog, lty = 2)
    legend("topright", legend = c("P pozytywna", paste0("prog = ", round(prog, 3))),
           lty = c(1, 2), bty = "n")
  })

  list(wyniki = wyniki_df, model = model)
}

# ------------------------------------------------------------
# 4. GBM3 - regresja, ze skalowaniem celu + pelne CV/PDP
# ------------------------------------------------------------

analizuj_regresje_gbm3 <- function(target) {
  if (!HAS_GBM3) return(NULL)
  suffix <- bezpieczna_nazwa(target)
  p <- length(predyktory)

  dane_model <- dane %>%
    select(data, datetime, all_of(target), all_of(predyktory)) %>%
    filter(complete.cases(.)) %>%
    arrange(datetime)

  train_data <- dane_model %>% filter(data < as.Date("2025-01-01"))
  test_data <- dane_model %>% filter(data >= as.Date("2025-01-01"))
  split <- podziel_train_valid_chrono(train_data, valid_frac = valid_frac)
  fit_data <- split$fit
  valid_data <- split$valid

  zrob_cv_gbm3_regresja(target, train_data, predyktory)

  mu <- mean(fit_data[[target]], na.rm = TRUE)
  sig <- sd(fit_data[[target]], na.rm = TRUE)
  if (is.na(sig) || sig == 0) sig <- 1
  target_scaled <- paste0(target, "_scaled")
  train_data[[target_scaled]] <- (train_data[[target]] - mu) / sig
  fit_data[[target_scaled]] <- (fit_data[[target]] - mu) / sig
  valid_data[[target_scaled]] <- (valid_data[[target]] - mu) / sig
  test_data[[target_scaled]] <- (test_data[[target]] - mu) / sig

  form <- buduj_formule(target_scaled, predyktory)
  df_gbm3 <- rbind(fit_data, valid_data)

  cat("\n============================================================\n")
  cat("REGRESJA GBM3: ", target, "\n", sep = "")
  cat("train: ", nrow(train_data), " | fit: ", nrow(fit_data), " | valid: ", nrow(valid_data),
      " | test: ", nrow(test_data), " | predyktory: ", p, "\n", sep = "")
  cat("GBM3: ", liczba_drzew_gbm3, " drzew, pelne CV osobno, PDP 1D+2D, diagnostyka\n", sep = "")
  cat("============================================================\n")
  flush.console()

  model_file <- paste0("gbm3_regresja_", suffix, ".rds")
  model <- wczytaj_model_cache(model_file)
  if (is.null(model)) {
    params <- make_gbm3_params(liczba_drzew_gbm3, nrow(fit_data), length(predyktory))
    model <- czasuj(paste("GBM3 regresja", target), gbm3::gbmt(
      form, data = df_gbm3, train_params = params, distribution = gbm3::gbm_dist("Gaussian")
    ))
    print(model)
    zapisz_model(model, model_file)
  }

  n_trees <- liczba_drzew_gbm3
  pred_train <- predict_gbm3_response(model, train_data, n_trees) * sig + mu
  pred_valid <- predict_gbm3_response(model, valid_data, n_trees) * sig + mu
  pred_test <- predict_gbm3_response(model, test_data, n_trees) * sig + mu
  pred_null_train <- rep(mean(fit_data[[target]]), nrow(train_data))
  pred_null_test <- rep(mean(fit_data[[target]]), nrow(test_data))

  met_null_train <- metryki_regresji(train_data[[target]], pred_null_train)
  met_null_test <- metryki_regresji(test_data[[target]], pred_null_test)
  met_train <- metryki_regresji(train_data[[target]], pred_train)
  met_valid <- metryki_regresji(valid_data[[target]], pred_valid)
  met_test <- metryki_regresji(test_data[[target]], pred_test)

  wyniki_df <- rbind(
    data.frame(target = target, model = "Model zerowy", typ_modelu = "bazowy", best_iteration = NA_integer_,
               MSE_train = met_null_train$MSE, RMSE_train = met_null_train$RMSE, MAE_train = met_null_train$MAE, R2_train = met_null_train$R2,
               MSE_valid = NA_real_, RMSE_valid = NA_real_, MAE_valid = NA_real_, R2_valid = NA_real_,
               MSE_test = met_null_test$MSE, RMSE_test = met_null_test$RMSE, MAE_test = met_null_test$MAE, R2_test = met_null_test$R2),
    data.frame(target = target, model = "GBM3", typ_modelu = "gbm3", best_iteration = n_trees,
               MSE_train = met_train$MSE, RMSE_train = met_train$RMSE, MAE_train = met_train$MAE, R2_train = met_train$R2,
               MSE_valid = met_valid$MSE, RMSE_valid = met_valid$RMSE, MAE_valid = met_valid$MAE, R2_valid = met_valid$R2,
               MSE_test = met_test$MSE, RMSE_test = met_test$RMSE, MAE_test = met_test$MAE, R2_test = met_test$R2)
  )

  zapisz_csv(wyniki_df, paste0("wyniki_regresja_gbm3_", suffix, ".csv"))
  cat("\n[CHAPTER_CREATOR_BEGIN]\n")
  cat("Wyniki regresji GBM3 dla targetu: ", target, "\n", sep = "")
  print(wyniki_df)
  cat("[CHAPTER_CREATOR_END]\n")

  predykcje <- data.frame(datetime = test_data$datetime, rzeczywiste = test_data[[target]],
                          gbm3 = pred_test, model_zerowy = pred_null_test,
                          residuum_gbm3 = test_data[[target]] - pred_test)
  zapisz_csv(predykcje, paste0("predykcje_regresja_gbm3_", suffix, ".csv"))

  imp <- zapisz_importance_gbm3(model, paste0("importance_regresja_gbm3_", suffix, ".csv"), "GBM3", target)
  top_features <- wybierz_top_features(imp, predyktory, 2L)
  zapisz_diagnostyke_gbm3(model, paste0("regresja_", suffix))
  zapisz_pdp_gbm3(model, train_data, predyktory, top_features, paste0("regresja_", suffix), "regresja",
                  n_trees, backtransform = list(mu = mu, sigma = sig))

  start_day <- start_day_plot
  if (!any(as.Date(predykcje$datetime) >= start_day & as.Date(predykcje$datetime) <= start_day + ile_dni_plot)) {
    start_day <- as.Date(min(predykcje$datetime))
  }
  end_day <- start_day + ile_dni_plot
  wykres_df <- predykcje[as.Date(predykcje$datetime) >= start_day & as.Date(predykcje$datetime) <= end_day, ]
  zapisz_wykres(paste0("predykcje_regresja_gbm3_", suffix, ".png"), {
    plot(wykres_df$datetime, wykres_df$rzeczywiste, pch = 16, xlab = "Czas", ylab = target,
         main = paste("GBM3: rzeczywiste i predykcja -", target))
    lines(wykres_df$datetime, wykres_df$gbm3, lwd = 2)
    lines(wykres_df$datetime, wykres_df$model_zerowy, lwd = 2, lty = 2)
    legend("topright", legend = c("rzeczywiste", "GBM3", "model zerowy"),
           pch = c(16, NA, NA), lty = c(NA, 1, 2), bty = "n")
  })

  list(wyniki = wyniki_df, model = model)
}

# ------------------------------------------------------------
# 5. GBM3 - klasyfikacja + pelne CV/PDP
# ------------------------------------------------------------

analizuj_klasyfikacje_gbm3 <- function(target) {
  if (!HAS_GBM3) return(NULL)
  suffix <- bezpieczna_nazwa(target)
  p <- length(predyktory)

  dane_model <- dane %>%
    select(data, datetime, all_of(target), all_of(predyktory)) %>%
    filter(complete.cases(.)) %>%
    arrange(datetime)
  dane_model[[target]] <- factor(ifelse(dane_model[[target]] == 1, "Tak", "Nie"), levels = c("Nie", "Tak"))

  train_data <- dane_model %>% filter(data < as.Date("2025-01-01"))
  test_data <- dane_model %>% filter(data >= as.Date("2025-01-01"))
  split <- podziel_train_valid_chrono(train_data, valid_frac = valid_frac)
  fit_data <- split$fit
  valid_data <- split$valid

  cv_train_data <- train_data
  cv_train_data[[target]] <- as.integer(cv_train_data[[target]] == "Tak")
  zrob_cv_gbm3_klasyfikacja(target, cv_train_data, predyktory)

  form <- buduj_formule(target, predyktory)
  df_gbm3 <- rbind(fit_data, valid_data)

  y_fit <- as.integer(fit_data[[target]] == "Tak")
  y_valid <- as.integer(valid_data[[target]] == "Tak")
  y_train <- as.integer(train_data[[target]] == "Tak")
  y_test <- as.integer(test_data[[target]] == "Tak")

  cat("\n============================================================\n")
  cat("KLASYFIKACJA GBM3: ", target, "\n", sep = "")
  cat("train: ", nrow(train_data), " | fit: ", nrow(fit_data), " | valid: ", nrow(valid_data),
      " | test: ", nrow(test_data), " | predyktory: ", p, "\n", sep = "")
  cat("GBM3: ", liczba_drzew_gbm3, " drzew, pelne CV osobno, PDP 1D+2D, diagnostyka\n", sep = "")
  cat("============================================================\n")
  flush.console()

  rozklad <- data.frame(target = target, zbior = c("fit", "valid", "train", "test"),
                        n = c(length(y_fit), length(y_valid), length(y_train), length(y_test)),
                        pozytywne = c(sum(y_fit == 1), sum(y_valid == 1), sum(y_train == 1), sum(y_test == 1)),
                        udzial_pozytywnych = c(mean(y_fit == 1), mean(y_valid == 1), mean(y_train == 1), mean(y_test == 1)))
  zapisz_csv(rozklad, paste0("rozklad_klas_gbm3_", suffix, ".csv"))
  print(rozklad)

  model_file <- paste0("gbm3_klasyfikacja_", suffix, ".rds")
  model <- wczytaj_model_cache(model_file)
  if (is.null(model)) {
    params <- make_gbm3_params(liczba_drzew_gbm3, nrow(fit_data), length(predyktory))
    model <- czasuj(paste("GBM3 klasyfikacja", target), gbm3::gbmt(
      form, data = df_gbm3, train_params = params, distribution = gbm3::gbm_dist("Bernoulli")
    ))
    print(model)
    zapisz_model(model, model_file)
  }

  n_trees <- liczba_drzew_gbm3
  p_train <- predict_gbm3_response(model, train_data, n_trees)
  p_valid <- predict_gbm3_response(model, valid_data, n_trees)
  p_test <- predict_gbm3_response(model, test_data, n_trees)
  p_null_train <- rep(mean(y_fit), length(y_train))
  p_null_test <- rep(mean(y_fit), length(y_test))

  prog <- wybierz_prog_youden(y_valid, p_valid)
  met_null_train <- metryki_klasyfikacji(y_train, p_null_train, prog = 0.5)
  met_null_test <- metryki_klasyfikacji(y_test, p_null_test, prog = 0.5)
  met_train <- metryki_klasyfikacji(y_train, p_train, prog = prog)
  met_valid <- metryki_klasyfikacji(y_valid, p_valid, prog = prog)
  met_test <- metryki_klasyfikacji(y_test, p_test, prog = prog)

  wyniki_df <- rbind(
    data.frame(target = target, model = "Model zerowy", typ_modelu = "bazowy", best_iteration = NA_integer_, prog_decyzyjny = 0.5,
               AUC_train = auc_roc(y_train, p_null_train), Brier_train = brier_score(y_train, p_null_train), LogLoss_train = log_loss(y_train, p_null_train), accuracy_train = met_null_train$accuracy,
               AUC_valid = NA_real_, Brier_valid = NA_real_, LogLoss_valid = NA_real_, accuracy_valid = NA_real_,
               AUC_test = auc_roc(y_test, p_null_test), Brier_test = brier_score(y_test, p_null_test), LogLoss_test = log_loss(y_test, p_null_test), accuracy_test = met_null_test$accuracy,
               sensitivity_test = met_null_test$sensitivity, specificity_test = met_null_test$specificity, precision_test = met_null_test$precision, F1_test = met_null_test$F1,
               TP_test = met_null_test$TP, TN_test = met_null_test$TN, FP_test = met_null_test$FP, FN_test = met_null_test$FN),
    data.frame(target = target, model = "GBM3", typ_modelu = "gbm3", best_iteration = n_trees, prog_decyzyjny = prog,
               AUC_train = auc_roc(y_train, p_train), Brier_train = brier_score(y_train, p_train), LogLoss_train = log_loss(y_train, p_train), accuracy_train = met_train$accuracy,
               AUC_valid = auc_roc(y_valid, p_valid), Brier_valid = brier_score(y_valid, p_valid), LogLoss_valid = log_loss(y_valid, p_valid), accuracy_valid = met_valid$accuracy,
               AUC_test = auc_roc(y_test, p_test), Brier_test = brier_score(y_test, p_test), LogLoss_test = log_loss(y_test, p_test), accuracy_test = met_test$accuracy,
               sensitivity_test = met_test$sensitivity, specificity_test = met_test$specificity, precision_test = met_test$precision, F1_test = met_test$F1,
               TP_test = met_test$TP, TN_test = met_test$TN, FP_test = met_test$FP, FN_test = met_test$FN)
  )

  zapisz_csv(wyniki_df, paste0("wyniki_klasyfikacja_gbm3_", suffix, ".csv"))
  cat("\n[CHAPTER_CREATOR_BEGIN]\n")
  cat("Wyniki klasyfikacji GBM3 dla targetu: ", target, "\n", sep = "")
  print(wyniki_df)
  cat("[CHAPTER_CREATOR_END]\n")

  predykcje <- data.frame(datetime = test_data$datetime, rzeczywiste = y_test,
                          p_gbm3 = p_test, p_model_zerowy = p_null_test,
                          prog_decyzyjny = prog, pred_gbm3 = ifelse(p_test >= prog, 1, 0))
  zapisz_csv(predykcje, paste0("predykcje_klasyfikacja_gbm3_", suffix, ".csv"))

  cm <- data.frame(actual = c(0, 0, 1, 1), predicted = c(0, 1, 0, 1),
                   n = c(met_test$TN, met_test$FP, met_test$FN, met_test$TP))
  zapisz_csv(cm, paste0("confusion_matrix_klasyfikacja_gbm3_", suffix, ".csv"))

  imp <- zapisz_importance_gbm3(model, paste0("importance_klasyfikacja_gbm3_", suffix, ".csv"), "GBM3", target)
  top_features <- wybierz_top_features(imp, predyktory, 2L)
  zapisz_diagnostyke_gbm3(model, paste0("klasyfikacja_", suffix))
  zapisz_pdp_gbm3(model, train_data, predyktory, top_features, paste0("klasyfikacja_", suffix), "klasyfikacja", n_trees)

  zapisz_wykres(paste0("prawdopodobienstwa_klasyfikacja_gbm3_", suffix, ".png"), {
    plot(predykcje$datetime, predykcje$p_gbm3, type = "l", xlab = "Czas",
         ylab = "P(klasa pozytywna)", main = paste("GBM3: prawdopodobienstwa -", target))
    abline(h = prog, lty = 2)
    legend("topright", legend = c("P pozytywna", paste0("prog = ", round(prog, 3))),
           lty = c(1, 2), bty = "n")
  })

  list(wyniki = wyniki_df, model = model)
}

# ------------------------------------------------------------
# 6. Uruchomienie analiz
# ------------------------------------------------------------

all_results <- list()

log_msg("START: modele regresyjne XGBoost")
wyniki_regresja_xgb_lista <- lapply(targety_regresja, analizuj_regresje_xgb)
names(wyniki_regresja_xgb_lista) <- targety_regresja
all_results$regresja_xgb <- do.call(rbind, lapply(wyniki_regresja_xgb_lista, function(x) x$wyniki))
log_msg("KONIEC: modele regresyjne XGBoost")

log_msg("START: modele klasyfikacyjne XGBoost")
wyniki_klasyfikacja_xgb_lista <- lapply(targety_klasyfikacja, analizuj_klasyfikacje_xgb)
names(wyniki_klasyfikacja_xgb_lista) <- targety_klasyfikacja
all_results$klasyfikacja_xgb <- do.call(rbind, lapply(wyniki_klasyfikacja_xgb_lista, function(x) x$wyniki))
log_msg("KONIEC: modele klasyfikacyjne XGBoost")

if (HAS_GBM3) {
  log_msg("START: modele regresyjne GBM3")
  wyniki_regresja_gbm3_lista <- lapply(targety_regresja, analizuj_regresje_gbm3)
  names(wyniki_regresja_gbm3_lista) <- targety_regresja
  all_results$regresja_gbm3 <- do.call(rbind, lapply(wyniki_regresja_gbm3_lista, function(x) x$wyniki))
  log_msg("KONIEC: modele regresyjne GBM3")

  log_msg("START: modele klasyfikacyjne GBM3")
  wyniki_klasyfikacja_gbm3_lista <- lapply(targety_klasyfikacja, analizuj_klasyfikacje_gbm3)
  names(wyniki_klasyfikacja_gbm3_lista) <- targety_klasyfikacja
  all_results$klasyfikacja_gbm3 <- do.call(rbind, lapply(wyniki_klasyfikacja_gbm3_lista, function(x) x$wyniki))
  log_msg("KONIEC: modele klasyfikacyjne GBM3")
}

wyniki_regresja_lacznie <- do.call(rbind, Filter(Negate(is.null), all_results[c("regresja_xgb", "regresja_gbm3")]))
wyniki_klasyfikacja_lacznie <- do.call(rbind, Filter(Negate(is.null), all_results[c("klasyfikacja_xgb", "klasyfikacja_gbm3")]))

if (!is.null(wyniki_regresja_lacznie)) {
  wyniki_regresja_lacznie <- wyniki_regresja_lacznie %>% arrange(target, MSE_test)
  zapisz_csv(wyniki_regresja_lacznie, "wyniki_regresja_boosting_lacznie.csv")
  cat("\n[CHAPTER_CREATOR_BEGIN]\n")
  cat("Zbiorcze wyniki regresji - boosting, pelny wymiar, male lasy:\n")
  print(wyniki_regresja_lacznie)
  cat("[CHAPTER_CREATOR_END]\n")
}

if (!is.null(wyniki_klasyfikacja_lacznie)) {
  wyniki_klasyfikacja_lacznie <- wyniki_klasyfikacja_lacznie %>% arrange(target, desc(AUC_test))
  zapisz_csv(wyniki_klasyfikacja_lacznie, "wyniki_klasyfikacja_boosting_lacznie.csv")
  cat("\n[CHAPTER_CREATOR_BEGIN]\n")
  cat("Zbiorcze wyniki klasyfikacji - boosting, pelny wymiar, male lasy:\n")
  print(wyniki_klasyfikacja_lacznie)
  cat("[CHAPTER_CREATOR_END]\n")
}

index_files <- data.frame(file = c(list.files(results_dir, recursive = TRUE, full.names = FALSE),
                                   file.path("..", list.files(plot_dir, recursive = TRUE, full.names = FALSE))))
zapisz_csv(index_files, "indeks_wynikow.csv")

czas_koniec_global <- Sys.time()
log_msg("KONIEC SKRYPTU | calkowity czas: ", round(as.numeric(difftime(czas_koniec_global, czas_start_global, units = "mins")), 2), " min")

cat("\n============================================================\n")
cat("KONIEC SKRYPTU\n")
cat("Wyniki: ", normalizePath(results_dir, mustWork = FALSE), "\n", sep = "")
cat("Wykresy: ", normalizePath(plot_dir, mustWork = FALSE), "\n", sep = "")
cat("Log: ", normalizePath(log_file, mustWork = FALSE), "\n", sep = "")
cat("============================================================\n")
