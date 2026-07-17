# ============================================================
# 13_interpretowalnosc_chalupy.R
# Metody wspierajace interpretacje modeli - adaptacja notatnika 13-interpretowalnosc.Rmd
# na dane pogodowe Chalupy.
#
# Skrypt tworzy wyniki pod kolejny rozdzial projektu:
# - dopasowanie modeli typu "black-box" / las losowy,
# - LIME / LocalModel z pakietu iml,
# - klasyczne wartosci Shapleya z pakietu iml,
# - SHAP z pakietow fastshap i shapviz,
# - wykresy waterfall, force, importance, dependence i beeswarm,
# - interpretacja lokalna wybranych obserwacji testowych,
# - interpretacja globalna modeli regresyjnych i klasyfikacyjnych.
#
# Domyslna sciezka danych:
#   data/chalupy_hourly_10000_days.csv
#
# Wyniki:
#   wyniki_interpretowalnosc_13_chalupy/
#   plots_interpretowalnosc_13_chalupy/
# ============================================================

options(stringsAsFactors = FALSE)
set.seed(2025)

# ----------------------------
# 0. Konfiguracja
# ----------------------------

DATA_PATH <- "data/chalupy_hourly_10000_days.csv"

RUN_NAME <- "interpretowalnosc_13_chalupy"
OUT_DIR  <- paste0("wyniki_", RUN_NAME)
PLOT_DIR <- paste0("plots_", RUN_NAME)
LOG_DIR  <- file.path(OUT_DIR, "logi")
MODEL_DIR <- file.path(OUT_DIR, "modele")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)

LOG_FILE <- file.path(LOG_DIR, paste0(RUN_NAME, "_log.txt"))

# Progi uzywane tylko wtedy, gdy w danych nie ma gotowych kolumn *_event.
# Jesli poprzednie skrypty tworzyly targety klasyfikacyjne inaczej, najlepiej
# uruchamiac ten skrypt na danych z gotowymi kolumnami:
#   wind_speed_10m_next_event
#   wind_gusts_10m_next_event
PROG_WIND_SPEED_EVENT <- 30   # km/h
PROG_WIND_GUSTS_EVENT <- 45   # km/h

# Liczba wierszy testowych zgodna z poprzednimi etapami projektu.
TEST_ROWS <- 10774

# Las losowy - model celowo mniej interpretowalny.
# Jesli obliczenia sa zbyt wolne, zmniejsz RF_NTREE albo ustaw RF_MAX_TRAIN_ROWS.
RF_NTREE <- 300
RF_MAX_TRAIN_ROWS <- Inf

# Obserwacje/rozmiary prob do interpretacji.
N_LOCAL_OBS <- 4
IML_BACKGROUND_ROWS <- 5000
GLOBAL_SHAP_ROWS <- 2500

# Dokladnosc estymacji Shapleya/SHAP.
IML_SHAPLEY_SAMPLE_SIZE <- 100
FASTSHAP_NSIM_LOCAL <- 50
FASTSHAP_NSIM_GLOBAL <- 25

# Rownoleglenie fastshap. Domyslnie FALSE, zeby uniknac problemow na Windows/RStudio.
PARALLEL_FASTSHAP <- TRUE
FASTSHAP_CORES <- max(1, parallel::detectCores() - 4)

# Ktore targety interpretowac.
REG_TARGETS_WANTED <- c("wind_speed_10m_next", "wind_gusts_10m_next")
CLASS_TARGETS_WANTED <- c("wind_speed_10m_next_event", "wind_gusts_10m_next_event")

# Wykresy zaleznosci SHAP zostana wykonane dla maksymalnie tylu najwazniejszych cech.
N_DEPENDENCE_FEATURES <- 3

# ----------------------------
# 1. Pakiety i funkcje pomocnicze
# ----------------------------

required_packages <- c(
  "data.table",
  "randomForest",
  "iml",
  "fastshap",
  "shapviz",
  "ggplot2"
)

missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Brakuje pakietow: ",
    paste(missing_packages, collapse = ", "),
    "\nZainstaluj je np. install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    "))"
  )
}

library(data.table)
library(randomForest)
library(iml)
library(fastshap)
library(shapviz)
library(ggplot2)

if (PARALLEL_FASTSHAP) {
  if (!requireNamespace("doParallel", quietly = TRUE) || !requireNamespace("foreach", quietly = TRUE)) {
    warning("PARALLEL_FASTSHAP = TRUE, ale brakuje doParallel/foreach. Przelaczam na obliczenia sekwencyjne.")
    PARALLEL_FASTSHAP <- FALSE
  } else {
    doParallel::registerDoParallel(cores = FASTSHAP_CORES)
  }
}

log_line <- function(...) {
  txt <- paste0(...)
  cat(txt, "\n")
  cat(txt, "\n", file = LOG_FILE, append = TRUE)
}

log_section <- function(title) {
  sep <- paste(rep("=", 70), collapse = "")
  log_line("")
  log_line(sep)
  log_line(title)
  log_line(sep)
}

saved_file <- function(path) {
  log_line("[SAVED_FILE] ", path)
}

chapter_block <- function(title, expr) {
  log_line("")
  log_line("[CHAPTER_CREATOR_BEGIN]")
  log_line(title)
  out <- capture.output(expr)
  if (length(out) > 0) {
    for (x in out) log_line(x)
  }
  log_line("[CHAPTER_CREATOR_END]")
  log_line("")
}

save_csv <- function(x, path) {
  data.table::fwrite(as.data.table(x), path)
  saved_file(path)
}

save_rds <- function(x, path) {
  saveRDS(x, path)
  saved_file(path)
}

safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x
}

save_gg <- function(plot_obj, path, width = 9, height = 6, dpi = 150) {
  ok <- tryCatch({
    ggplot2::ggsave(path, plot_obj, width = width, height = height, dpi = dpi)
    TRUE
  }, error = function(e) {
    log_line("[BLAD WYKRESU] ", path, " | ", conditionMessage(e))
    png(path, width = 1600, height = 1000, res = 150)
    plot.new()
    title("Blad wykresu")
    text(0.5, 0.5, conditionMessage(e), cex = 0.8)
    dev.off()
    FALSE
  })
  saved_file(path)
  invisible(ok)
}

save_base_plot <- function(path, expr, width = 1600, height = 1200, res = 150) {
  png(path, width = width, height = height, res = res)
  ok <- tryCatch({
    force(expr)
    TRUE
  }, error = function(e) {
    plot.new()
    title("Blad wykresu")
    text(0.5, 0.5, conditionMessage(e), cex = 0.8)
    log_line("[BLAD WYKRESU] ", path, " | ", conditionMessage(e))
    FALSE
  })
  dev.off()
  saved_file(path)
  invisible(ok)
}

sample_rows <- function(df, n, seed = 2025) {
  df <- as.data.frame(df)
  if (is.infinite(n) || nrow(df) <= n) return(df)
  set.seed(seed)
  df[sample(seq_len(nrow(df)), n), , drop = FALSE]
}

rmse <- function(y, pred) sqrt(mean((y - pred)^2, na.rm = TRUE))
mae  <- function(y, pred) mean(abs(y - pred), na.rm = TRUE)
mse  <- function(y, pred) mean((y - pred)^2, na.rm = TRUE)

r2_score <- function(y, pred) {
  1 - sum((y - pred)^2, na.rm = TRUE) / sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
}

reg_metrics <- function(y_train, p_train, y_test, p_test, target, model) {
  data.frame(
    target = target,
    model = model,
    MSE_train = mse(y_train, p_train),
    RMSE_train = rmse(y_train, p_train),
    MAE_train = mae(y_train, p_train),
    R2_train = r2_score(y_train, p_train),
    MSE_test = mse(y_test, p_test),
    RMSE_test = rmse(y_test, p_test),
    MAE_test = mae(y_test, p_test),
    R2_test = r2_score(y_test, p_test)
  )
}

classification_metrics <- function(actual, predicted, target, model, positive = "Yes") {
  actual <- factor(actual, levels = c("No", "Yes"))
  predicted <- factor(predicted, levels = c("No", "Yes"))

  tab <- table(predicted = predicted, actual = actual)

  TP <- tab[positive, positive]
  TN <- tab["No", "No"]
  FP <- tab[positive, "No"]
  FN <- tab["No", positive]

  accuracy <- (TP + TN) / sum(tab)
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
  specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
  precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
  f1 <- ifelse(!is.na(precision) && !is.na(sensitivity) && (precision + sensitivity) > 0,
               2 * precision * sensitivity / (precision + sensitivity), NA)
  error_rate <- mean(predicted != actual)

  data.frame(
    target = target,
    model = model,
    accuracy = accuracy,
    error_rate = error_rate,
    sensitivity = sensitivity,
    specificity = specificity,
    precision = precision,
    F1 = f1,
    TP = as.integer(TP),
    TN = as.integer(TN),
    FP = as.integer(FP),
    FN = as.integer(FN)
  )
}

plot_confusion_heatmap <- function(tab, title, path) {
  d <- as.data.frame(tab)
  names(d) <- c("predicted", "actual", "n")

  p <- ggplot(d, aes(x = actual, y = predicted, fill = n)) +
    geom_tile() +
    geom_text(aes(label = n), size = 5) +
    labs(title = title, x = "Klasa rzeczywista", y = "Klasa przewidywana") +
    theme_minimal()

  save_gg(p, path, width = 7, height = 5)
}

select_regression_obs <- function(test_data, target, n_obs = 4) {
  y <- test_data[[target]]
  probs <- seq(0.10, 0.90, length.out = n_obs)
  q <- as.numeric(quantile(y, probs = probs, na.rm = TRUE, type = 7))
  idx <- vapply(q, function(v) which.min(abs(y - v)), integer(1))
  idx <- unique(idx)

  if (length(idx) < n_obs) {
    candidates <- order(y, na.last = NA)
    extra <- candidates[!candidates %in% idx]
    idx <- c(idx, head(extra, n_obs - length(idx)))
  }

  idx[seq_len(min(length(idx), n_obs))]
}

select_classification_obs <- function(test_data, target, n_obs = 4) {
  y <- factor(test_data[[target]], levels = c("No", "Yes"))
  n_yes <- ceiling(n_obs / 2)
  n_no <- n_obs - n_yes

  yes_idx <- which(y == "Yes")
  no_idx <- which(y == "No")

  selected <- c(
    head(no_idx, n_no),
    head(yes_idx, n_yes)
  )

  if (length(selected) < n_obs) {
    extra <- setdiff(seq_along(y), selected)
    selected <- c(selected, head(extra, n_obs - length(selected)))
  }

  selected[seq_len(min(length(selected), n_obs))]
}

make_importance_df <- function(rf, target, task) {
  imp <- randomForest::importance(rf)
  imp_df <- as.data.frame(imp)
  imp_df$feature <- rownames(imp_df)
  rownames(imp_df) <- NULL

  # Ujednolicenie glownej kolumny waznosci.
  if ("%IncMSE" %in% names(imp_df)) {
    imp_df$importance_main <- imp_df[["%IncMSE"]]
  } else if ("MeanDecreaseGini" %in% names(imp_df)) {
    imp_df$importance_main <- imp_df[["MeanDecreaseGini"]]
  } else if ("IncNodePurity" %in% names(imp_df)) {
    imp_df$importance_main <- imp_df[["IncNodePurity"]]
  } else {
    numeric_cols <- names(imp_df)[vapply(imp_df, is.numeric, logical(1))]
    imp_df$importance_main <- if (length(numeric_cols) > 0) imp_df[[numeric_cols[1]]] else NA_real_
  }

  imp_df$target <- target
  imp_df$task <- task
  imp_df[order(-imp_df$importance_main), c("target", "task", "feature", setdiff(names(imp_df), c("target", "task", "feature")))]
}

make_shap_summary <- function(shap_matrix, target, task, model_name) {
  shap_df <- as.data.frame(shap_matrix)
  data.frame(
    target = target,
    task = task,
    model = model_name,
    feature = names(shap_df),
    mean_abs_shap = vapply(shap_df, function(x) mean(abs(x), na.rm = TRUE), numeric(1)),
    mean_shap = vapply(shap_df, function(x) mean(x, na.rm = TRUE), numeric(1)),
    sd_shap = vapply(shap_df, function(x) sd(x, na.rm = TRUE), numeric(1)),
    row.names = NULL
  )[order(-vapply(shap_df, function(x) mean(abs(x), na.rm = TRUE), numeric(1))), ]
}

# ----------------------------
# 2. Wczytanie i przygotowanie danych
# ----------------------------

log_section("INTERPRETOWALNOSC 13 - DANE CHALUPY")
log_line("Start: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
log_line("Dane: ", DATA_PATH)
log_line("Wyniki: ", OUT_DIR)
log_line("Wykresy: ", PLOT_DIR)
log_line("Log: ", LOG_FILE)

if (!file.exists(DATA_PATH)) {
  stop("Nie znaleziono pliku danych: ", DATA_PATH)
}

dt <- data.table::fread(DATA_PATH)
log_line("Wczytano danych: ", nrow(dt), " wierszy, ", ncol(dt), " kolumn.")

# Uporzadkowanie po czasie, jesli istnieje kolumna czasowa.
time_candidates <- intersect(names(dt), c("time", "datetime", "date", "data", "timestamp"))
if (length(time_candidates) > 0) {
  time_col <- time_candidates[1]
  log_line("Wykryto kolumne czasu: ", time_col)
  parsed_time <- suppressWarnings(as.POSIXct(dt[[time_col]], tz = "UTC"))
  if (all(is.na(parsed_time))) {
    parsed_time <- suppressWarnings(as.POSIXct(dt[[time_col]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  }
  if (!all(is.na(parsed_time))) {
    dt[, .czas_tmp := parsed_time]
    data.table::setorder(dt, .czas_tmp)

    lt <- as.POSIXlt(dt$.czas_tmp, tz = "UTC")
    dzien_roku <- lt$yday + 1
    godzina <- lt$hour
    miesiac <- lt$mon + 1

    if (!"sin_dzien" %in% names(dt)) dt[, sin_dzien := sin(2 * pi * dzien_roku / 365)]
    if (!"cos_dzien" %in% names(dt)) dt[, cos_dzien := cos(2 * pi * dzien_roku / 365)]
    if (!"sin_godzina" %in% names(dt)) dt[, sin_godzina := sin(2 * pi * godzina / 24)]
    if (!"cos_godzina" %in% names(dt)) dt[, cos_godzina := cos(2 * pi * godzina / 24)]
    if (!"polrocze_cieple" %in% names(dt)) dt[, polrocze_cieple := as.integer(miesiac %in% 4:9)]
    dt[, .czas_tmp := NULL]
  }
}

if (!"is_day" %in% names(dt) && "shortwave_radiation" %in% names(dt)) {
  dt[, is_day := as.integer(shortwave_radiation > 0)]
}

# Targety przesuniete o godzine.
if (!"wind_speed_10m_next" %in% names(dt) && "wind_speed_10m" %in% names(dt)) {
  dt[, wind_speed_10m_next := data.table::shift(wind_speed_10m, type = "lead")]
}
if (!"wind_gusts_10m_next" %in% names(dt) && "wind_gusts_10m" %in% names(dt)) {
  dt[, wind_gusts_10m_next := data.table::shift(wind_gusts_10m, type = "lead")]
}

# Targety klasyfikacyjne.
if (!"wind_speed_10m_next_event" %in% names(dt) && "wind_speed_10m_next" %in% names(dt)) {
  dt[, wind_speed_10m_next_event := ifelse(wind_speed_10m_next >= PROG_WIND_SPEED_EVENT, "Yes", "No")]
  log_line("Utworzono wind_speed_10m_next_event z progiem: ", PROG_WIND_SPEED_EVENT)
}
if (!"wind_gusts_10m_next_event" %in% names(dt) && "wind_gusts_10m_next" %in% names(dt)) {
  dt[, wind_gusts_10m_next_event := ifelse(wind_gusts_10m_next >= PROG_WIND_GUSTS_EVENT, "Yes", "No")]
  log_line("Utworzono wind_gusts_10m_next_event z progiem: ", PROG_WIND_GUSTS_EVENT)
}

predictors_base <- c(
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

predictors <- intersect(predictors_base, names(dt))
reg_targets <- intersect(REG_TARGETS_WANTED, names(dt))
class_targets <- intersect(CLASS_TARGETS_WANTED, names(dt))

if (length(predictors) == 0) stop("Brak predyktorow.")
if (length(reg_targets) == 0 && length(class_targets) == 0) {
  stop("Brak targetow regresyjnych i klasyfikacyjnych.")
}

# Typy danych.
for (v in predictors) {
  if (is.integer(dt[[v]])) dt[[v]] <- as.numeric(dt[[v]])
}

for (target in class_targets) {
  if (is.numeric(dt[[target]]) || is.integer(dt[[target]])) {
    dt[[target]] <- ifelse(dt[[target]] == 1, "Yes", "No")
  }
  dt[[target]] <- factor(dt[[target]], levels = c("No", "Yes"))
}

all_needed <- unique(c(predictors, reg_targets, class_targets))
df <- as.data.frame(dt[, ..all_needed])
df <- na.omit(df)

# Usuniecie kolumn stalych.
is_constant <- vapply(df[, predictors, drop = FALSE], function(x) length(unique(x)) < 2, logical(1))
if (any(is_constant)) {
  log_line("Usunieto stale predyktory: ", paste(names(is_constant)[is_constant], collapse = ", "))
  predictors <- setdiff(predictors, names(is_constant)[is_constant])
}

# Zostawiamy tylko numeryczne predyktory, bo randomForest, iml i SHAP beda na nich dzialaly najstabilniej.
numeric_predictors <- predictors[vapply(df[, predictors, drop = FALSE], is.numeric, logical(1))]
predictors <- numeric_predictors

df <- df[, unique(c(predictors, reg_targets, class_targets)), drop = FALSE]
df <- na.omit(df)

n <- nrow(df)
n_test <- min(TEST_ROWS, max(1, floor(0.2 * n)))
if (n <= n_test + 100) n_test <- floor(0.2 * n)

idx_train <- seq_len(n - n_test)
idx_test <- (n - n_test + 1):n

train_df <- df[idx_train, , drop = FALSE]
test_df  <- df[idx_test, , drop = FALSE]

X_train <- train_df[, predictors, drop = FALSE]
X_test  <- test_df[, predictors, drop = FALSE]

X_background <- sample_rows(X_train, IML_BACKGROUND_ROWS, seed = 2025)
X_global <- sample_rows(df[, predictors, drop = FALSE], GLOBAL_SHAP_ROWS, seed = 2026)

chapter_block("Opis konfiguracji analizy interpretowalnosci", {
  print(data.frame(
    element = c(
      "liczba_obserwacji_po_przygotowaniu",
      "liczba_obserwacji_train",
      "liczba_obserwacji_test",
      "liczba_predyktorow",
      "targety_regresyjne",
      "targety_klasyfikacyjne",
      "model_black_box",
      "liczba_drzew_lasu_losowego",
      "obserwacje_lokalne_na_target",
      "wiersze_tla_iml_fastshap",
      "wiersze_global_shap"
    ),
    wartosc = c(
      nrow(df),
      nrow(train_df),
      nrow(test_df),
      length(predictors),
      paste(reg_targets, collapse = ", "),
      paste(class_targets, collapse = ", "),
      "randomForest",
      RF_NTREE,
      N_LOCAL_OBS,
      nrow(X_background),
      nrow(X_global)
    )
  ))
  cat("\nPredyktory:\n")
  print(data.frame(predyktor = predictors))
})

save_csv(data.frame(predyktor = predictors), file.path(OUT_DIR, "lista_predyktorow.csv"))
save_csv(data.frame(target_regresyjny = reg_targets), file.path(OUT_DIR, "targety_regresyjne.csv"))
save_csv(data.frame(target_klasyfikacyjny = class_targets), file.path(OUT_DIR, "targety_klasyfikacyjne.csv"))

# ----------------------------
# 3. Funkcje interpretacji modeli
# ----------------------------

train_rf_regression <- function(target) {
  y <- train_df[[target]]
  train_model <- data.frame(y = y, X_train)

  if (!is.infinite(RF_MAX_TRAIN_ROWS) && nrow(train_model) > RF_MAX_TRAIN_ROWS) {
    set.seed(2025)
    train_model <- train_model[sample(seq_len(nrow(train_model)), RF_MAX_TRAIN_ROWS), , drop = FALSE]
  }

  rf <- randomForest::randomForest(
    y ~ .,
    data = train_model,
    ntree = RF_NTREE,
    importance = TRUE
  )

  rf
}

train_rf_classification <- function(target) {
  y <- factor(train_df[[target]], levels = c("No", "Yes"))
  train_model <- data.frame(y = y, X_train)

  if (!is.infinite(RF_MAX_TRAIN_ROWS) && nrow(train_model) > RF_MAX_TRAIN_ROWS) {
    set.seed(2025)
    train_model <- train_model[sample(seq_len(nrow(train_model)), RF_MAX_TRAIN_ROWS), , drop = FALSE]
  }

  rf <- randomForest::randomForest(
    y ~ .,
    data = train_model,
    ntree = RF_NTREE,
    importance = TRUE
  )

  rf
}

run_local_iml <- function(rf, target, task, X_selected, y_selected, pred_fun, predictor_obj) {
  target_dir <- file.path(OUT_DIR, paste0("local_", task, "_", target))
  target_plot_dir <- file.path(PLOT_DIR, paste0("local_", task, "_", target))
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(target_plot_dir, recursive = TRUE, showWarnings = FALSE)

  all_lime <- list()
  all_shapley <- list()

  for (i in seq_len(nrow(X_selected))) {
    obs <- X_selected[i, , drop = FALSE]
    obs_name <- paste0("obs_", i)

    log_line("LIME/LocalModel: ", target, " | ", obs_name)

    lime <- tryCatch(
      iml::LocalModel$new(predictor_obj, x.interest = obs),
      error = function(e) {
        log_line("[BLAD LIME] ", target, " | ", obs_name, " | ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(lime)) {
      lime_res <- as.data.frame(lime$results)
      lime_res$target <- target
      lime_res$task <- task
      lime_res$obs_id <- i
      lime_res$actual <- y_selected[i]
      lime_res$prediction <- as.numeric(pred_fun(rf, obs))
      all_lime[[i]] <- lime_res

      save_csv(lime_res, file.path(target_dir, paste0("lime_localmodel_", obs_name, ".csv")))

      p_lime <- tryCatch(plot(lime), error = function(e) NULL)
      if (!is.null(p_lime)) {
        save_gg(p_lime, file.path(target_plot_dir, paste0("lime_localmodel_", obs_name, ".png")), width = 8, height = 5)
      }
    }

    log_line("Shapley iml: ", target, " | ", obs_name)

    shap <- tryCatch(
      iml::Shapley$new(predictor_obj, x.interest = obs, sample.size = IML_SHAPLEY_SAMPLE_SIZE),
      error = function(e) {
        log_line("[BLAD SHAPLEY IML] ", target, " | ", obs_name, " | ", conditionMessage(e))
        NULL
      }
    )

    if (!is.null(shap)) {
      shap_res <- as.data.frame(shap$results)
      shap_res$target <- target
      shap_res$task <- task
      shap_res$obs_id <- i
      shap_res$actual <- y_selected[i]
      shap_res$prediction <- as.numeric(pred_fun(rf, obs))
      all_shapley[[i]] <- shap_res

      save_csv(shap_res, file.path(target_dir, paste0("shapley_iml_", obs_name, ".csv")))

      p_shap <- tryCatch(plot(shap), error = function(e) NULL)
      if (!is.null(p_shap)) {
        save_gg(p_shap, file.path(target_plot_dir, paste0("shapley_iml_", obs_name, ".png")), width = 8, height = 5)
      }
    }
  }

  if (length(all_lime) > 0) {
    lime_all <- do.call(rbind, all_lime)
    save_csv(lime_all, file.path(target_dir, "lime_localmodel_wszystkie_obserwacje.csv"))
  } else {
    lime_all <- data.frame()
  }

  if (length(all_shapley) > 0) {
    shapley_all <- do.call(rbind, all_shapley)
    save_csv(shapley_all, file.path(target_dir, "shapley_iml_wszystkie_obserwacje.csv"))
  } else {
    shapley_all <- data.frame()
  }

  list(lime = lime_all, shapley = shapley_all)
}

run_fastshap_and_shapviz <- function(rf, target, task, X_selected, y_selected, pred_fun) {
  target_dir <- file.path(OUT_DIR, paste0("shap_", task, "_", target))
  target_plot_dir <- file.path(PLOT_DIR, paste0("shap_", task, "_", target))
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(target_plot_dir, recursive = TRUE, showWarnings = FALSE)

  # SHAP lokalny dla wybranych obserwacji.
  log_line("fastshap lokalny: ", target)

  fshap_local <- tryCatch(
    fastshap::explain(
      object = rf,
      X = X_background,
      newdata = X_selected,
      nsim = FASTSHAP_NSIM_LOCAL,
      pred_wrapper = pred_fun,
      parallel = PARALLEL_FASTSHAP,
      adjust = TRUE
    ),
    error = function(e) {
      log_line("[BLAD FASTSHAP LOKALNY] ", target, " | ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(fshap_local)) {
    fshap_local_df <- as.data.frame(fshap_local)
    fshap_local_df$obs_id <- seq_len(nrow(fshap_local_df))
    fshap_local_df$actual <- y_selected
    fshap_local_df$prediction <- as.numeric(pred_fun(rf, X_selected))
    fshap_local_df$target <- target
    fshap_local_df$task <- task
    save_csv(fshap_local_df, file.path(target_dir, "fastshap_lokalny_wybrane_obserwacje.csv"))

    # Porownanie: suma SHAP + baseline kontra predykcja.
    baseline_pred <- mean(as.numeric(pred_fun(rf, X_background)), na.rm = TRUE)
    local_check <- data.frame(
      target = target,
      task = task,
      obs_id = seq_len(nrow(X_selected)),
      actual = y_selected,
      prediction = as.numeric(pred_fun(rf, X_selected)),
      baseline = baseline_pred,
      sum_shap = rowSums(as.data.frame(fshap_local[, predictors, drop = FALSE]), na.rm = TRUE),
      baseline_plus_sum_shap = baseline_pred + rowSums(as.data.frame(fshap_local[, predictors, drop = FALSE]), na.rm = TRUE)
    )
    save_csv(local_check, file.path(target_dir, "fastshap_lokalny_sprawdzenie_sum.csv"))

    sv_local <- tryCatch(shapviz::shapviz(fshap_local[, predictors, drop = FALSE], X = X_selected), error = function(e) {
      log_line("[BLAD SHAPVIZ LOKALNY] ", target, " | ", conditionMessage(e))
      NULL
    })

    if (!is.null(sv_local)) {
      for (i in seq_len(nrow(X_selected))) {
        p_waterfall <- tryCatch(shapviz::sv_waterfall(sv_local, row_id = i), error = function(e) NULL)
        if (!is.null(p_waterfall)) {
          save_gg(p_waterfall, file.path(target_plot_dir, paste0("shap_waterfall_obs_", i, ".png")), width = 9, height = 6)
        }

        p_force <- tryCatch(shapviz::sv_force(sv_local, row_id = i), error = function(e) NULL)
        if (!is.null(p_force)) {
          save_gg(p_force, file.path(target_plot_dir, paste0("shap_force_obs_", i, ".png")), width = 10, height = 4)
        }
      }
    }
  }

  # SHAP globalny na probie danych.
  log_line("fastshap globalny: ", target)

  # Model globalny analogicznie jak w notatniku: dopasowany na calym przygotowanym zbiorze.
  if (task == "regresja") {
    all_model_data <- data.frame(y = df[[target]], df[, predictors, drop = FALSE])
    rf_all <- randomForest::randomForest(y ~ ., data = all_model_data, ntree = RF_NTREE, importance = TRUE)
    pred_fun_all <- function(model, newdata) as.numeric(predict(model, newdata = newdata))
  } else {
    all_model_data <- data.frame(y = factor(df[[target]], levels = c("No", "Yes")), df[, predictors, drop = FALSE])
    rf_all <- randomForest::randomForest(y ~ ., data = all_model_data, ntree = RF_NTREE, importance = TRUE)
    pred_fun_all <- function(model, newdata) as.numeric(predict(model, newdata = newdata, type = "prob")[, "Yes"])
  }
  save_rds(rf_all, file.path(MODEL_DIR, paste0("rf_all_", task, "_", target, ".rds")))

  fshap_global <- tryCatch(
    fastshap::explain(
      object = rf_all,
      X = X_background,
      newdata = X_global,
      nsim = FASTSHAP_NSIM_GLOBAL,
      pred_wrapper = pred_fun_all,
      parallel = PARALLEL_FASTSHAP,
      adjust = TRUE
    ),
    error = function(e) {
      log_line("[BLAD FASTSHAP GLOBALNY] ", target, " | ", conditionMessage(e))
      NULL
    }
  )

  if (!is.null(fshap_global)) {
    fshap_global_df <- as.data.frame(fshap_global)
    fshap_global_df$target <- target
    fshap_global_df$task <- task
    save_csv(fshap_global_df, file.path(target_dir, "fastshap_globalny.csv"))

    shap_summary <- make_shap_summary(fshap_global[, predictors, drop = FALSE], target, task, "randomForest")
    save_csv(shap_summary, file.path(target_dir, "shap_global_importance_summary.csv"))

    sv_global <- tryCatch(shapviz::shapviz(fshap_global[, predictors, drop = FALSE], X = X_global), error = function(e) {
      log_line("[BLAD SHAPVIZ GLOBALNY] ", target, " | ", conditionMessage(e))
      NULL
    })

    if (!is.null(sv_global)) {
      p_imp <- tryCatch(shapviz::sv_importance(sv_global), error = function(e) NULL)
      if (!is.null(p_imp)) {
        save_gg(p_imp, file.path(target_plot_dir, "shap_importance_bar.png"), width = 9, height = 6)
      }

      p_bee <- tryCatch(shapviz::sv_importance(sv_global, kind = "beeswarm", show_numbers = TRUE), error = function(e) NULL)
      if (!is.null(p_bee)) {
        save_gg(p_bee, file.path(target_plot_dir, "shap_importance_beeswarm.png"), width = 10, height = 7)
      }

      top_features <- head(shap_summary$feature, N_DEPENDENCE_FEATURES)
      for (v in top_features) {
        p_dep <- tryCatch(shapviz::sv_dependence(sv_global, v = v), error = function(e) NULL)
        if (!is.null(p_dep)) {
          save_gg(p_dep, file.path(target_plot_dir, paste0("shap_dependence_", safe_name(v), ".png")), width = 8, height = 6)
        }
      }
    }
  } else {
    shap_summary <- data.frame()
  }

  list(
    local = if (exists("fshap_local") && !is.null(fshap_local)) fshap_local else NULL,
    global = if (exists("fshap_global") && !is.null(fshap_global)) fshap_global else NULL,
    global_summary = if (exists("shap_summary")) shap_summary else data.frame()
  )
}

# ----------------------------
# 4. Interpretacja modeli regresyjnych
# ----------------------------

log_section("MODELE REGRESYJNE - RANDOM FOREST + LIME/SHAP")

all_reg_metrics <- list()
all_rf_importance <- list()
all_shap_summaries <- list()

for (target in reg_targets) {
  log_section(paste("Regresja:", target))

  rf <- train_rf_regression(target)
  save_rds(rf, file.path(MODEL_DIR, paste0("rf_regresja_", target, ".rds")))

  pred_train <- as.numeric(predict(rf, newdata = X_train))
  pred_test <- as.numeric(predict(rf, newdata = X_test))

  metrics <- reg_metrics(train_df[[target]], pred_train, test_df[[target]], pred_test, target, "randomForest")
  save_csv(metrics, file.path(OUT_DIR, paste0("metryki_rf_regresja_", target, ".csv")))
  all_reg_metrics[[target]] <- metrics

  pred_df <- data.frame(
    target = target,
    zbior = "test",
    indeks_test = seq_len(nrow(test_df)),
    rzeczywiste = test_df[[target]],
    predykcja = pred_test,
    blad = test_df[[target]] - pred_test
  )
  save_csv(pred_df, file.path(OUT_DIR, paste0("predykcje_rf_regresja_", target, ".csv")))

  pred_plot_df <- pred_df[seq_len(min(500, nrow(pred_df))), ]
  p_pred <- ggplot(pred_plot_df, aes(x = indeks_test)) +
    geom_line(aes(y = rzeczywiste, linetype = "rzeczywiste")) +
    geom_line(aes(y = predykcja, linetype = "predykcja")) +
    labs(title = paste("Random Forest - predykcje testowe -", target),
         x = "Indeks obserwacji testowej", y = target, linetype = "") +
    theme_minimal()
  save_gg(p_pred, file.path(PLOT_DIR, paste0("predykcje_rf_regresja_", target, ".png")), width = 9, height = 5)

  imp <- make_importance_df(rf, target, "regresja")
  save_csv(imp, file.path(OUT_DIR, paste0("importance_rf_regresja_", target, ".csv")))
  all_rf_importance[[paste0("regresja_", target)]] <- imp

  save_base_plot(file.path(PLOT_DIR, paste0("importance_rf_regresja_", target, ".png")), {
    randomForest::varImpPlot(rf, main = paste("Waznosc predyktorow RF -", target))
  })

  selected_idx <- select_regression_obs(test_df, target, N_LOCAL_OBS)
  X_selected <- X_test[selected_idx, , drop = FALSE]
  y_selected <- test_df[[target]][selected_idx]
  selected_info <- data.frame(
    target = target,
    task = "regresja",
    obs_id = seq_along(selected_idx),
    indeks_test = selected_idx,
    rzeczywiste = y_selected,
    predykcja = as.numeric(predict(rf, newdata = X_selected))
  )
  save_csv(cbind(selected_info, X_selected), file.path(OUT_DIR, paste0("wybrane_obserwacje_regresja_", target, ".csv")))

  pred_fun <- function(model, newdata) as.numeric(predict(model, newdata = newdata))

  predictor_obj <- iml::Predictor$new(
    model = rf,
    data = X_background,
    y = train_df[[target]][as.integer(rownames(X_background))],
    predict.function = pred_fun
  )

  local_results <- run_local_iml(
    rf = rf,
    target = target,
    task = "regresja",
    X_selected = X_selected,
    y_selected = y_selected,
    pred_fun = pred_fun,
    predictor_obj = predictor_obj
  )

  shap_results <- run_fastshap_and_shapviz(
    rf = rf,
    target = target,
    task = "regresja",
    X_selected = X_selected,
    y_selected = y_selected,
    pred_fun = pred_fun
  )

  if (nrow(shap_results$global_summary) > 0) {
    all_shap_summaries[[paste0("regresja_", target)]] <- shap_results$global_summary
  }

  # Porownanie iml Shapley z fastshap dla wybranych obserwacji.
  if (nrow(local_results$shapley) > 0 && !is.null(shap_results$local)) {
    fast_long <- as.data.frame(shap_results$local[, predictors, drop = FALSE])
    fast_long$obs_id <- seq_len(nrow(fast_long))
    fast_long <- data.table::melt(
      as.data.table(fast_long),
      id.vars = "obs_id",
      variable.name = "feature",
      value.name = "phi_fastshap"
    )

    iml_long <- as.data.table(local_results$shapley)
    if (all(c("feature", "phi", "obs_id") %in% names(iml_long))) {
      cmp <- merge(
        iml_long[, .(obs_id, feature, phi_iml = phi)],
        fast_long,
        by = c("obs_id", "feature"),
        all = TRUE
      )
      cmp$target <- target
      cmp$task <- "regresja"
      save_csv(cmp, file.path(OUT_DIR, paste0("porownanie_shapley_iml_fastshap_regresja_", target, ".csv")))
    }
  }

  chapter_block(paste("Interpretowalnosc regresji dla targetu", target), {
    cat("Model randomForest:\n")
    print(rf)
    cat("\nMetryki regresji:\n")
    print(metrics)
    cat("\nWybrane obserwacje testowe:\n")
    print(selected_info)
    cat("\nTop importance RF:\n")
    print(head(imp, 10))
    if (!is.null(shap_results$global_summary) && nrow(shap_results$global_summary) > 0) {
      cat("\nTop global SHAP:\n")
      print(head(shap_results$global_summary, 10))
    }
    if (nrow(local_results$lime) > 0) {
      cat("\nLIME / LocalModel - pierwsze wiersze:\n")
      print(head(local_results$lime, 10))
    }
    if (nrow(local_results$shapley) > 0) {
      cat("\nShapley iml - pierwsze wiersze:\n")
      print(head(local_results$shapley, 10))
    }
  })
}

if (length(all_reg_metrics) > 0) {
  save_csv(do.call(rbind, all_reg_metrics), file.path(OUT_DIR, "metryki_rf_regresja_wszystkie.csv"))
}

# ----------------------------
# 5. Interpretacja modeli klasyfikacyjnych
# ----------------------------

log_section("MODELE KLASYFIKACYJNE - RANDOM FOREST + LIME/SHAP")

all_cls_metrics <- list()

for (target in class_targets) {
  log_section(paste("Klasyfikacja:", target))

  rf <- train_rf_classification(target)
  save_rds(rf, file.path(MODEL_DIR, paste0("rf_klasyfikacja_", target, ".rds")))

  pred_train_class <- predict(rf, newdata = X_train, type = "response")
  pred_test_class <- predict(rf, newdata = X_test, type = "response")

  pred_train_prob <- as.numeric(predict(rf, newdata = X_train, type = "prob")[, "Yes"])
  pred_test_prob <- as.numeric(predict(rf, newdata = X_test, type = "prob")[, "Yes"])

  metrics <- classification_metrics(train_df[[target]], pred_train_class, target, "randomForest_train")
  metrics_test <- classification_metrics(test_df[[target]], pred_test_class, target, "randomForest_test")
  metrics_all <- rbind(metrics, metrics_test)
  save_csv(metrics_all, file.path(OUT_DIR, paste0("metryki_rf_klasyfikacja_", target, ".csv")))
  all_cls_metrics[[target]] <- metrics_all

  tab_test <- table(predicted = pred_test_class, actual = factor(test_df[[target]], levels = c("No", "Yes")))
  save_csv(as.data.frame.matrix(tab_test), file.path(OUT_DIR, paste0("confusion_rf_klasyfikacja_", target, ".csv")))
  plot_confusion_heatmap(tab_test, paste("Random Forest - tablica pomylek -", target),
                         file.path(PLOT_DIR, paste0("confusion_rf_klasyfikacja_", target, ".png")))

  pred_df <- data.frame(
    target = target,
    zbior = "test",
    indeks_test = seq_len(nrow(test_df)),
    rzeczywiste = as.character(test_df[[target]]),
    pred_klasa = as.character(pred_test_class),
    prawdopodobienstwo_yes = pred_test_prob
  )
  save_csv(pred_df, file.path(OUT_DIR, paste0("predykcje_rf_klasyfikacja_", target, ".csv")))

  p_prob <- ggplot(pred_df[seq_len(min(1000, nrow(pred_df))), ],
                   aes(x = indeks_test, y = prawdopodobienstwo_yes)) +
    geom_line() +
    geom_point(aes(shape = rzeczywiste), size = 1.5) +
    labs(title = paste("Random Forest - prawdopodobienstwo klasy Yes -", target),
         x = "Indeks obserwacji testowej",
         y = "P(Yes)",
         shape = "Klasa rzeczywista") +
    theme_minimal()
  save_gg(p_prob, file.path(PLOT_DIR, paste0("prawdopodobienstwa_rf_klasyfikacja_", target, ".png")), width = 9, height = 5)

  imp <- make_importance_df(rf, target, "klasyfikacja")
  save_csv(imp, file.path(OUT_DIR, paste0("importance_rf_klasyfikacja_", target, ".csv")))
  all_rf_importance[[paste0("klasyfikacja_", target)]] <- imp

  save_base_plot(file.path(PLOT_DIR, paste0("importance_rf_klasyfikacja_", target, ".png")), {
    randomForest::varImpPlot(rf, main = paste("Waznosc predyktorow RF -", target))
  })

  selected_idx <- select_classification_obs(test_df, target, N_LOCAL_OBS)
  X_selected <- X_test[selected_idx, , drop = FALSE]
  y_selected <- as.character(test_df[[target]][selected_idx])
  selected_info <- data.frame(
    target = target,
    task = "klasyfikacja",
    obs_id = seq_along(selected_idx),
    indeks_test = selected_idx,
    rzeczywista_klasa = y_selected,
    pred_klasa = as.character(predict(rf, newdata = X_selected, type = "response")),
    prawdopodobienstwo_yes = as.numeric(predict(rf, newdata = X_selected, type = "prob")[, "Yes"])
  )
  save_csv(cbind(selected_info, X_selected), file.path(OUT_DIR, paste0("wybrane_obserwacje_klasyfikacja_", target, ".csv")))

  pred_fun_prob <- function(model, newdata) {
    prob <- predict(model, newdata = newdata, type = "prob")
    as.numeric(prob[, "Yes"])
  }

  # Dla klasyfikacji interpretujemy prawdopodobienstwo klasy Yes, a nie etykiete klasy.
  background_rows <- as.integer(rownames(X_background))
  y_background <- as.numeric(factor(train_df[[target]][background_rows], levels = c("No", "Yes")) == "Yes")

  predictor_obj <- iml::Predictor$new(
    model = rf,
    data = X_background,
    y = y_background,
    predict.function = pred_fun_prob
  )

  local_results <- run_local_iml(
    rf = rf,
    target = target,
    task = "klasyfikacja",
    X_selected = X_selected,
    y_selected = y_selected,
    pred_fun = pred_fun_prob,
    predictor_obj = predictor_obj
  )

  shap_results <- run_fastshap_and_shapviz(
    rf = rf,
    target = target,
    task = "klasyfikacja",
    X_selected = X_selected,
    y_selected = y_selected,
    pred_fun = pred_fun_prob
  )

  if (nrow(shap_results$global_summary) > 0) {
    all_shap_summaries[[paste0("klasyfikacja_", target)]] <- shap_results$global_summary
  }

  if (nrow(local_results$shapley) > 0 && !is.null(shap_results$local)) {
    fast_long <- as.data.frame(shap_results$local[, predictors, drop = FALSE])
    fast_long$obs_id <- seq_len(nrow(fast_long))
    fast_long <- data.table::melt(
      as.data.table(fast_long),
      id.vars = "obs_id",
      variable.name = "feature",
      value.name = "phi_fastshap"
    )

    iml_long <- as.data.table(local_results$shapley)
    if (all(c("feature", "phi", "obs_id") %in% names(iml_long))) {
      cmp <- merge(
        iml_long[, .(obs_id, feature, phi_iml = phi)],
        fast_long,
        by = c("obs_id", "feature"),
        all = TRUE
      )
      cmp$target <- target
      cmp$task <- "klasyfikacja"
      save_csv(cmp, file.path(OUT_DIR, paste0("porownanie_shapley_iml_fastshap_klasyfikacja_", target, ".csv")))
    }
  }

  chapter_block(paste("Interpretowalnosc klasyfikacji dla targetu", target), {
    cat("Model randomForest:\n")
    print(rf)
    cat("\nMetryki klasyfikacji:\n")
    print(metrics_all)
    cat("\nTablica pomylek - test:\n")
    print(tab_test)
    cat("\nWybrane obserwacje testowe:\n")
    print(selected_info)
    cat("\nTop importance RF:\n")
    print(head(imp, 10))
    if (!is.null(shap_results$global_summary) && nrow(shap_results$global_summary) > 0) {
      cat("\nTop global SHAP:\n")
      print(head(shap_results$global_summary, 10))
    }
    if (nrow(local_results$lime) > 0) {
      cat("\nLIME / LocalModel - pierwsze wiersze:\n")
      print(head(local_results$lime, 10))
    }
    if (nrow(local_results$shapley) > 0) {
      cat("\nShapley iml - pierwsze wiersze:\n")
      print(head(local_results$shapley, 10))
    }
  })
}

if (length(all_cls_metrics) > 0) {
  save_csv(do.call(rbind, all_cls_metrics), file.path(OUT_DIR, "metryki_rf_klasyfikacja_wszystkie.csv"))
}

# ----------------------------
# 6. Zbiorcze podsumowanie
# ----------------------------

log_section("PODSUMOWANIE")

if (length(all_rf_importance) > 0) {
  all_imp <- do.call(rbind, all_rf_importance)
  save_csv(all_imp, file.path(OUT_DIR, "importance_rf_wszystkie_modele.csv"))
}

if (length(all_shap_summaries) > 0) {
  all_shap <- do.call(rbind, all_shap_summaries)
  save_csv(all_shap, file.path(OUT_DIR, "shap_global_importance_wszystkie_modele.csv"))
}

summary_files <- data.frame(
  typ = c(
    "metryki regresji RF",
    "metryki klasyfikacji RF",
    "waznosc predyktorow RF",
    "globalna waznosc SHAP",
    "wybrane obserwacje regresja predkosc",
    "wybrane obserwacje klasyfikacja porywy",
    "log"
  ),
  plik = c(
    file.path(OUT_DIR, "metryki_rf_regresja_wszystkie.csv"),
    file.path(OUT_DIR, "metryki_rf_klasyfikacja_wszystkie.csv"),
    file.path(OUT_DIR, "importance_rf_wszystkie_modele.csv"),
    file.path(OUT_DIR, "shap_global_importance_wszystkie_modele.csv"),
    file.path(OUT_DIR, "wybrane_obserwacje_regresja_wind_speed_10m_next.csv"),
    file.path(OUT_DIR, "wybrane_obserwacje_klasyfikacja_wind_gusts_10m_next_event.csv"),
    LOG_FILE
  )
)

save_csv(summary_files, file.path(OUT_DIR, "najwazniejsze_pliki_do_rozdzialu.csv"))

chapter_block("Zbiorcze pliki do rozdzialu Interpretowalnosc modeli", {
  print(summary_files)
  cat("\nZadania wykonane analogicznie do notatnika:\n")
  cat("- dopasowanie modelu black-box randomForest;\n")
  cat("- LIME/LocalModel dla wybranych obserwacji testowych;\n")
  cat("- klasyczne wartosci Shapleya z iml dla wybranych obserwacji;\n")
  cat("- fastshap dla obserwacji testowych i porownanie z iml;\n")
  cat("- shapviz: waterfall i force dla wybranych obserwacji;\n")
  cat("- globalny SHAP: importance, dependence i beeswarm;\n")
  cat("- interpretacja regresji i klasyfikacji na danych pogodowych Chalupy.\n")
})

log_line("Koniec: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
log_line("Gotowe.")
