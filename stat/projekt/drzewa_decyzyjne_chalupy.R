# ============================================================
# drzewa_decyzyjne_chalupy.R
# Skrypt odtwarzajacy wyniki jak w rozdziale "Drzewa decyzyjne"
# dla danych pogodowych z Chalup.
#
# Zakres:
# - drzewa regresyjne dla wind_speed_10m_next i wind_gusts_10m_next
# - drzewa klasyfikacyjne dla wind_speed_10m_next_event i wind_gusts_10m_next_event
# - drzewo duze, drzewo przyciete wg minimum CV, drzewo 1SE
# - metryki, tablice pomylek, AUC, progi decyzyjne
# - wykresy drzew, krzywe przycinania, waznosci predyktorow, predykcje, ROC
#
# Domyslna sciezka danych:
#   data/chalupy_hourly_10000_days.csv
#
# Wyniki:
#   wyniki_drzewa_decyzyjne_chalupy/
#   plots_drzewa_decyzyjne_chalupy/
# ============================================================

options(stringsAsFactors = FALSE)
set.seed(2026)

# ----------------------------
# 0. Konfiguracja
# ----------------------------

DATA_PATH <- "data/chalupy_hourly_10000_days.csv"

RUN_NAME <- "drzewa_decyzyjne_chalupy"
OUT_DIR  <- paste0("wyniki_", RUN_NAME)
PLOT_DIR <- paste0("plots_", RUN_NAME)
LOG_DIR  <- file.path(OUT_DIR, "logi")
MODEL_DIR <- file.path(OUT_DIR, "modele")

dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PLOT_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(LOG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(MODEL_DIR, recursive = TRUE, showWarnings = FALSE)

LOG_FILE <- file.path(LOG_DIR, paste0(RUN_NAME, "_log.txt"))

# Podzial zgodny z dalszymi rozdzialami projektu: ostatnie ok. 449 dni godzinowych jako test.
TEST_ROWS <- 10774

# Progi z rozdzialu "Drzewa decyzyjne": 15 kt = 27.78 km/h, 25 kt = 46.30 km/h.
PROG_WIND_SPEED_EVENT <- 27.78
PROG_WIND_GUSTS_EVENT <- 46.30

# Parametry drzew.
XVAL_FOLDS <- 10
BIG_TREE_CP <- 0.00001
MIN_SPLIT <- 50
MIN_BUCKET <- 20
MAX_DEPTH <- 10

# Progi decyzyjne dla klasyfikacji beda dobierane na zbiorze treningowym
# przez maksymalizacje F1, a potem stosowane na zbiorze testowym.
THRESHOLD_GRID <- seq(0.01, 0.99, by = 0.01)

# ----------------------------
# 1. Pakiety i funkcje pomocnicze
# ----------------------------

required_packages <- c("data.table", "rpart", "rpart.plot", "ggplot2", "pROC")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Brakuje pakietow: ", paste(missing_packages, collapse = ", "),
    "\nZainstaluj je np. install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "), "))"
  )
}

library(data.table)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(pROC)

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

saved_file <- function(path) log_line("[SAVED_FILE] ", path)

chapter_block <- function(title, expr) {
  log_line("")
  log_line("[CHAPTER_CREATOR_BEGIN]")
  log_line(title)
  out <- capture.output(expr)
  if (length(out) > 0) for (x in out) log_line(x)
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

save_gg <- function(plot_obj, path, width = 9, height = 6, dpi = 150) {
  ok <- tryCatch({
    ggplot2::ggsave(path, plot_obj, width = width, height = height, dpi = dpi)
    TRUE
  }, error = function(e) {
    log_line("[BLAD WYKRESU] ", path, " | ", conditionMessage(e))
    png(path, width = 1600, height = 1000, res = 150)
    plot.new(); title("Blad wykresu"); text(0.5, 0.5, conditionMessage(e), cex = 0.8)
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
    plot.new(); title("Blad wykresu"); text(0.5, 0.5, conditionMessage(e), cex = 0.8)
    log_line("[BLAD WYKRESU] ", path, " | ", conditionMessage(e))
    FALSE
  })
  dev.off()
  saved_file(path)
  invisible(ok)
}

rmse <- function(y, pred) sqrt(mean((y - pred)^2, na.rm = TRUE))
mae  <- function(y, pred) mean(abs(y - pred), na.rm = TRUE)
mse  <- function(y, pred) mean((y - pred)^2, na.rm = TRUE)
r2_score <- function(y, pred) {
  1 - sum((y - pred)^2, na.rm = TRUE) / sum((y - mean(y, na.rm = TRUE))^2, na.rm = TRUE)
}

tree_info <- function(model, cp_used = NA_real_) {
  fr <- model$frame
  node_ids <- as.numeric(row.names(fr))
  data.frame(
    nsplit = sum(fr$var != "<leaf>"),
    leaves = sum(fr$var == "<leaf>"),
    depth = max(floor(log(node_ids, base = 2))),
    cp = cp_used
  )
}

reg_metrics <- function(y_train, p_train, y_test, p_test, target, model, tree_info) {
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
    R2_test = r2_score(y_test, p_test),
    nsplit = tree_info$nsplit,
    leaves = tree_info$leaves,
    depth = tree_info$depth,
    cp = tree_info$cp
  )
}

classification_metrics_from_prob <- function(actual, prob, threshold, target, model) {
  actual <- factor(actual, levels = c("No", "Yes"))
  pred <- factor(ifelse(prob >= threshold, "Yes", "No"), levels = c("No", "Yes"))
  tab <- table(predicted = pred, actual = actual)

  TP <- as.numeric(tab["Yes", "Yes"]); TN <- as.numeric(tab["No", "No"])
  FP <- as.numeric(tab["Yes", "No"]);  FN <- as.numeric(tab["No", "Yes"])

  accuracy <- (TP + TN) / sum(tab)
  sensitivity <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
  specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
  precision <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
  f1 <- ifelse(!is.na(precision) && !is.na(sensitivity) && (precision + sensitivity) > 0,
               2 * precision * sensitivity / (precision + sensitivity), NA)
  error_rate <- mean(pred != actual)

  auc_val <- tryCatch({
    as.numeric(pROC::auc(pROC::roc(response = actual, predictor = prob, levels = c("No", "Yes"), direction = "<", quiet = TRUE)))
  }, error = function(e) NA_real_)

  list(
    metrics = data.frame(
      target = target,
      model = model,
      threshold = threshold,
      AUC = auc_val,
      accuracy = accuracy,
      error_rate = error_rate,
      sensitivity = sensitivity,
      specificity = specificity,
      precision = precision,
      F1 = f1,
      TP = as.integer(TP), TN = as.integer(TN), FP = as.integer(FP), FN = as.integer(FN)
    ),
    confusion = tab,
    predicted = pred
  )
}

choose_threshold_f1 <- function(actual, prob, grid = THRESHOLD_GRID) {
  scores <- lapply(grid, function(th) {
    m <- classification_metrics_from_prob(actual, prob, th, target = "tmp", model = "tmp")$metrics
    data.frame(threshold = th, F1 = m$F1, sensitivity = m$sensitivity, specificity = m$specificity, precision = m$precision)
  })
  scores <- do.call(rbind, scores)
  scores <- scores[order(-scores$F1, -scores$sensitivity, -scores$precision), ]
  scores$threshold[1]
}

select_cp_min_1se <- function(model) {
  ct <- as.data.frame(model$cptable)
  idx_min <- which.min(ct$xerror)
  min_xerror <- ct$xerror[idx_min]
  min_xstd <- ct$xstd[idx_min]
  limit <- min_xerror + min_xstd
  idx_1se <- which(ct$xerror <= limit)[1]
  list(
    cp_min = ct$CP[idx_min],
    cp_1se = ct$CP[idx_1se],
    row_min = ct[idx_min, ],
    row_1se = ct[idx_1se, ],
    cptable = ct
  )
}

plot_tree <- function(model, path, title) {
  save_base_plot(path, {
    rpart.plot::rpart.plot(model, main = title, type = 2, extra = 101, fallen.leaves = TRUE, cex = 0.55)
  }, width = 1800, height = 1200, res = 150)
}

plot_cp_curve <- function(model, path, title) {
  save_base_plot(path, plotcp(model, main = title), width = 1400, height = 1000, res = 150)
}

extract_importance <- function(model, target, task, model_name) {
  vi <- model$variable.importance
  if (is.null(vi) || length(vi) == 0) {
    return(data.frame(target = character(), task = character(), model = character(), feature = character(), importance = numeric()))
  }
  data.frame(
    target = target,
    task = task,
    model = model_name,
    feature = names(vi),
    importance = as.numeric(vi),
    row.names = NULL
  )[order(-as.numeric(vi)), ]
}

plot_importance <- function(imp_df, path, title) {
  if (nrow(imp_df) == 0) return(invisible(FALSE))
  p <- ggplot(imp_df[order(imp_df$importance), ], aes(x = reorder(feature, importance), y = importance)) +
    geom_col() + coord_flip() + labs(title = title, x = "Predyktor", y = "Waznosc") + theme_minimal()
  save_gg(p, path, width = 8, height = 6)
}

save_rules <- function(model, path) {
  txt <- tryCatch(
    capture.output(rpart.plot::rpart.rules(model, roundint = FALSE)),
    error = function(e) paste("Nie udalo sie zapisac regul:", conditionMessage(e))
  )
  writeLines(txt, path)
  saved_file(path)
}

plot_reg_predictions <- function(actual, pred, target, model_name, path, n_plot = 500) {
  d <- data.frame(indeks = seq_along(actual), rzeczywiste = actual, predykcja = pred)
  d <- d[seq_len(min(n_plot, nrow(d))), ]
  p <- ggplot(d, aes(x = indeks)) +
    geom_line(aes(y = rzeczywiste, linetype = "rzeczywiste")) +
    geom_line(aes(y = predykcja, linetype = "predykcja")) +
    labs(title = paste("Drzewo regresyjne -", model_name, "-", target), x = "Indeks obserwacji testowej", y = target, linetype = "") +
    theme_minimal()
  save_gg(p, path, width = 9, height = 5)
}

plot_cls_prob <- function(actual, prob, threshold, target, model_name, path, n_plot = 1000) {
  d <- data.frame(indeks = seq_along(actual), rzeczywiste = as.character(actual), prob_yes = prob)
  d <- d[seq_len(min(n_plot, nrow(d))), ]
  p <- ggplot(d, aes(x = indeks, y = prob_yes)) +
    geom_line() + geom_point(aes(shape = rzeczywiste), size = 1.4) +
    geom_hline(yintercept = threshold, linetype = "dashed") +
    labs(title = paste("Drzewo klasyfikacyjne -", model_name, "-", target), x = "Indeks obserwacji testowej", y = "P(Yes)", shape = "Klasa") +
    theme_minimal()
  save_gg(p, path, width = 9, height = 5)
}

plot_confusion_heatmap <- function(tab, title, path) {
  d <- as.data.frame(tab)
  names(d) <- c("predicted", "actual", "n")
  p <- ggplot(d, aes(x = actual, y = predicted, fill = n)) +
    geom_tile() + geom_text(aes(label = n), size = 5) +
    labs(title = title, x = "Klasa rzeczywista", y = "Klasa przewidywana") + theme_minimal()
  save_gg(p, path, width = 7, height = 5)
}

plot_roc_curve <- function(actual, prob, title, path) {
  roc_obj <- tryCatch(
    pROC::roc(response = factor(actual, levels = c("No", "Yes")), predictor = prob, levels = c("No", "Yes"), direction = "<", quiet = TRUE),
    error = function(e) NULL
  )
  if (is.null(roc_obj)) return(invisible(FALSE))
  d <- data.frame(specificity = roc_obj$specificities, sensitivity = roc_obj$sensitivities)
  auc_val <- as.numeric(pROC::auc(roc_obj))
  p <- ggplot(d, aes(x = 1 - specificity, y = sensitivity)) +
    geom_line() + geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    labs(title = paste0(title, " | AUC = ", round(auc_val, 4)), x = "1 - specificity", y = "sensitivity") +
    theme_minimal()
  save_gg(p, path, width = 7, height = 6)
}

# ----------------------------
# 2. Wczytanie i przygotowanie danych
# ----------------------------

log_section("DRZEWA DECYZYJNE - DANE CHALUPY")
log_line("Start: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
log_line("Dane: ", DATA_PATH)
log_line("Wyniki: ", OUT_DIR)
log_line("Wykresy: ", PLOT_DIR)
log_line("Log: ", LOG_FILE)

if (!file.exists(DATA_PATH)) stop("Nie znaleziono pliku danych: ", DATA_PATH)

dt <- data.table::fread(DATA_PATH)
log_line("Wczytano danych: ", nrow(dt), " wierszy, ", ncol(dt), " kolumn.")

time_candidates <- intersect(names(dt), c("time", "datetime", "date", "data", "timestamp"))
if (length(time_candidates) > 0) {
  time_col <- time_candidates[1]
  log_line("Wykryto kolumne czasu: ", time_col)
  parsed_time <- suppressWarnings(as.POSIXct(dt[[time_col]], tz = "UTC"))
  if (all(is.na(parsed_time))) parsed_time <- suppressWarnings(as.POSIXct(dt[[time_col]], format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
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

if (!all(c("wind_dir_sin", "wind_dir_cos") %in% names(dt))) {
  dir_candidates <- intersect(names(dt), c("wind_direction_10m", "wind_direction_100m", "wind_dir", "wind_direction"))
  if (length(dir_candidates) > 0) {
    dir_col <- dir_candidates[1]
    log_line("Tworze wind_dir_sin/cos z kolumny: ", dir_col)
    rad <- dt[[dir_col]] * pi / 180
    if (!"wind_dir_sin" %in% names(dt)) dt[, wind_dir_sin := sin(rad)]
    if (!"wind_dir_cos" %in% names(dt)) dt[, wind_dir_cos := cos(rad)]
  }
}

if (!"wind_speed_10m_next" %in% names(dt) && "wind_speed_10m" %in% names(dt)) {
  dt[, wind_speed_10m_next := data.table::shift(wind_speed_10m, type = "lead")]
}
if (!"wind_gusts_10m_next" %in% names(dt) && "wind_gusts_10m" %in% names(dt)) {
  dt[, wind_gusts_10m_next := data.table::shift(wind_gusts_10m, type = "lead")]
}

# Wymuszenie progow z rozdzialu drzew.
if ("wind_speed_10m_next" %in% names(dt)) {
  dt[, wind_speed_10m_next_event := ifelse(wind_speed_10m_next >= PROG_WIND_SPEED_EVENT, "Yes", "No")]
  log_line("Utworzono wind_speed_10m_next_event z progiem: ", PROG_WIND_SPEED_EVENT)
}
if ("wind_gusts_10m_next" %in% names(dt)) {
  dt[, wind_gusts_10m_next_event := ifelse(wind_gusts_10m_next >= PROG_WIND_GUSTS_EVENT, "Yes", "No")]
  log_line("Utworzono wind_gusts_10m_next_event z progiem: ", PROG_WIND_GUSTS_EVENT)
}

predictors_base <- c(
  "wind_speed_10m", "wind_speed_100m", "wind_gusts_10m", "wind_dir_sin", "wind_dir_cos",
  "temperature_2m", "relative_humidity_2m", "dew_point_2m", "apparent_temperature",
  "precipitation", "cloud_cover", "pressure_msl", "shortwave_radiation", "is_day",
  "sin_dzien", "cos_dzien", "sin_godzina", "cos_godzina", "polrocze_cieple"
)

reg_targets <- c("wind_speed_10m_next", "wind_gusts_10m_next")
class_targets <- c("wind_speed_10m_next_event", "wind_gusts_10m_next_event")

predictors <- intersect(predictors_base, names(dt))
reg_targets <- intersect(reg_targets, names(dt))
class_targets <- intersect(class_targets, names(dt))

if (length(reg_targets) == 0) stop("Brak targetow regresyjnych.")
if (length(class_targets) == 0) stop("Brak targetow klasyfikacyjnych.")
if (length(predictors) == 0) stop("Brak predyktorow.")

for (v in predictors) if (is.integer(dt[[v]])) dt[[v]] <- as.numeric(dt[[v]])
for (target in class_targets) dt[[target]] <- factor(dt[[target]], levels = c("No", "Yes"))

all_needed <- unique(c(predictors, reg_targets, class_targets))
df <- as.data.frame(dt[, ..all_needed])
df <- na.omit(df)

is_constant <- vapply(df[, predictors, drop = FALSE], function(x) length(unique(x)) < 2, logical(1))
if (any(is_constant)) {
  log_line("Usunieto stale predyktory: ", paste(names(is_constant)[is_constant], collapse = ", "))
  predictors <- setdiff(predictors, names(is_constant)[is_constant])
}

df <- df[, unique(c(predictors, reg_targets, class_targets)), drop = FALSE]
df <- na.omit(df)

n <- nrow(df)
n_test <- min(TEST_ROWS, max(1, floor(0.2 * n)))
if (n <= n_test + 100) n_test <- floor(0.2 * n)

idx_train <- seq_len(n - n_test)
idx_test <- (n - n_test + 1):n
train_df <- df[idx_train, , drop = FALSE]
test_df  <- df[idx_test, , drop = FALSE]

chapter_block("Opis konfiguracji analizy drzew decyzyjnych", {
  print(data.frame(
    element = c(
      "liczba_obserwacji_po_przygotowaniu", "liczba_obserwacji_train", "liczba_obserwacji_test",
      "liczba_predyktorow", "targety_regresyjne", "targety_klasyfikacyjne",
      "prog_wind_speed_event_kmh", "prog_wind_gusts_event_kmh",
      "xval_folds", "big_tree_cp", "minsplit", "minbucket", "maxdepth"
    ),
    wartosc = c(
      nrow(df), nrow(train_df), nrow(test_df), length(predictors),
      paste(reg_targets, collapse = ", "), paste(class_targets, collapse = ", "),
      PROG_WIND_SPEED_EVENT, PROG_WIND_GUSTS_EVENT, XVAL_FOLDS, BIG_TREE_CP, MIN_SPLIT, MIN_BUCKET, MAX_DEPTH
    )
  ))
  cat("\nPredyktory:\n")
  print(data.frame(predyktor = predictors))
  cat("\nRozklady targetow klasyfikacyjnych:\n")
  for (target in class_targets) {
    cat("\n", target, " train:\n", sep = ""); print(table(train_df[[target]]))
    cat("\n", target, " test:\n", sep = ""); print(table(test_df[[target]]))
  }
})

save_csv(data.frame(predyktor = predictors), file.path(OUT_DIR, "lista_predyktorow.csv"))
save_csv(data.frame(target_regresyjny = reg_targets), file.path(OUT_DIR, "targety_regresyjne.csv"))
save_csv(data.frame(target_klasyfikacyjny = class_targets), file.path(OUT_DIR, "targety_klasyfikacyjne.csv"))

# ----------------------------
# 3. Drzewa regresyjne
# ----------------------------

log_section("DRZEWA REGRESYJNE")
all_reg_metrics <- list()
all_importance <- list()

for (target in reg_targets) {
  log_section(paste("Drzewo regresyjne:", target))
  formula_tree <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))

  fit_big <- rpart::rpart(
    formula_tree,
    data = train_df,
    method = "anova",
    control = rpart.control(cp = BIG_TREE_CP, minsplit = MIN_SPLIT, minbucket = MIN_BUCKET, maxdepth = MAX_DEPTH, xval = XVAL_FOLDS)
  )

  cp_sel <- select_cp_min_1se(fit_big)
  fit_min <- prune(fit_big, cp = cp_sel$cp_min)
  fit_1se <- prune(fit_big, cp = cp_sel$cp_1se)

  models <- list(duze = list(fit = fit_big, cp = BIG_TREE_CP), min_cv = list(fit = fit_min, cp = cp_sel$cp_min), one_se = list(fit = fit_1se, cp = cp_sel$cp_1se))

  save_rds(fit_big, file.path(MODEL_DIR, paste0("rpart_regresja_duze_", target, ".rds")))
  save_rds(fit_min, file.path(MODEL_DIR, paste0("rpart_regresja_min_cv_", target, ".rds")))
  save_rds(fit_1se, file.path(MODEL_DIR, paste0("rpart_regresja_1se_", target, ".rds")))
  save_csv(cp_sel$cptable, file.path(OUT_DIR, paste0("cptable_regresja_", target, ".csv")))
  plot_cp_curve(fit_big, file.path(PLOT_DIR, paste0("cp_regresja_", target, ".png")), paste("Krzywa przycinania - regresja -", target))

  metrics_target <- list()
  for (model_name in names(models)) {
    model <- models[[model_name]]$fit
    cp_used <- models[[model_name]]$cp
    pred_train <- as.numeric(predict(model, newdata = train_df))
    pred_test <- as.numeric(predict(model, newdata = test_df))

    info <- tree_info(model, cp_used)
    met <- reg_metrics(train_df[[target]], pred_train, test_df[[target]], pred_test, target, paste0("rpart_", model_name), info)
    metrics_target[[model_name]] <- met

    pred_df <- data.frame(target = target, model = paste0("rpart_", model_name), indeks_test = seq_len(nrow(test_df)), rzeczywiste = test_df[[target]], predykcja = pred_test, blad = test_df[[target]] - pred_test)
    save_csv(pred_df, file.path(OUT_DIR, paste0("predykcje_regresja_", model_name, "_", target, ".csv")))

    plot_reg_predictions(test_df[[target]], pred_test, target, model_name, file.path(PLOT_DIR, paste0("predykcje_regresja_", model_name, "_", target, ".png")))
    plot_tree(model, file.path(PLOT_DIR, paste0("drzewo_regresja_", model_name, "_", target, ".png")), paste("Drzewo regresyjne", model_name, "-", target))
    save_rules(model, file.path(OUT_DIR, paste0("reguly_regresja_", model_name, "_", target, ".txt")))

    imp <- extract_importance(model, target = target, task = "regresja", model_name = paste0("rpart_", model_name))
    save_csv(imp, file.path(OUT_DIR, paste0("importance_regresja_", model_name, "_", target, ".csv")))
    plot_importance(imp, file.path(PLOT_DIR, paste0("importance_regresja_", model_name, "_", target, ".png")), paste("Waznosc predyktorow - regresja -", model_name, "-", target))
    all_importance[[paste0("reg_", target, "_", model_name)]] <- imp
  }

  metrics_target_df <- do.call(rbind, metrics_target)
  save_csv(metrics_target_df, file.path(OUT_DIR, paste0("metryki_regresja_", target, ".csv")))
  all_reg_metrics[[target]] <- metrics_target_df

  best_by_rmse <- metrics_target_df[which.min(metrics_target_df$RMSE_test), ]
  best_by_r2 <- metrics_target_df[which.max(metrics_target_df$R2_test), ]
  save_csv(rbind(best_by_rmse, best_by_r2), file.path(OUT_DIR, paste0("najlepsze_drzewa_regresja_", target, ".csv")))

  chapter_block(paste("Drzewa regresyjne dla targetu", target), {
    cat("Tabela cp:\n"); print(cp_sel$cptable)
    cat("\nWybor minimum CV:\n"); print(cp_sel$row_min)
    cat("\nWybor 1SE:\n"); print(cp_sel$row_1se)
    cat("\nMetryki modeli:\n"); print(metrics_target_df)
    cat("\nNajlepszy model wg RMSE_test:\n"); print(best_by_rmse)
    cat("\nTop importance dla duzego drzewa:\n"); print(head(all_importance[[paste0("reg_", target, "_duze")]], 10))
  })
}

if (length(all_reg_metrics) > 0) save_csv(do.call(rbind, all_reg_metrics), file.path(OUT_DIR, "metryki_regresja_wszystkie.csv"))

# ----------------------------
# 4. Drzewa klasyfikacyjne
# ----------------------------

log_section("DRZEWA KLASYFIKACYJNE")
all_cls_metrics <- list()

for (target in class_targets) {
  log_section(paste("Drzewo klasyfikacyjne:", target))
  formula_tree <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))

  fit_big <- rpart::rpart(
    formula_tree,
    data = train_df,
    method = "class",
    parms = list(split = "gini"),
    control = rpart.control(cp = BIG_TREE_CP, minsplit = MIN_SPLIT, minbucket = MIN_BUCKET, maxdepth = MAX_DEPTH, xval = XVAL_FOLDS)
  )

  cp_sel <- select_cp_min_1se(fit_big)
  fit_min <- prune(fit_big, cp = cp_sel$cp_min)
  fit_1se <- prune(fit_big, cp = cp_sel$cp_1se)

  models <- list(duze = list(fit = fit_big, cp = BIG_TREE_CP), min_cv = list(fit = fit_min, cp = cp_sel$cp_min), one_se = list(fit = fit_1se, cp = cp_sel$cp_1se))

  save_rds(fit_big, file.path(MODEL_DIR, paste0("rpart_klasyfikacja_duze_", target, ".rds")))
  save_rds(fit_min, file.path(MODEL_DIR, paste0("rpart_klasyfikacja_min_cv_", target, ".rds")))
  save_rds(fit_1se, file.path(MODEL_DIR, paste0("rpart_klasyfikacja_1se_", target, ".rds")))
  save_csv(cp_sel$cptable, file.path(OUT_DIR, paste0("cptable_klasyfikacja_", target, ".csv")))
  plot_cp_curve(fit_big, file.path(PLOT_DIR, paste0("cp_klasyfikacja_", target, ".png")), paste("Krzywa przycinania - klasyfikacja -", target))

  metrics_target <- list()
  for (model_name in names(models)) {
    model <- models[[model_name]]$fit
    cp_used <- models[[model_name]]$cp

    prob_train <- as.numeric(predict(model, newdata = train_df, type = "prob")[, "Yes"])
    prob_test <- as.numeric(predict(model, newdata = test_df, type = "prob")[, "Yes"])
    threshold <- choose_threshold_f1(train_df[[target]], prob_train, THRESHOLD_GRID)

    m_train <- classification_metrics_from_prob(train_df[[target]], prob_train, threshold, target, paste0("rpart_", model_name, "_train"))
    m_test <- classification_metrics_from_prob(test_df[[target]], prob_test, threshold, target, paste0("rpart_", model_name, "_test"))

    info <- tree_info(model, cp_used)
    for (obj in list(m_train, m_test)) {
      obj$metrics$nsplit <- info$nsplit
      obj$metrics$leaves <- info$leaves
      obj$metrics$depth <- info$depth
      obj$metrics$cp <- info$cp
    }
    m_train$metrics$nsplit <- info$nsplit; m_train$metrics$leaves <- info$leaves; m_train$metrics$depth <- info$depth; m_train$metrics$cp <- info$cp
    m_test$metrics$nsplit <- info$nsplit; m_test$metrics$leaves <- info$leaves; m_test$metrics$depth <- info$depth; m_test$metrics$cp <- info$cp

    metrics_target[[model_name]] <- rbind(m_train$metrics, m_test$metrics)

    pred_df <- data.frame(target = target, model = paste0("rpart_", model_name), indeks_test = seq_len(nrow(test_df)), rzeczywiste = as.character(test_df[[target]]), prob_yes = prob_test, threshold = threshold, pred_klasa = as.character(m_test$predicted))
    save_csv(pred_df, file.path(OUT_DIR, paste0("predykcje_klasyfikacja_", model_name, "_", target, ".csv")))

    save_csv(as.data.frame.matrix(m_train$confusion), file.path(OUT_DIR, paste0("confusion_train_klasyfikacja_", model_name, "_", target, ".csv")))
    save_csv(as.data.frame.matrix(m_test$confusion), file.path(OUT_DIR, paste0("confusion_test_klasyfikacja_", model_name, "_", target, ".csv")))
    plot_confusion_heatmap(m_test$confusion, paste("Tablica pomylek -", model_name, "-", target), file.path(PLOT_DIR, paste0("confusion_test_klasyfikacja_", model_name, "_", target, ".png")))
    plot_cls_prob(test_df[[target]], prob_test, threshold, target, model_name, file.path(PLOT_DIR, paste0("prawdopodobienstwa_klasyfikacja_", model_name, "_", target, ".png")))
    plot_roc_curve(test_df[[target]], prob_test, paste("ROC -", model_name, "-", target), file.path(PLOT_DIR, paste0("roc_klasyfikacja_", model_name, "_", target, ".png")))
    plot_tree(model, file.path(PLOT_DIR, paste0("drzewo_klasyfikacja_", model_name, "_", target, ".png")), paste("Drzewo klasyfikacyjne", model_name, "-", target))
    save_rules(model, file.path(OUT_DIR, paste0("reguly_klasyfikacja_", model_name, "_", target, ".txt")))

    imp <- extract_importance(model, target = target, task = "klasyfikacja", model_name = paste0("rpart_", model_name))
    save_csv(imp, file.path(OUT_DIR, paste0("importance_klasyfikacja_", model_name, "_", target, ".csv")))
    plot_importance(imp, file.path(PLOT_DIR, paste0("importance_klasyfikacja_", model_name, "_", target, ".png")), paste("Waznosc predyktorow - klasyfikacja -", model_name, "-", target))
    all_importance[[paste0("cls_", target, "_", model_name)]] <- imp
  }

  metrics_target_df <- do.call(rbind, metrics_target)
  save_csv(metrics_target_df, file.path(OUT_DIR, paste0("metryki_klasyfikacja_", target, ".csv")))
  all_cls_metrics[[target]] <- metrics_target_df

  test_rows <- metrics_target_df[grepl("_test$", metrics_target_df$model), ]
  best_by_auc <- test_rows[which.max(test_rows$AUC), ]
  best_by_f1 <- test_rows[which.max(test_rows$F1), ]
  save_csv(rbind(best_by_auc, best_by_f1), file.path(OUT_DIR, paste0("najlepsze_drzewa_klasyfikacja_", target, ".csv")))

  chapter_block(paste("Drzewa klasyfikacyjne dla targetu", target), {
    cat("Rozklad klas train:\n"); print(table(train_df[[target]]))
    cat("\nRozklad klas test:\n"); print(table(test_df[[target]]))
    cat("\nTabela cp:\n"); print(cp_sel$cptable)
    cat("\nWybor minimum CV:\n"); print(cp_sel$row_min)
    cat("\nWybor 1SE:\n"); print(cp_sel$row_1se)
    cat("\nMetryki modeli:\n"); print(metrics_target_df)
    cat("\nNajlepszy model wg AUC_test:\n"); print(best_by_auc)
    cat("\nNajlepszy model wg F1_test:\n"); print(best_by_f1)
    cat("\nTop importance dla duzego drzewa:\n"); print(head(all_importance[[paste0("cls_", target, "_duze")]], 10))
  })
}

if (length(all_cls_metrics) > 0) save_csv(do.call(rbind, all_cls_metrics), file.path(OUT_DIR, "metryki_klasyfikacja_wszystkie.csv"))

# ----------------------------
# 5. Zbiorcze podsumowanie
# ----------------------------

log_section("PODSUMOWANIE")
if (length(all_importance) > 0) save_csv(do.call(rbind, all_importance), file.path(OUT_DIR, "importance_wszystkie_drzewa.csv"))

summary_files <- data.frame(
  typ = c(
    "metryki regresji", "metryki klasyfikacji", "importance wszystkich drzew", "lista predyktorow", "log",
    "wykres cp regresja wind_speed", "wykres cp regresja wind_gusts",
    "wykres cp klasyfikacja wind_speed_event", "wykres cp klasyfikacja wind_gusts_event"
  ),
  plik = c(
    file.path(OUT_DIR, "metryki_regresja_wszystkie.csv"),
    file.path(OUT_DIR, "metryki_klasyfikacja_wszystkie.csv"),
    file.path(OUT_DIR, "importance_wszystkie_drzewa.csv"),
    file.path(OUT_DIR, "lista_predyktorow.csv"),
    LOG_FILE,
    file.path(PLOT_DIR, "cp_regresja_wind_speed_10m_next.png"),
    file.path(PLOT_DIR, "cp_regresja_wind_gusts_10m_next.png"),
    file.path(PLOT_DIR, "cp_klasyfikacja_wind_speed_10m_next_event.png"),
    file.path(PLOT_DIR, "cp_klasyfikacja_wind_gusts_10m_next_event.png")
  )
)

save_csv(summary_files, file.path(OUT_DIR, "najwazniejsze_pliki_do_rozdzialu.csv"))

chapter_block("Zbiorcze pliki do rozdzialu Drzewa decyzyjne", {
  print(summary_files)
  cat("\nZadania wykonane jak w rozdziale Drzewa decyzyjne:\n")
  cat("- przygotowanie targetow wind_speed_10m_next i wind_gusts_10m_next;\n")
  cat("- przygotowanie targetow klasyfikacyjnych przekroczenia progow 15 kt i 25 kt;\n")
  cat("- dopasowanie duzych drzew rpart dla regresji i klasyfikacji;\n")
  cat("- wybor drzew przycietych wg minimum CV i reguly 1SE;\n")
  cat("- porownanie modeli regresyjnych przez MSE, RMSE, MAE i R2;\n")
  cat("- porownanie modeli klasyfikacyjnych przez AUC, accuracy, sensitivity, specificity, precision i F1;\n")
  cat("- zapis regul decyzyjnych, wykresow drzew, krzywych cp, predykcji, ROC i tablic pomylek.\n")
})

log_line("Koniec: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
log_line("Gotowe.")
