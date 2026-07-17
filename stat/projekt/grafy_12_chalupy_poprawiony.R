# ============================================================
# 12_grafy_chalupy.R
# Modele graficzne - adaptacja notatnika 12-grafy.Rmd na dane pogodowe Chalupy
#
# Skrypt tworzy wyniki pod kolejny rozdzial projektu:
# - dyskretne sieci bayesowskie / DGM,
# - wnioskowanie probabilistyczne,
# - NBC i TAN dla klasyfikacji zdarzen wiatrowych,
# - Gaussowskie sieci bayesowskie / GBN dla regresji,
# - modele hybrydowe,
# - porownanie z modelami liniowymi.
#
# Domyslna sciezka danych:
#   data/chalupy_hourly_10000_days.csv
#
# Wyniki:
#   wyniki_grafy_12_chalupy/
#   plots_grafy_12_chalupy/
# ============================================================

options(stringsAsFactors = FALSE)
set.seed(123)

# ----------------------------
# 0. Konfiguracja
# ----------------------------

DATA_PATH <- "data/chalupy_hourly_10000_days.csv"

RUN_NAME <- "grafy_12_chalupy"
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
# Jezeli Twoj poprzedni skrypt tworzyl targety klasyfikacyjne inaczej,
# najlepiej wczytac dane juz z kolumnami:
#   wind_speed_10m_next_event
#   wind_gusts_10m_next_event
PROG_WIND_SPEED_EVENT <- 30   # km/h
PROG_WIND_GUSTS_EVENT <- 45   # km/h

# Liczba wierszy testowych zgodna z poprzednimi etapami projektu.
# Przy danych godzinowych 449 dni daje okolo 10776 obserwacji.
TEST_ROWS <- 10774

# Uczenie struktury sieci moze byc kosztowne.
# Dla pelnego uczenia struktury ustaw Inf.
# Parametry sieci i ocena predykcji sa wykonywane na pelnym zbiorze train/test.
STRUCTURE_MAX_ROWS <- 50000

# Liczba prob w przyblizonym wnioskowaniu.
CPQUERY_N <- 20000
BAYES_LW_N <- 2000

# Liczba przedzialow do dyskretyzacji zmiennych ciaglych.
N_BINS <- 3
BIN_LABELS <- c("niska", "srednia", "wysoka")

# ----------------------------
# 1. Pakiety i funkcje pomocnicze
# ----------------------------

required_packages <- c("data.table", "bnlearn", "ggplot2")

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
library(bnlearn)
library(ggplot2)

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

save_plot_png <- function(path, expr, width = 1600, height = 1200, res = 150) {
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

sample_structure_data <- function(df, max_rows = STRUCTURE_MAX_ROWS) {
  df <- as.data.frame(df)
  if (is.infinite(max_rows) || nrow(df) <= max_rows) return(df)
  df[sample(seq_len(nrow(df)), max_rows), , drop = FALSE]
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

  ggsave(path, p, width = 7, height = 5, dpi = 150)
  saved_file(path)
}

safe_predict_bn <- function(fit, node, data, method = "exact", n = NULL) {
  nd <- as.data.frame(data)
  if (node %in% names(nd)) {
    if (is.numeric(nd[[node]])) {
      nd[[node]] <- NA_real_
    } else {
      nd[[node]] <- NA
    }
  }

  if (is.null(n)) {
    predict(fit, node = node, data = nd, method = method)
  } else {
    predict(fit, node = node, data = nd, method = method, n = n)
  }
}

fit_lm_baseline <- function(train, test, target, predictors) {
  predictors <- predictors[predictors %in% names(train)]
  predictors <- predictors[vapply(train[, predictors, drop = FALSE], is.numeric, logical(1))]
  predictors <- setdiff(predictors, target)

  formula_lm <- as.formula(paste(target, "~", paste(predictors, collapse = " + ")))
  fit <- lm(formula_lm, data = train)
  pred_train <- predict(fit, newdata = train)
  pred_test <- predict(fit, newdata = test)

  list(
    fit = fit,
    metrics = reg_metrics(train[[target]], pred_train, test[[target]], pred_test, target, "LM_OLS"),
    pred_train = pred_train,
    pred_test = pred_test
  )
}

# ----------------------------
# 2. Wczytanie i przygotowanie danych
# ----------------------------

log_section("MODELE GRAFICZNE 12 - DANE CHALUPY")
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

# Proba uporzadkowania po czasie, jezeli istnieje kolumna czasowa.
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
    # Cechy sezonowe, jesli nie istnieja.
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

# Cechy cykliczne / pomocnicze, jezeli nie bylo kolumny czasu.
if (!"is_day" %in% names(dt) && "shortwave_radiation" %in% names(dt)) {
  dt[, is_day := as.integer(shortwave_radiation > 0)]
}

# Targety przesuniete o godzine, jezeli nie istnieja.
if (!"wind_speed_10m_next" %in% names(dt) && "wind_speed_10m" %in% names(dt)) {
  dt[, wind_speed_10m_next := data.table::shift(wind_speed_10m, type = "lead")]
}
if (!"wind_gusts_10m_next" %in% names(dt) && "wind_gusts_10m" %in% names(dt)) {
  dt[, wind_gusts_10m_next := data.table::shift(wind_gusts_10m, type = "lead")]
}

# Targety klasyfikacyjne, jezeli nie istnieja.
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

reg_targets <- c("wind_speed_10m_next", "wind_gusts_10m_next")
class_targets <- c("wind_speed_10m_next_event", "wind_gusts_10m_next_event")

predictors <- intersect(predictors_base, names(dt))
reg_targets <- intersect(reg_targets, names(dt))
class_targets <- intersect(class_targets, names(dt))

if (length(reg_targets) == 0) stop("Brak targetow regresyjnych.")
if (length(class_targets) == 0) stop("Brak targetow klasyfikacyjnych.")
if (length(predictors) == 0) stop("Brak predyktorow.")

# Uporzadkowanie typow.
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
is_constant <- vapply(df, function(x) length(unique(x)) < 2, logical(1))
if (any(is_constant)) {
  log_line("Usunieto stale kolumny: ", paste(names(df)[is_constant], collapse = ", "))
  df <- df[, !is_constant, drop = FALSE]
  predictors <- setdiff(predictors, names(is_constant)[is_constant])
}

n <- nrow(df)
n_test <- min(TEST_ROWS, max(1, floor(0.2 * n)))
if (n <= n_test + 100) {
  n_test <- floor(0.2 * n)
}
idx_train <- seq_len(n - n_test)
idx_test <- (n - n_test + 1):n

train_df <- df[idx_train, , drop = FALSE]
test_df  <- df[idx_test, , drop = FALSE]

chapter_block("Opis konfiguracji analizy modeli graficznych", {
  print(data.frame(
    element = c(
      "liczba_obserwacji_po_przygotowaniu",
      "liczba_obserwacji_train",
      "liczba_obserwacji_test",
      "liczba_predyktorow",
      "targety_regresyjne",
      "targety_klasyfikacyjne",
      "liczba_przedzialow_dyskretyzacji",
      "maks_wierszy_do_uczenia_struktury"
    ),
    wartosc = c(
      nrow(df),
      nrow(train_df),
      nrow(test_df),
      length(predictors),
      paste(reg_targets, collapse = ", "),
      paste(class_targets, collapse = ", "),
      N_BINS,
      as.character(STRUCTURE_MAX_ROWS)
    )
  ))
  cat("\nPredyktory:\n")
  print(data.frame(predyktor = predictors))
})

save_csv(data.frame(predyktor = predictors), file.path(OUT_DIR, "lista_predyktorow.csv"))
save_csv(data.frame(target_regresyjny = reg_targets), file.path(OUT_DIR, "targety_regresyjne.csv"))
save_csv(data.frame(target_klasyfikacyjny = class_targets), file.path(OUT_DIR, "targety_klasyfikacyjne.csv"))

# ----------------------------
# 3. Dyskretyzacja danych dla DGM, NBC i TAN
# ----------------------------

make_breaks <- function(x, n_bins = N_BINS) {
  qs <- unique(as.numeric(quantile(x, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE, type = 7)))
  if (length(qs) < 3) {
    qs <- unique(pretty(x, n = n_bins))
  }
  qs[1] <- -Inf
  qs[length(qs)] <- Inf
  unique(qs)
}

discretize_train_test <- function(train, test, variables, class_targets, n_bins = N_BINS) {
  train_out <- data.frame(row.names = seq_len(nrow(train)))
  test_out <- data.frame(row.names = seq_len(nrow(test)))
  breaks_list <- list()

  for (v in variables) {
    if (!v %in% names(train)) next

    if (v %in% class_targets) {
      train_out[[v]] <- factor(train[[v]], levels = c("No", "Yes"))
      test_out[[v]]  <- factor(test[[v]], levels = c("No", "Yes"))
      next
    }

    if (is.numeric(train[[v]]) || is.integer(train[[v]])) {
      ux <- unique(train[[v]][!is.na(train[[v]])])
      if (length(ux) <= 5) {
        lv <- sort(unique(c(as.character(train[[v]]), as.character(test[[v]]))))
        train_out[[v]] <- factor(as.character(train[[v]]), levels = lv)
        test_out[[v]]  <- factor(as.character(test[[v]]), levels = lv)
      } else {
        br <- make_breaks(train[[v]], n_bins)
        lab <- BIN_LABELS[seq_len(length(br) - 1)]
        train_out[[v]] <- cut(train[[v]], breaks = br, include.lowest = TRUE, labels = lab, ordered_result = FALSE)
        test_out[[v]]  <- cut(test[[v]], breaks = br, include.lowest = TRUE, labels = lab, ordered_result = FALSE)
        breaks_list[[v]] <- br
      }
    } else {
      lv <- sort(unique(c(as.character(train[[v]]), as.character(test[[v]]))))
      train_out[[v]] <- factor(as.character(train[[v]]), levels = lv)
      test_out[[v]]  <- factor(as.character(test[[v]]), levels = lv)
    }
  }

  # Usuniecie kolumn stalych, bo hc() wymaga zmiennosci.
  keep <- vapply(train_out, function(x) length(unique(x[!is.na(x)])) >= 2, logical(1))
  train_out <- train_out[, keep, drop = FALSE]
  test_out <- test_out[, names(train_out), drop = FALSE]

  list(train = train_out, test = test_out, breaks = breaks_list)
}

disc_vars <- unique(c(predictors, class_targets))
disc <- discretize_train_test(train_df, test_df, disc_vars, class_targets)
train_disc <- disc$train
test_disc <- disc$test

save_csv(data.frame(
  zmienna = names(disc$breaks),
  punkty_podzialu = vapply(disc$breaks, function(x) paste(round(x, 4), collapse = "; "), character(1))
), file.path(OUT_DIR, "dyskretyzacja_przedzialy.csv"))

chapter_block("Dyskretyzacja zmiennych do modeli dyskretnych", {
  cat("Zmienne w danych dyskretnych:\n")
  print(data.frame(zmienna = names(train_disc), liczba_poziomow = vapply(train_disc, nlevels, integer(1))))
})

# ----------------------------
# 4. Skierowane modele graficzne - dyskretna siec bayesowska
# ----------------------------

log_section("DYSKRETNE MODELE GRAFICZNE - DGM")

for (target in class_targets) {
  if (!target %in% names(train_disc)) next

  log_section(paste("DGM dyskretny dla targetu:", target))

  dgm_vars <- unique(c(setdiff(names(train_disc), setdiff(class_targets, target))))
  dgm_train <- train_disc[, dgm_vars, drop = FALSE]
  dgm_test <- test_disc[, dgm_vars, drop = FALSE]

  # Zakazujemy lukow od przyszlego targetu do aktualnych predyktorow.
  blacklist_dgm <- data.frame(
    from = rep(target, length(setdiff(dgm_vars, target))),
    to = setdiff(dgm_vars, target),
    stringsAsFactors = FALSE
  )

  dgm_struct_data <- sample_structure_data(dgm_train)
  dgm_dag <- bnlearn::hc(dgm_struct_data, score = "bic", blacklist = blacklist_dgm)
  dgm_bn <- bnlearn::bn.fit(dgm_dag, data = dgm_train, method = "mle")

  save_rds(dgm_dag, file.path(MODEL_DIR, paste0("dgm_dag_", target, ".rds")))
  save_rds(dgm_bn, file.path(MODEL_DIR, paste0("dgm_fit_", target, ".rds")))

  arcs_path <- file.path(OUT_DIR, paste0("arcs_dgm_", target, ".csv"))
  save_csv(as.data.frame(bnlearn::arcs(dgm_dag)), arcs_path)

  graph_path <- file.path(PLOT_DIR, paste0("dag_dgm_", target, ".png"))
  save_plot_png(graph_path, {
    plot(dgm_dag)
    title(paste("DGM -", target))
  })

  # CPT / rozklady warunkowe dla targetu.
  cpt_path <- file.path(OUT_DIR, paste0("cpt_dgm_", target, ".txt"))
  writeLines(capture.output(print(dgm_bn[[target]])), cpt_path)
  saved_file(cpt_path)

  # Wykres rozkladu warunkowego targetu.
  dot_path <- file.path(PLOT_DIR, paste0("dotplot_dgm_", target, ".png"))
  save_plot_png(dot_path, {
    print(bnlearn::bn.fit.dotplot(dgm_bn[[target]]))
  })

  bar_path <- file.path(PLOT_DIR, paste0("barchart_dgm_", target, ".png"))
  save_plot_png(bar_path, {
    print(bnlearn::bn.fit.barchart(dgm_bn[[target]]))
  })

  # Wnioskowanie przyblizone cpquery.
  evidence_var <- if (grepl("gust", target)) "wind_gusts_10m" else "wind_speed_10m"
  query_results <- data.frame()

  if (evidence_var %in% names(dgm_train) && "Yes" %in% levels(dgm_train[[target]])) {
    high_level <- tail(levels(dgm_train[[evidence_var]]), 1)
    query_txt <- sprintf(
      'bnlearn::cpquery(dgm_bn, event = (%s == "Yes"), evidence = (%s == "%s"), n = %d)',
      target, evidence_var, high_level, CPQUERY_N
    )

    p_event <- tryCatch(eval(parse(text = query_txt)), error = function(e) NA_real_)

    query_results <- rbind(query_results, data.frame(
      target = target,
      pytanie = paste0("P(", target, " = Yes | ", evidence_var, " = ", high_level, ")"),
      wynik = p_event
    ))

    log_line("cpquery: ", query_results$pytanie[nrow(query_results)], " = ", round(p_event, 5))
  }

  # Drugie wnioskowanie: zdarzenie przy dniu, jesli is_day istnieje.
  if ("is_day" %in% names(dgm_train) && "Yes" %in% levels(dgm_train[[target]])) {
    day_level <- tail(levels(dgm_train$is_day), 1)
    query_txt2 <- sprintf(
      'bnlearn::cpquery(dgm_bn, event = (%s == "Yes"), evidence = (is_day == "%s"), n = %d)',
      target, day_level, CPQUERY_N
    )
    p_event_day <- tryCatch(eval(parse(text = query_txt2)), error = function(e) NA_real_)

    query_results <- rbind(query_results, data.frame(
      target = target,
      pytanie = paste0("P(", target, " = Yes | is_day = ", day_level, ")"),
      wynik = p_event_day
    ))

    log_line("cpquery: ", query_results$pytanie[nrow(query_results)], " = ", round(p_event_day, 5))
  }

  if (nrow(query_results) > 0) {
    save_csv(query_results, file.path(OUT_DIR, paste0("cpquery_dgm_", target, ".csv")))
  }

  # Probkowanie z sieci.
  sample_path <- file.path(OUT_DIR, paste0("rbn_dgm_", target, ".csv"))
  sampled <- tryCatch(bnlearn::rbn(dgm_bn, n = 10), error = function(e) data.frame(error = conditionMessage(e)))
  save_csv(sampled, sample_path)

  chapter_block(paste("DGM dyskretny dla targetu", target), {
    cat("Liczba lukow w sieci:", nrow(bnlearn::arcs(dgm_dag)), "\n")
    cat("Luki sieci:\n")
    print(as.data.frame(bnlearn::arcs(dgm_dag)))
    cat("\nCPT targetu:\n")
    print(dgm_bn[[target]])
    if (exists("query_results") && nrow(query_results) > 0) {
      cat("\nWyniki cpquery:\n")
      print(query_results)
    }
  })
}

# ----------------------------
# 5. NBC i TAN dla klasyfikacji zdarzen
# ----------------------------

log_section("NBC I TAN - KLASYFIKACJA ZDARZEN WIATROWYCH")

all_cls_metrics <- list()

for (target in class_targets) {
  if (!target %in% names(train_disc)) next

  log_section(paste("NBC/TAN dla targetu:", target))

  cls_vars <- unique(c(setdiff(names(train_disc), setdiff(class_targets, target))))
  train_cls <- train_disc[, cls_vars, drop = FALSE]
  test_cls <- test_disc[, cls_vars, drop = FALSE]

  # NBC
  nbc <- bnlearn::naive.bayes(train_cls, training = target)
  save_rds(nbc, file.path(MODEL_DIR, paste0("nbc_", target, ".rds")))

  save_plot_png(file.path(PLOT_DIR, paste0("struktura_nbc_", target, ".png")), {
    plot(nbc)
    title(paste("NBC -", target))
  })

  pred_nbc_train <- predict(nbc, data = train_cls)
  pred_nbc_test  <- predict(nbc, data = test_cls)

  tab_nbc_train <- table(predicted = pred_nbc_train, actual = train_cls[[target]])
  tab_nbc_test  <- table(predicted = pred_nbc_test, actual = test_cls[[target]])

  save_csv(as.data.frame.matrix(tab_nbc_train), file.path(OUT_DIR, paste0("confusion_train_nbc_", target, ".csv")))
  save_csv(as.data.frame.matrix(tab_nbc_test), file.path(OUT_DIR, paste0("confusion_test_nbc_", target, ".csv")))

  plot_confusion_heatmap(tab_nbc_test, paste("NBC - tablica pomylek test -", target),
                         file.path(PLOT_DIR, paste0("confusion_test_nbc_", target, ".png")))

  met_nbc_train <- classification_metrics(train_cls[[target]], pred_nbc_train, target, "NBC_train")
  met_nbc_test  <- classification_metrics(test_cls[[target]], pred_nbc_test, target, "NBC_test")

  # TAN
  tan <- bnlearn::tree.bayes(train_cls, training = target)
  save_rds(tan, file.path(MODEL_DIR, paste0("tan_", target, ".rds")))

  save_plot_png(file.path(PLOT_DIR, paste0("struktura_tan_", target, ".png")), {
    plot(tan)
    title(paste("TAN -", target))
  })

  pred_tan_train <- predict(tan, data = train_cls)
  pred_tan_test  <- predict(tan, data = test_cls)

  tab_tan_train <- table(predicted = pred_tan_train, actual = train_cls[[target]])
  tab_tan_test  <- table(predicted = pred_tan_test, actual = test_cls[[target]])

  save_csv(as.data.frame.matrix(tab_tan_train), file.path(OUT_DIR, paste0("confusion_train_tan_", target, ".csv")))
  save_csv(as.data.frame.matrix(tab_tan_test), file.path(OUT_DIR, paste0("confusion_test_tan_", target, ".csv")))

  plot_confusion_heatmap(tab_tan_test, paste("TAN - tablica pomylek test -", target),
                         file.path(PLOT_DIR, paste0("confusion_test_tan_", target, ".png")))

  met_tan_train <- classification_metrics(train_cls[[target]], pred_tan_train, target, "TAN_train")
  met_tan_test  <- classification_metrics(test_cls[[target]], pred_tan_test, target, "TAN_test")

  # Predykcja dla pojedynczej obserwacji testowej - analogia do przykladu passenger.
  one_obs <- test_cls[1, , drop = FALSE]
  single_pred <- data.frame(
    target = target,
    model = c("NBC", "TAN"),
    predykcja_pierwszej_obserwacji_testowej = c(
      as.character(predict(nbc, data = one_obs)),
      as.character(predict(tan, data = one_obs))
    ),
    rzeczywista_klasa = as.character(one_obs[[target]])
  )
  save_csv(single_pred, file.path(OUT_DIR, paste0("single_prediction_nbc_tan_", target, ".csv")))

  metrics_target <- rbind(met_nbc_train, met_nbc_test, met_tan_train, met_tan_test)
  save_csv(metrics_target, file.path(OUT_DIR, paste0("metryki_nbc_tan_", target, ".csv")))
  all_cls_metrics[[target]] <- metrics_target

  chapter_block(paste("NBC i TAN dla targetu", target), {
    cat("NBC - treningowa tabela pomylek:\n")
    print(tab_nbc_train)
    cat("\nNBC - testowa tabela pomylek:\n")
    print(tab_nbc_test)
    cat("\nTAN - treningowa tabela pomylek:\n")
    print(tab_tan_train)
    cat("\nTAN - testowa tabela pomylek:\n")
    print(tab_tan_test)
    cat("\nMetryki:\n")
    print(metrics_target)
    cat("\nPredykcja pierwszej obserwacji testowej:\n")
    print(single_pred)
  })
}

if (length(all_cls_metrics) > 0) {
  all_cls_df <- do.call(rbind, all_cls_metrics)
  save_csv(all_cls_df, file.path(OUT_DIR, "metryki_nbc_tan_wszystkie.csv"))
}

# ----------------------------
# 6. Gaussowskie sieci bayesowskie - GBN
# ----------------------------

log_section("GAUSSOWSKIE SIECI BAYESOWSKIE - GBN")

# Tylko zmienne numeryczne o wiecej niz kilku unikalnych wartosciach.
is_continuous <- function(x) is.numeric(x) && length(unique(x[!is.na(x)])) > 10

continuous_predictors <- predictors[predictors %in% names(train_df)]
continuous_predictors <- continuous_predictors[vapply(train_df[, continuous_predictors, drop = FALSE], is_continuous, logical(1))]

gbn_vars <- unique(c(continuous_predictors, reg_targets))
gbn_vars <- gbn_vars[gbn_vars %in% names(train_df)]
train_gbn <- train_df[, gbn_vars, drop = FALSE]
test_gbn  <- test_df[, gbn_vars, drop = FALSE]

for (v in names(train_gbn)) {
  train_gbn[[v]] <- as.numeric(train_gbn[[v]])
  test_gbn[[v]] <- as.numeric(test_gbn[[v]])
}

# Zakazujemy lukow od targetow przyszlych do predyktorow.
blacklist_gbn <- do.call(rbind, lapply(reg_targets[reg_targets %in% gbn_vars], function(tg) {
  data.frame(from = tg, to = setdiff(gbn_vars, tg), stringsAsFactors = FALSE)
}))

gbn_struct_data <- sample_structure_data(train_gbn)
gbn_dag <- bnlearn::hc(gbn_struct_data, score = "bic-g", blacklist = blacklist_gbn)
gbn_fit <- bnlearn::bn.fit(gbn_dag, data = train_gbn, method = "mle-g")

save_rds(gbn_dag, file.path(MODEL_DIR, "gbn_dag_regresja.rds"))
save_rds(gbn_fit, file.path(MODEL_DIR, "gbn_fit_regresja.rds"))
save_csv(as.data.frame(bnlearn::arcs(gbn_dag)), file.path(OUT_DIR, "arcs_gbn_regresja.csv"))

save_plot_png(file.path(PLOT_DIR, "dag_gbn_regresja.png"), {
  plot(gbn_dag)
  title("GBN - regresja")
})

# d-separacja analogiczna do notatnika.
dsep_results <- data.frame()
if (all(c("wind_speed_100m", "wind_gusts_10m", "wind_speed_10m_next") %in% names(train_gbn))) {
  dsep_results <- rbind(dsep_results, data.frame(
    x = "wind_speed_100m",
    y = "wind_gusts_10m",
    given = "wind_speed_10m_next",
    d_separated = bnlearn::dsep(gbn_dag, "wind_speed_100m", "wind_gusts_10m", "wind_speed_10m_next")
  ))
}
if (all(c("temperature_2m", "pressure_msl", "wind_gusts_10m_next") %in% names(train_gbn))) {
  dsep_results <- rbind(dsep_results, data.frame(
    x = "temperature_2m",
    y = "pressure_msl",
    given = "wind_gusts_10m_next",
    d_separated = bnlearn::dsep(gbn_dag, "temperature_2m", "pressure_msl", "wind_gusts_10m_next")
  ))
}
if (nrow(dsep_results) > 0) {
  save_csv(dsep_results, file.path(OUT_DIR, "dsep_gbn.csv"))
}

# Diagnostyka GBN.
diagnostic_nodes <- unique(c(reg_targets[1], continuous_predictors[1], continuous_predictors[min(2, length(continuous_predictors))]))
diagnostic_nodes <- diagnostic_nodes[diagnostic_nodes %in% names(gbn_fit)]

for (node in diagnostic_nodes) {
  save_plot_png(file.path(PLOT_DIR, paste0("gbn_xyplot_", node, ".png")), {
    print(bnlearn::bn.fit.xyplot(gbn_fit[[node]]))
  })
  save_plot_png(file.path(PLOT_DIR, paste0("gbn_qqplot_", node, ".png")), {
    print(bnlearn::bn.fit.qqplot(gbn_fit[[node]]))
  })
  save_plot_png(file.path(PLOT_DIR, paste0("gbn_histogram_", node, ".png")), {
    print(bnlearn::bn.fit.histogram(gbn_fit[[node]]))
  })
}

all_reg_metrics <- list()

for (target in reg_targets) {
  if (!target %in% names(train_gbn)) next

  log_section(paste("GBN regresja dla targetu:", target))

  pred_gbn_train <- tryCatch(
    as.numeric(safe_predict_bn(gbn_fit, node = target, data = train_gbn, method = "exact")),
    error = function(e) {
      log_line("[BLAD PREDYKCJI GBN TRAIN] ", target, " | ", conditionMessage(e))
      rep(NA_real_, nrow(train_gbn))
    }
  )

  pred_gbn_test <- tryCatch(
    as.numeric(safe_predict_bn(gbn_fit, node = target, data = test_gbn, method = "exact")),
    error = function(e) {
      log_line("[BLAD PREDYKCJI GBN TEST] ", target, " | ", conditionMessage(e))
      rep(NA_real_, nrow(test_gbn))
    }
  )

  met_gbn <- reg_metrics(train_gbn[[target]], pred_gbn_train, test_gbn[[target]], pred_gbn_test, target, "GBN")

  # Porownanie z modelem liniowym OLS.
  lm_res <- fit_lm_baseline(train_gbn, test_gbn, target, setdiff(names(train_gbn), reg_targets))
  met_lm <- lm_res$metrics

  metrics_reg_target <- rbind(met_gbn, met_lm)
  save_csv(metrics_reg_target, file.path(OUT_DIR, paste0("metryki_regresja_gbn_lm_", target, ".csv")))

  preds_path <- file.path(OUT_DIR, paste0("predykcje_regresja_gbn_lm_", target, ".csv"))
  save_csv(data.frame(
    target = target,
    zbior = "test",
    rzeczywiste = test_gbn[[target]],
    pred_gbn = pred_gbn_test,
    pred_lm = lm_res$pred_test
  ), preds_path)

  pred_plot <- ggplot(data.frame(
    indeks = seq_along(test_gbn[[target]]),
    rzeczywiste = test_gbn[[target]],
    GBN = pred_gbn_test,
    LM = lm_res$pred_test
  )[seq_len(min(500, nrow(test_gbn))), ], aes(x = indeks)) +
    geom_line(aes(y = rzeczywiste, linetype = "rzeczywiste")) +
    geom_line(aes(y = GBN, linetype = "GBN")) +
    geom_line(aes(y = LM, linetype = "LM")) +
    labs(title = paste("Predykcje GBN i LM -", target), x = "Indeks obserwacji testowej", y = target, linetype = "") +
    theme_minimal()

  ggsave(file.path(PLOT_DIR, paste0("predykcje_gbn_lm_", target, ".png")), pred_plot, width = 9, height = 5, dpi = 150)
  saved_file(file.path(PLOT_DIR, paste0("predykcje_gbn_lm_", target, ".png")))

  all_reg_metrics[[paste0("GBN_", target)]] <- metrics_reg_target

  chapter_block(paste("GBN i model liniowy dla targetu", target), {
    cat("Metryki regresji:\n")
    print(metrics_reg_target)
    cat("\nPredykcja 20. obserwacji testowej, jesli istnieje:\n")
    if (nrow(test_gbn) >= 20) {
      print(data.frame(
        target = target,
        rzeczywiste = test_gbn[[target]][20],
        pred_gbn = pred_gbn_test[20],
        pred_lm = lm_res$pred_test[20]
      ))
    }
  })
}

# ----------------------------
# 7. Modele hybrydowe
# ----------------------------

log_section("MODELE HYBRYDOWE")

# Zmienne ciagle + kilka dyskretnych/porzadkowych czynnikow.
hybrid_cont <- continuous_predictors
hybrid_disc <- intersect(c("is_day", "polrocze_cieple"), names(train_df))

# Dodatkowa dyskretna zmienna: sektor kierunku wiatru na podstawie sin/cos.
add_wind_sector <- function(d) {
  if (all(c("wind_dir_sin", "wind_dir_cos") %in% names(d))) {
    ang <- atan2(d$wind_dir_sin, d$wind_dir_cos)
    deg <- (ang * 180 / pi + 360) %% 360
    d$wind_dir_sector <- cut(
      deg,
      breaks = c(-Inf, 45, 135, 225, 315, Inf),
      labels = c("N", "E", "S", "W", "N2"),
      include.lowest = TRUE
    )
    d$wind_dir_sector <- as.character(d$wind_dir_sector)
    d$wind_dir_sector[d$wind_dir_sector == "N2"] <- "N"
    d$wind_dir_sector <- factor(d$wind_dir_sector, levels = c("N", "E", "S", "W"))
  }
  d
}

train_hybrid_base <- add_wind_sector(train_df)
test_hybrid_base  <- add_wind_sector(test_df)
if ("wind_dir_sector" %in% names(train_hybrid_base)) {
  hybrid_disc <- unique(c(hybrid_disc, "wind_dir_sector"))
}

hybrid_vars <- unique(c(hybrid_cont, hybrid_disc, reg_targets))
hybrid_vars <- hybrid_vars[hybrid_vars %in% names(train_hybrid_base)]

train_hybrid <- train_hybrid_base[, hybrid_vars, drop = FALSE]
test_hybrid  <- test_hybrid_base[, hybrid_vars, drop = FALSE]

for (v in hybrid_cont) {
  if (v %in% names(train_hybrid)) {
    train_hybrid[[v]] <- as.numeric(train_hybrid[[v]])
    test_hybrid[[v]]  <- as.numeric(test_hybrid[[v]])
  }
}
for (v in reg_targets) {
  if (v %in% names(train_hybrid)) {
    train_hybrid[[v]] <- as.numeric(train_hybrid[[v]])
    test_hybrid[[v]]  <- as.numeric(test_hybrid[[v]])
  }
}
for (v in hybrid_disc) {
  if (v %in% names(train_hybrid)) {
    lv <- sort(unique(c(as.character(train_hybrid[[v]]), as.character(test_hybrid[[v]]))))
    train_hybrid[[v]] <- factor(as.character(train_hybrid[[v]]), levels = lv)
    test_hybrid[[v]]  <- factor(as.character(test_hybrid[[v]]), levels = lv)
  }
}

# W modelu hybrydowym wezel dyskretny nie moze miec rodzicow ciaglych.
continuous_nodes <- names(train_hybrid)[vapply(train_hybrid, is.numeric, logical(1))]
discrete_nodes <- names(train_hybrid)[vapply(train_hybrid, is.factor, logical(1))]

blacklist_cg_1 <- if (length(continuous_nodes) > 0 && length(discrete_nodes) > 0) {
  expand.grid(from = continuous_nodes, to = discrete_nodes, stringsAsFactors = FALSE)
} else {
  data.frame(from = character(), to = character())
}

blacklist_cg_2 <- do.call(rbind, lapply(reg_targets[reg_targets %in% names(train_hybrid)], function(tg) {
  data.frame(from = tg, to = setdiff(names(train_hybrid), tg), stringsAsFactors = FALSE)
}))

blacklist_hybrid <- unique(rbind(blacklist_cg_1, blacklist_cg_2))

hybrid_struct_data <- sample_structure_data(train_hybrid)
hybrid_dag <- bnlearn::hc(hybrid_struct_data, score = "bic-cg", blacklist = blacklist_hybrid)
hybrid_fit <- bnlearn::bn.fit(hybrid_dag, data = train_hybrid, method = "mle-cg")

save_rds(hybrid_dag, file.path(MODEL_DIR, "hybrid_dag_regresja.rds"))
save_rds(hybrid_fit, file.path(MODEL_DIR, "hybrid_fit_regresja.rds"))
save_csv(as.data.frame(bnlearn::arcs(hybrid_dag)), file.path(OUT_DIR, "arcs_hybrid_regresja.csv"))

save_plot_png(file.path(PLOT_DIR, "dag_hybrid_regresja.png"), {
  plot(hybrid_dag)
  title("Model hybrydowy - regresja")
})

# Wykresy dla wezlow hybrydowych.
if (length(reg_targets[reg_targets %in% names(hybrid_fit)]) > 0) {
  node <- reg_targets[reg_targets %in% names(hybrid_fit)][1]
  save_plot_png(file.path(PLOT_DIR, paste0("hybrid_histogram_", node, ".png")), {
    print(bnlearn::bn.fit.histogram(hybrid_fit[[node]]))
  })
}
if (length(discrete_nodes) > 0) {
  node <- discrete_nodes[1]
  save_plot_png(file.path(PLOT_DIR, paste0("hybrid_dotplot_", node, ".png")), {
    print(bnlearn::bn.fit.dotplot(hybrid_fit[[node]]))
  })
}

for (target in reg_targets) {
  if (!target %in% names(train_hybrid)) next

  log_section(paste("Model hybrydowy dla targetu:", target))

  # Predykcja bayes-lw jest przyblizona i moze byc wolniejsza.
  pred_hybrid_train <- tryCatch(
    as.numeric(safe_predict_bn(hybrid_fit, node = target, data = train_hybrid, method = "bayes-lw", n = BAYES_LW_N)),
    error = function(e) {
      log_line("[BLAD PREDYKCJI HYBRID TRAIN] ", target, " | ", conditionMessage(e))
      rep(NA_real_, nrow(train_hybrid))
    }
  )

  pred_hybrid_test <- tryCatch(
    as.numeric(safe_predict_bn(hybrid_fit, node = target, data = test_hybrid, method = "bayes-lw", n = BAYES_LW_N)),
    error = function(e) {
      log_line("[BLAD PREDYKCJI HYBRID TEST] ", target, " | ", conditionMessage(e))
      rep(NA_real_, nrow(test_hybrid))
    }
  )

  met_hybrid <- reg_metrics(train_hybrid[[target]], pred_hybrid_train, test_hybrid[[target]], pred_hybrid_test, target, "Hybrid_BN")

  # Porownanie z LM na czesci numerycznej oraz zakodowanych czynnikach.
  lm_predictors <- setdiff(names(train_hybrid), reg_targets)
  formula_lm <- as.formula(paste(target, "~", paste(lm_predictors, collapse = " + ")))
  lm_h <- lm(formula_lm, data = train_hybrid)
  pred_lm_h_train <- predict(lm_h, newdata = train_hybrid)
  pred_lm_h_test <- predict(lm_h, newdata = test_hybrid)
  met_lm_h <- reg_metrics(train_hybrid[[target]], pred_lm_h_train, test_hybrid[[target]], pred_lm_h_test, target, "LM_hybrid_vars")

  metrics_hybrid_target <- rbind(met_hybrid, met_lm_h)
  save_csv(metrics_hybrid_target, file.path(OUT_DIR, paste0("metryki_regresja_hybrid_lm_", target, ".csv")))

  save_csv(data.frame(
    target = target,
    zbior = "test",
    rzeczywiste = test_hybrid[[target]],
    pred_hybrid = pred_hybrid_test,
    pred_lm = pred_lm_h_test
  ), file.path(OUT_DIR, paste0("predykcje_regresja_hybrid_lm_", target, ".csv")))

  pred_plot <- ggplot(data.frame(
    indeks = seq_along(test_hybrid[[target]]),
    rzeczywiste = test_hybrid[[target]],
    Hybrid_BN = pred_hybrid_test,
    LM = pred_lm_h_test
  )[seq_len(min(500, nrow(test_hybrid))), ], aes(x = indeks)) +
    geom_line(aes(y = rzeczywiste, linetype = "rzeczywiste")) +
    geom_line(aes(y = Hybrid_BN, linetype = "Hybrid_BN")) +
    geom_line(aes(y = LM, linetype = "LM")) +
    labs(title = paste("Predykcje modelu hybrydowego i LM -", target),
         x = "Indeks obserwacji testowej", y = target, linetype = "") +
    theme_minimal()

  ggsave(file.path(PLOT_DIR, paste0("predykcje_hybrid_lm_", target, ".png")), pred_plot, width = 9, height = 5, dpi = 150)
  saved_file(file.path(PLOT_DIR, paste0("predykcje_hybrid_lm_", target, ".png")))

  all_reg_metrics[[paste0("Hybrid_", target)]] <- metrics_hybrid_target

  chapter_block(paste("Model hybrydowy i LM dla targetu", target), {
    cat("Metryki regresji:\n")
    print(metrics_hybrid_target)
  })
}

if (length(all_reg_metrics) > 0) {
  all_reg_df <- do.call(rbind, all_reg_metrics)
  save_csv(all_reg_df, file.path(OUT_DIR, "metryki_regresja_wszystkie_modele_graficzne.csv"))
}

# ----------------------------
# 8. Zbiorcze podsumowanie
# ----------------------------

log_section("PODSUMOWANIE")

summary_files <- data.frame(
  typ = c(
    "metryki klasyfikacji NBC/TAN",
    "metryki regresji GBN/hybrid/LM",
    "luki GBN",
    "luki hybrid",
    "wykres GBN",
    "wykres hybrid"
  ),
  plik = c(
    file.path(OUT_DIR, "metryki_nbc_tan_wszystkie.csv"),
    file.path(OUT_DIR, "metryki_regresja_wszystkie_modele_graficzne.csv"),
    file.path(OUT_DIR, "arcs_gbn_regresja.csv"),
    file.path(OUT_DIR, "arcs_hybrid_regresja.csv"),
    file.path(PLOT_DIR, "dag_gbn_regresja.png"),
    file.path(PLOT_DIR, "dag_hybrid_regresja.png")
  )
)

save_csv(summary_files, file.path(OUT_DIR, "najwazniejsze_pliki_do_rozdzialu.csv"))

chapter_block("Zbiorcze pliki do rozdzialu Modele graficzne", {
  print(summary_files)
  cat("\nModele graficzne wykonane analogicznie do notatnika:\n")
  cat("- dyskretne DGM: hc(), plot(), bn.fit(), CPT, cpquery(), rbn();\n")
  cat("- NBC i TAN: naive.bayes(), tree.bayes(), predykcja, tablice pomylek, blad klasyfikacji;\n")
  cat("- GBN: hc() dla danych ciaglych, dsep(), bn.fit(method = 'mle-g'), diagnostyka, predykcja, MSE/RMSE/MAE/R2;\n")
  cat("- modele hybrydowe: hc() z score bic-cg, bn.fit(method = 'mle-cg'), wykresy wezlow, predykcja bayes-lw;\n")
  cat("- porownanie regresji z modelem liniowym OLS.\n")
})

log_line("Koniec: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
log_line("Gotowe.")
