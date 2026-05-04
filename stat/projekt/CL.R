# ============================================================
# KLASYFIKACJA WARUNKÓW WIATROWYCH DLA KITESURFINGU
# ============================================================

# Wykorzystywane pakiety
library(dplyr)
library(MASS)
library(class)
library(lubridate)
# dane <- read.csv("/Users/damia/OneDrive/Dokumenty/Rlab/stat/projekt/data/chalupy_hourly_10000_days.csv")
# ------------------------------------------------------------
# 1. Przygotowanie danych
# ------------------------------------------------------------

# Upewniamy się, że kolumna datetime ma typ daty i czasu
dane$datetime <- as.POSIXct(dane$datetime, format = "%Y-%m-%dT%H:%M")
dane$dzien <- yday(dane$datetime)
dane$godzina <- hour(dane$datetime)
# Przyjęte progi klasyfikacji.
# UWAGA:
# Jeżeli dane dotyczące prędkości wiatru są w km/h, poniższe progi można zostawić.
# Jeżeli dane są w m/s, progi należy zmienić.
min_wiatr <- 12
max_wiatr <- 40
max_poryw <- 55

# Tworzymy zmienne przesunięte o jedną godzinę.
# Dzięki temu predyktory z chwili t służą do przewidywania warunków w chwili t+1.
dane$wind_speed_10m_next <- dplyr::lead(dane$wind_speed_10m, 1)
dane$wind_gusts_10m_next <- dplyr::lead(dane$wind_gusts_10m, 1)

# Tworzymy zmienne opisujące sezonowość roczną.
# Dzień roku jest cykliczny, dlatego używamy funkcji sin i cos.
dane$sin_dzien <- sin(2 * pi * dane$dzien / 365)
dane$cos_dzien <- cos(2 * pi * dane$dzien / 365)

# Tworzymy zmienne opisujące sezonowość dobową.
# Godzina również ma charakter cykliczny: po 23 następuje 0.
dane$sin_godzina <- sin(2 * pi * dane$godzina / 24)
dane$cos_godzina <- cos(2 * pi * dane$godzina / 24)

# Zmienne przesunięte o jedną godzinę
dane$wind_speed_10m_next <- dplyr::lead(dane$wind_speed_10m, 1)
dane$wind_gusts_10m_next <- dplyr::lead(dane$wind_gusts_10m, 1)
dane$is_day_next <- dplyr::lead(dane$is_day, 1)

# Zmienna klasyfikowana: warunki dobre tylko wtedy,
# gdy za godzinę wiatr jest odpowiedni, porywy nie są zbyt silne
# i jednocześnie jest dzień
dane$warunki <- ifelse(
  dane$wind_speed_10m_next >= min_wiatr &
    dane$wind_speed_10m_next <= max_wiatr &
    dane$wind_gusts_10m_next <= max_poryw &
    dane$is_day_next == 1,
  "Dobre",
  "Zle"
)
# Ustawiamy kolejność poziomów faktora.
# W regresji logistycznej R domyślnie modeluje prawdopodobieństwo drugiego poziomu,
# czyli w tym przypadku klasy "Dobre".
dane$warunki <- factor(dane$warunki, levels = c("Zle", "Dobre"))

# Usuwamy obserwacje z brakującymi wartościami.
# Ostatni wiersz ma NA w zmiennych *_next, bo nie istnieje kolejna godzina.
dane_klasyfikacja <- na.omit(dane)

# Sprawdzamy liczebność klas
table(dane_klasyfikacja$warunki)

# Sprawdzamy proporcje klas
prop.table(table(dane_klasyfikacja$warunki))


# ------------------------------------------------------------
# 2. Podział danych na zbiór treningowy i testowy
# ------------------------------------------------------------

# Podział wykonujemy chronologicznie.
# Dane sprzed 2025 roku wykorzystujemy do uczenia,
# dane od 2025 roku do testowania.
train <- dane_klasyfikacja$datetime < as.POSIXct("2025-01-01 00:00:00")

dane_train <- dane_klasyfikacja[train, ]
dane_test <- dane_klasyfikacja[!train, ]

# Wektor rzeczywistych klas w zbiorze testowym
warunki_test <- dane_test$warunki


# ------------------------------------------------------------
# 3. Regresja logistyczna — model pełny
# ------------------------------------------------------------

# Model pełny zawiera szeroki zestaw zmiennych meteorologicznych.
model_log_pelny <- glm(
  warunki ~ temperature_2m +
    relative_humidity_2m +
    dew_point_2m +
    apparent_temperature +
    precipitation +
    rain +
    snowfall +
    snow_depth +
    weather_code +
    pressure_msl +
    surface_pressure +
    cloud_cover +
    cloud_cover_low +
    cloud_cover_mid +
    cloud_cover_high +
    vapour_pressure_deficit +
    wind_speed_10m +
    wind_speed_100m +
    wind_direction_10m +
    wind_direction_100m +
    wind_gusts_10m +
    is_day +
    sunshine_duration +
    shortwave_radiation +
    direct_radiation +
    diffuse_radiation +
    sin_dzien +
    cos_dzien +
    sin_godzina +
    cos_godzina,
  family = binomial,
  data = dane_train
)

# Podsumowanie modelu pełnego
summary(model_log_pelny)

# Predykcja prawdopodobieństw klasy "Dobre" dla zbioru testowego
log_pelny_probs <- predict(model_log_pelny, dane_test, type = "response")

# Zamiana prawdopodobieństw na klasy przy progu 0.5
log_pelny_pred <- ifelse(log_pelny_probs > 0.5, "Dobre", "Zle")
log_pelny_pred <- factor(log_pelny_pred, levels = c("Zle", "Dobre"))

# Tablica pomyłek
log_pelny_cm <- table(log_pelny_pred, warunki_test)
log_pelny_cm

# Testowa proporcja błędów liczona bezpośrednio
log_pelny_error <- mean(log_pelny_pred != warunki_test)
log_pelny_error

# Testowa proporcja błędów liczona na podstawie tablicy pomyłek
sum(log_pelny_cm[row(log_pelny_cm) != col(log_pelny_cm)]) / sum(log_pelny_cm)


# ------------------------------------------------------------
# 4. Regresja logistyczna — model zredukowany
# ------------------------------------------------------------

# Model zredukowany zawiera zmienne najważniejsze praktycznie:
# wiatr, porywy, kierunek wiatru, podstawowe parametry pogodowe i sezonowość.
model_log_redukowany <- glm(
  warunki ~ wind_speed_10m +
    wind_speed_100m +
    wind_gusts_10m +
    wind_direction_10m +
    pressure_msl +
    temperature_2m +
    relative_humidity_2m +
    precipitation +
    cloud_cover +
    shortwave_radiation +
    sin_dzien +
    cos_dzien +
    sin_godzina +
    cos_godzina,
  family = binomial,
  data = dane_train
)

# Podsumowanie modelu zredukowanego
summary(model_log_redukowany)

# Predykcja prawdopodobieństw klasy "Dobre"
log_redukowany_probs <- predict(model_log_redukowany, dane_test, type = "response")

# Klasyfikacja przy progu 0.5
log_redukowany_pred <- ifelse(log_redukowany_probs > 0.5, "Dobre", "Zle")
log_redukowany_pred <- factor(log_redukowany_pred, levels = c("Zle", "Dobre"))

# Tablica pomyłek
log_redukowany_cm <- table(log_redukowany_pred, warunki_test)
log_redukowany_cm

# Testowa proporcja błędów
log_redukowany_error <- mean(log_redukowany_pred != warunki_test)
log_redukowany_error

# Proporcja błędów na podstawie tablicy pomyłek
sum(log_redukowany_cm[row(log_redukowany_cm) != col(log_redukowany_cm)]) /
  sum(log_redukowany_cm)


# ------------------------------------------------------------
# 5. Model zerowy
# ------------------------------------------------------------

# Model zerowy zawsze przewiduje klasę najczęściej występującą
# w zbiorze treningowym.
klasa_dominujaca <- names(which.max(table(dane_train$warunki)))

pred_zerowy <- rep(klasa_dominujaca, length(warunki_test))
pred_zerowy <- factor(pred_zerowy, levels = c("Zle", "Dobre"))

# Tablica pomyłek dla modelu zerowego
zerowy_cm <- table(pred_zerowy, warunki_test)
zerowy_cm

# Testowa proporcja błędów modelu zerowego
zerowy_error <- mean(pred_zerowy != warunki_test)
zerowy_error


# ------------------------------------------------------------
# 6. LDA — liniowa analiza dyskryminacyjna
# ------------------------------------------------------------

# Model LDA wykorzystuje ten sam zestaw zmiennych co zredukowana regresja logistyczna.
model_lda <- lda(
  warunki ~ wind_speed_10m +
    wind_speed_100m +
    wind_gusts_10m +
    wind_direction_10m +
    pressure_msl +
    temperature_2m +
    relative_humidity_2m +
    precipitation +
    cloud_cover +
    shortwave_radiation +
    sin_dzien +
    cos_dzien +
    sin_godzina +
    cos_godzina,
  data = dane_train
)

# Informacje o modelu LDA
model_lda

# Predykcja dla zbioru testowego
lda_predicted <- predict(model_lda, dane_test)

# Przewidywane klasy
head(lda_predicted$class)

# Prawdopodobieństwa a posteriori
head(lda_predicted$posterior)

# Wartości dyskryminatora
head(lda_predicted$x)

# Tablica pomyłek
lda_cm <- table(lda_predicted$class, warunki_test)
lda_cm

# Testowa proporcja błędów dla LDA
lda_error <- mean(lda_predicted$class != warunki_test)
lda_error

# Maksymalne przewidywane prawdopodobieństwo klasy "Dobre"
max(lda_predicted$posterior[, "Dobre"])

# Maksymalne przewidywane prawdopodobieństwo klasy "Zle"
max(lda_predicted$posterior[, "Zle"])


# ------------------------------------------------------------
# 7. QDA — kwadratowa analiza dyskryminacyjna
# ------------------------------------------------------------

# Model QDA wykorzystuje ten sam zestaw zmiennych co LDA.
model_qda <- qda(
  warunki ~ wind_speed_10m +
    wind_speed_100m +
    wind_gusts_10m +
    wind_direction_10m +
    pressure_msl +
    temperature_2m +
    relative_humidity_2m +
    precipitation +
    cloud_cover +
    shortwave_radiation +
    sin_dzien +
    cos_dzien +
    sin_godzina +
    cos_godzina,
  data = dane_train
)

# Informacje o modelu QDA
model_qda

# Predykcja dla zbioru testowego
qda_predicted <- predict(model_qda, dane_test)

# Tablica pomyłek
qda_cm <- table(qda_predicted$class, warunki_test)
qda_cm

# Testowa proporcja błędów dla QDA
qda_error <- mean(qda_predicted$class != warunki_test)
qda_error


# ------------------------------------------------------------
# 8. kNN — przygotowanie danych
# ------------------------------------------------------------

# Predyktory użyte w kNN.
# Wykorzystujemy taki sam zestaw jak w modelach zredukowanych.
predyktory_knn <- c(
  "wind_speed_10m",
  "wind_speed_100m",
  "wind_gusts_10m",
  "wind_direction_10m",
  "pressure_msl",
  "temperature_2m",
  "relative_humidity_2m",
  "precipitation",
  "cloud_cover",
  "shortwave_radiation",
  "sin_dzien",
  "cos_dzien",
  "sin_godzina",
  "cos_godzina"
)

# Tworzymy macierze predyktorów dla zbioru treningowego i testowego
train_set <- dane_train[, predyktory_knn]
test_set <- dane_test[, predyktory_knn]

# Wektor klas dla zbioru treningowego
warunki_train <- dane_train$warunki

# Skalowanie predyktorów.
# Parametry skalowania wyznaczamy tylko na zbiorze treningowym.
train_set_scaled <- scale(train_set)

# Zbiór testowy skalujemy tymi samymi średnimi i odchyleniami standardowymi,
# które zostały wyznaczone na zbiorze treningowym.
test_set_scaled <- scale(
  test_set,
  center = attr(train_set_scaled, "scaled:center"),
  scale = attr(train_set_scaled, "scaled:scale")
)


# ------------------------------------------------------------
# 9. kNN dla k = 1
# ------------------------------------------------------------

set.seed(2026)

knn_1 <- knn(
  train = train_set_scaled,
  test = test_set_scaled,
  cl = warunki_train,
  k = 1
)

# Tablica pomyłek dla k = 1
knn_1_cm <- table(knn_1, warunki_test)
knn_1_cm

# Testowa proporcja błędów dla k = 1
knn_1_error <- mean(knn_1 != warunki_test)
knn_1_error


# ------------------------------------------------------------
# 10. kNN dla k = 2, 3, 4, 5
# ------------------------------------------------------------

set.seed(2026)

knn_2 <- knn(train_set_scaled, test_set_scaled, warunki_train, k = 2)
knn_3 <- knn(train_set_scaled, test_set_scaled, warunki_train, k = 3)
knn_4 <- knn(train_set_scaled, test_set_scaled, warunki_train, k = 4)
knn_5 <- knn(train_set_scaled, test_set_scaled, warunki_train, k = 5)

# Testowe proporcje błędów dla różnych wartości k
bledy_knn <- c(
  k1 = mean(knn_1 != warunki_test),
  k2 = mean(knn_2 != warunki_test),
  k3 = mean(knn_3 != warunki_test),
  k4 = mean(knn_4 != warunki_test),
  k5 = mean(knn_5 != warunki_test)
)

bledy_knn

# Najlepszy wariant kNN
bledy_knn[which.min(bledy_knn)]


# ------------------------------------------------------------
# 11. Prawdopodobieństwo klasy "Zle" dla 10. obserwacji testowej
# ------------------------------------------------------------

i <- 10

# Regresja logistyczna, model zredukowany.
# predict() zwraca prawdopodobieństwo klasy "Dobre",
# dlatego prawdopodobieństwo klasy "Zle" liczymy jako 1 - P(Dobre).
logistic_prob_zle <- 1 - log_redukowany_probs[i]

# LDA — prawdopodobieństwo a posteriori klasy "Zle"
lda_prob_zle <- lda_predicted$posterior[i, "Zle"]

# QDA — prawdopodobieństwo a posteriori klasy "Zle"
qda_prob_zle <- qda_predicted$posterior[i, "Zle"]

# kNN dla k = 3 z włączonym zwracaniem prawdopodobieństwa zwycięskiej klasy
set.seed(2026)

knn_3_prob <- knn(
  train = train_set_scaled,
  test = test_set_scaled,
  cl = warunki_train,
  k = 3,
  prob = TRUE
)

# Atrybut "prob" zwraca prawdopodobieństwo klasy, która wygrała głosowanie.
knn_prob_winner <- attr(knn_3_prob, "prob")[i]

# Jeśli zwycięską klasą była "Zle", bierzemy to prawdopodobieństwo.
# Jeśli zwycięską klasą było "Dobre", prawdopodobieństwo "Zle" to 1 - prob.
knn_prob_zle <- ifelse(
  knn_3_prob[i] == "Zle",
  knn_prob_winner,
  1 - knn_prob_winner
)

# Zestawienie prawdopodobieństw klasy "Zle"
prawdopodobienstwa_zle <- c(
  regresja_logistyczna = logistic_prob_zle,
  LDA = lda_prob_zle,
  QDA = qda_prob_zle,
  kNN_k3 = knn_prob_zle
)

prawdopodobienstwa_zle


# ------------------------------------------------------------
# 12. Porównanie wszystkich modeli
# ------------------------------------------------------------

wyniki_klasyfikacji <- data.frame(
  Model = c(
    "Regresja logistyczna - pełna",
    "Regresja logistyczna - zredukowana",
    "Model zerowy",
    "LDA",
    "QDA",
    "kNN k=1",
    "kNN k=2",
    "kNN k=3",
    "kNN k=4",
    "kNN k=5"
  ),
  Proporcja_bledow = c(
    log_pelny_error,
    log_redukowany_error,
    zerowy_error,
    lda_error,
    qda_error,
    mean(knn_1 != warunki_test),
    mean(knn_2 != warunki_test),
    mean(knn_3 != warunki_test),
    mean(knn_4 != warunki_test),
    mean(knn_5 != warunki_test)
  )
)

wyniki_klasyfikacji

# Najlepszy model według najniższej testowej proporcji błędów
wyniki_klasyfikacji[which.min(wyniki_klasyfikacji$Proporcja_bledow), ]