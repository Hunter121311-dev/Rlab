# Stwórz wektor liczb od 1 do 10 i oblicz ich średnią.
wektor = c(1:10)
(srednia <- mean(wektor))

# Utwórz macierz 3x3 z wartości od 1 do 9 i pomnóż ją przez 2.
(macierz <- matrix(data = 1:9, ncol = 3, nrow = 3))

# Stwórz ramkę danych z informacjami o produktach (nazwa, cena, ilość). Dodaj kolumnę z całkowitym kosztem (cena * ilość).
ramka <- data.frame(nazwa = c("jabłko", "banan", "pomarańcz"), cena = c(2, 4, 3), ilość = c(20, 3, 11))
(ramka["całkowity koszt"] <- ramka["ilość"]*ramka["cena"])


# Utwórz listę zawierającą wektor, macierz i ramkę danych. Wyświetl element ramki danych z listy.
(lista <- list(
  a = wektor,
  b = macierz,
  c = ramka
))
# pobranie elementu z ramki (cena jabłka)
(lista$c$cena[lista$c$nazwa == "jabłko"])
