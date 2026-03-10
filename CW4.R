# Napisz funkcję silnia, która oblicza silnię liczby całkowitej.
silnia <- function(x){
  if(x == 0){
    return(1)
  } else return(x*silnia(x-1))
}
silnia(4)

# Utwórz funkcję czy_pierwsza, która sprawdza, czy liczba jest pierwsza.
czy_pierwsza <- function(x){
  for (y in 2:floor(sqrt(abs(x)))){
    if(x%%y == 0){
      return("Liczba")
      }
    }
}

# Zdefiniuj funkcję, która przyjmuje listę liczb i zwraca ich średnią oraz odchylenie standardowe.
funkcja_s_sd <- function(x){
  return(c(
    średnia = mean(x),
    "odchylenie standardowe" = sd(x)))
}
funkcja_s_sd(c(2, 45, 53, 346, 4, 54, 754, 34, 235))

# Stwórz funkcję, która na podstawie podanych danych (imie, wiek, oceny) generuje ramkę danych.
stwórz_ramkę <- function(podane_imie, podany_wiek, podane_oceny){
                  data.frame(imie = podane_imie,
                             wiek = podany_wiek,
                             oceny = podane_oceny)
                }
(ramka <- stwórz_ramkę("Damian", 23, c(3, 4, 2, 5, 3, 4.5, 3.5)))
