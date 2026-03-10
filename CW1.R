# Przełóż funkcję z języka Scheme do R:
#  Scheme:
#  
#  (define (suma x y)
#   (+ x y))

# Napisz równoważną funkcję w R.
sum_f <- function(x, y){
  return(x + y)
}

# Stwórz funkcję anonimową:
#   Zdefiniuj funkcję anonimową w R, która oblicza sześcian liczby.
liczba <- 4
(function(x) x^3)(liczba)

# Użyj funkcji wyższego rzędu:
#   Napisz funkcję w R, która przyjmuje funkcję i listę, a następnie zwraca wynik zastosowania tej funkcji do każdego elementu listy.
funkcja <- function(x) x^2
funkcja_2 <- function(x, f){lapply(x, f)}
funkcja_2(c(1:4), funkcja)
