# Utwórz pętlę for, która iteruje po liczbach od 1 do 10 i wyświetla tylko liczby parzyste.
for (x in 1:10){
  if(x%%2 == 0){
    print(x)
  }
}

# Napisz pętlę while, która zaczyna od liczby 100 i odejmuje 10, dopóki liczba nie osiągnie 50.
liczba <- 100
while(liczba > 50){
  # w celu zobrazowania funckji wyświetlanie wartości liczby
  print(liczba)
  liczba <- liczba - 10
}
liczba

# Stwórz program, który na podstawie wartości zmiennej x (tekst) wypisuje komunikat za pomocą switch (np. “dzień tygodnia”).
x <- "czwarsatek"
dzień_tygodnia <- switch(x,
                         "niedziela" = 0,
                         "poniedziałek" = 1,
                         "wtorek" = 2,
                         "środa" = 3,
                         "czwartek" = 4,
                         "piątek" = 5,
                         "sobota" = 6,
                         stop("Niepoprawny dzień"))
dzień_tygodnia

# Wykorzystaj if-else, aby określić, czy liczba jest dodatnia, ujemna czy równa zero.0
liczba <- -0.1
if(liczba>0){
  print("liczba jest dodatnia")
} else if(liczba<0){
  print("liczba jest ujemna")
} else{
  print("liczba jest równa zero")
}