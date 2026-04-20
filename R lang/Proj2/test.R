## czytanie danych
library(readr)
library(magrittr)
source("rozwiazanie.R") # twoje rozwiazanie
## zaladuj inne biblioteki jesli potrzebujesz

data <- read_csv("MIE.csv")
x <- make_LD(data)
print(class(x))
print(x)

## Obiekt 10 nie istnieje
out <- subject(x, 10)
print(out)

out <- subject(x, 14)
print(out)

out <- subject(x, 54) %>% summary
print(out)

out <- subject(x, 14) %>% summary
print(out)

out <- subject(x, 44) %>% visit(0) %>% room("bedroom")
print(out)

## Podsumowanie zanieczyszczen
out <- subject(x, 44) %>% visit(0) %>% room("bedroom") %>% summary
print(out)

out <- subject(x, 44) %>% visit(1) %>% room("living room") %>% summary
print(out)
