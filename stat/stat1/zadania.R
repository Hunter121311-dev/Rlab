# Zadanie 6
goals <- scan("stat/stat1/goals.dat")
lambda_est <- mean(goals)
lambda_est
# estymator sprowadza się do śrendiej z próby

# Zadanie 7
x <- scan("stat/stat1/gamma.dat")
m <- mean(x)
v <- var(x)

# metoda momentów
s_mom <- m^2 / v
r_mom <- m / v

s_mom
r_mom

# metoda MLE
library(MASS)
fit <- fitdistr(x, "gamma")
fit

# Zadanie 8
x <- trees$Height
n <- length(x)
mean(x)
sd(x)

# przedział ufności 0.95
t.test(trees$Height, conf.level = 0.95)$conf.int

# przedział ufności 0.99
t.test(trees$Height, conf.level = 0.99)$conf.int

# Zadanie 9
sigma <- 1.5
d <- 0.5
alpha <- 0.01

n <- (qnorm(1 - alpha/2) * sigma / d)^2
n
ceiling(n)

# Zadanie 10
x <- scan("stat/stat1/blaszki.dat")

# hipotezy:
# H0: mu = 0.04
# H1: mu < 0.04

alpha <- 0.01
mu0 <- 0.04
n <- length(x)

# statystyki z próby
x_bar <- mean(x)
s <- sd(x)

# statystyka testowa
t_stat <- (x_bar - mu0) / (s / sqrt(n))

# wartość krytyczna dla testu lewostronnego
t_crit <- qt(alpha, df = n - 1)

# p-value
p_value <- pt(t_stat, df = n - 1)

# wypisanie wyników
x_bar
s
t_stat
t_crit
p_value

# powrót do hipotez i wyświetlenie wyniku
if (t_stat < t_crit) {
  cat("Odrzucamy H0 na poziomie istotności", alpha, "\n")
  cat("Można twierdzić, że blaszki są cieńsze niż 0.04 mm.\n")
} else
  {
  cat("Brak podstaw do odrzucenia H0 na poziomie istotności", alpha, "\n")
  cat("Nie można twierdzić, że blaszki są cieńsze niż 0.04 mm.\n")
  }

# Zadanie 11
# liczba popraw
x <- c(12, 5)

# liczba pacjentów w grupach
n <- c(51, 46)

# test dwóch proporcji
test <- prop.test(x, n)

test

alpha <- 0.05

if (test$p.value < alpha) {
  cat("Odrzucamy H0 – lek wpływa na samopoczucie pacjentów.\n")
} else {
  cat("Brak podstaw do odrzucenia H0 – brak dowodów na wpływ leku.\n")
}

# Zadanie 12
men <- c(171,176,179,189,176,182,173,179,184,186,189,167,177)
women <- c(161,162,163,162,166,164,168,165,168,157,161,172)

# poziom istotności
alpha <- 0.1

# test F dla wariancji
test <- var.test(men, women, alternative = "greater", conf.level = 1 - alpha)

test

if(test$p.value < alpha){
  cat("Odrzucamy H0 – zmienność wzrostu mężczyzn jest większa.\n")
} else {
  cat("Brak podstaw do odrzucenia H0 – brak dowodów, że zmienność wzrostu mężczyzn jest większa.\n")
}
