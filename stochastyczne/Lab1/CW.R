inicjalizacja <- function(){
# ustawienie ziarna losowości
set.seed(42)

# ustawienie współczynnika uczenia
lr = 10^(-6)

# liczba obserwacji
n <- 100

# generowanie jednej cechy x[100,]
x <- runif(n, min = 10, max = 15)

# szum Gaussowski
noise <- rnorm(n, mean = 0, sd = 2)

# prawdziwy model y [100,]
y <- 2 * x + 5 + noise

# utworzenie macierzy z polem na bias X [100, 2]
X <- matrix(data = cbind(x, 1), nrow=100, ncol=2)

# definicja parametrów [2,]
param <- matrix(data=c(0, 0), ncol=1)
return(list(X = X, y = y, param = param))
}
# definicja funkcji straty
L <- function(w, x, y){
  return((1/nrow(x))*t(x%*%w-y)%*%(x%*%w-y))
}

# definicja gradientu straty
gL <- function(w, x, y){
  return((2/nrow(x))*(t(x)%*%(x%*%w - y)))
}

uczenie <- function(liczba_cykli, lr, param, X, y){
  loss <- matrix(0, nrow = liczba_cykli, ncol = 1)
  steps <- matrix(0, nrow = liczba_cykli, ncol = length(param))
  
  for (i in 1:liczba_cykli){
    gradient <- as.vector(gL(param, X, y))
    
    loss[i] <- as.numeric(L(param, X, y))
    steps[i, ] <- gradient
    
    param <- param - lr * gradient
  }
  
  wynik <- cbind(loss, steps)
  colnames(wynik) <- c("loss", "grad_w1", "grad_bias")
  return(wynik)
}

lrs <- c(10^(-6), 10^(-5), 10^(-4), 10^(-3), 10^(-2), 10^(-1))
cykle <- c(10, 100, 500, 1000, 2000)


# lr = 0.1 lub lr = 0.01 nawet przy tylko 10 cyklach powodują rozbieganie optymalizacji,
# uczenie jest nieefektywne

# lr = 0.001 optymalizacja mocno zwalnia po 14 krokach, a krok staje się minimalny,
# natomiast po 100 krokach min(loss) = 3.449567

# lr = 10^(-4) optymalizacja wolniej przebiega i po około 200 krokach zbliża się do granicy, ale wyżej niż dla lr = 0.001

# Mniejsze współczynniki uczenia spowalniają zbieżność, dlatego przy ograniczonej liczbie iteracji algorytm może nie osiągnąć minimum funkcji straty

# wniosek: efektywne uczenie jest możliwe przy dobraniu odpowiedniego współczynnika uczenia, a liczba cykli jest
# "wypadkową" tego wyboru - zbyt mały współczynnik uczenia powoduje bardzo małą zbieżność, zbyt duży niestabilność i rozbieżność optymalizacji

dane <- inicjalizacja()
wyniki <- uczenie(liczba_cykli = cykle[5], lr = lrs[4], param = dane$param, X = dane$X, y = dane$y)
plot(wyniki[,2])
min(wyniki[,1])

# najniższą wartość funkcji straty uzyskujemy przy lr = 0.001