makeCacheMatrix <- function(x = matrix()) {
  ## Inicjalizacja zmiennej do przechowywania odwrotności
  ## Na początku jest NULL (oznacza "jeszcze nie obliczona")
  inv <- NULL
  
  ## set: ustawia nową macierz i kasuje starą odwrotność
  ## (musi być <<- bo modyfikujemy zmienną z środowiska nadrzędnego)
  set <- function(y) {
    x <<- y          # nowa macierz
    inv <<- NULL     # kasujemy cache - nowa macierz = nowa odwrotność
  }
  
  ## get: zwraca aktualną macierz
  get <- function() x
  
  ## setinverse: zapisuje obliczoną odwrotność do cache
  setinverse <- function(inverse) {
    inv <<- inverse
  }
  
  ## getinverse: zwraca zapisaną odwrotność (lub NULL jeśli nie ma)
  getinverse <- function() inv
  
  ## Zwracamy listę funkcji - to jest nasz "specjalny obiekt macierzowy"
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  ## Pobieramy aktualnie zapisaną odwrotność z obiektu
  inv <- x$getinverse()
  
  ## Jeśli cache nie jest pusty - zwracamy od razu zapisaną wartość
  if(!is.null(inv)) {
    message("getting cached data")   # informacja dla użytkownika
    return(inv)
  }
  
  ## Jeśli cache jest pusty - musimy obliczyć odwrotność
  data <- x$get()                  # pobieramy macierz
  inv <- solve(data, ...)          # obliczamy odwrotność (solve() z base R)
  
  ## Zapisujemy wynik do cache (żeby następne wywołanie było błyskawiczne)
  x$setinverse(inv)
  
  ## Zwracamy odwrotność
  inv
}

# Tworzymy macierz
m <- matrix(c(4, 7,
              2, 6), nrow = 2, byrow = TRUE)

# Tworzymy obiekt cache
cached_matrix <- makeCacheMatrix(m)

# Pierwsze wywołanie - liczy odwrotność
inv1 <- cacheSolve(cached_matrix)
print("Pierwsze obliczenie odwrotności:")
print(inv1)

# Drugie wywołanie - powinno użyć cache
inv2 <- cacheSolve(cached_matrix)
print("Drugie wywołanie (z cache):")
print(inv2)

# Zmieniamy macierz (czyści cache)
cached_matrix$set(matrix(c(1, 2,
                           3, 4), nrow = 2, byrow = TRUE))

# Obliczamy odwrotność nowej macierzy
inv3 <- cacheSolve(cached_matrix)

print("Nowa odwrotność po zmianie macierzy:")
print(inv3)

# Sprawdzamy czy wynik jest poprawny
print(all.equal(inv3, solve(matrix(c(1, 2,
                                     3, 4), nrow = 2, byrow = TRUE))))