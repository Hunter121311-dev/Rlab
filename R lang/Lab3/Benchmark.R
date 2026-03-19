Factorial_loop <- function(n){
  if(n==0){
    return(1)
  }
  else{
    x <- 1
    for(i in 1:n){
    x <- x*i
    }
    return(x)
    }
}

library(purrr)
Factorial_reduce <- function(n){
  x <- 1:n
  return(x %>% reduce(`*`))
}

Factorial_recursive <- function(n){
  if(n == 0 | n == 1){
    return(1)
  } else return(Factorial_recursive(n-1)*n)
}

cache <- c("0" = 1, "1" = 1)

Factorial_mem <- function(n) {
  key <- as.character(n)
  
  if (key %in% names(cache)) {
    return(cache[key])
  }
  
  result <- n * Factorial_mem(n - 1)
  cache[key] <<- result
  
  return(result)
}

Factorial_loop(4)
Factorial_reduce(3)
Factorial_recursive(4)
Factorial_mem(40)

library(microbenchmark)
microbenchmark(
  Factorial_loop(43),
  Factorial_reduce(43),
  Factorial_recursive(43),
  Factorial_mem(43),
  times = 1000
)

# wyniki

# Unit: microseconds
# expr  min   lq    mean median   uq   max neval
# Factorial_loop(43)  1.1  1.3  1.4475    1.4  1.5   8.0  1000
# Factorial_reduce(43) 75.7 80.2 85.3143   83.3 85.3 268.7  1000
# Factorial_recursive(43) 11.6 12.5 13.5817   13.3 14.0  34.6  1000
# Factorial_mem(43)  2.9  3.3  4.3999    4.1  4.6  51.3  1000