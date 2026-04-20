pollutantmean <- function(directory, pollutant, id = 1:332){
  suma = 0
  n = 0
  for(idx in id){
    temp <- read.csv(file.path(directory, paste0(sprintf("%03d", idx), ".csv")))
    cecha <- temp[[pollutant]]
    cecha <- cecha[!is.na(cecha)]
    suma <- suma + sum(cecha)
    n <- n + length(cecha)
  }
  return(suma/n)
}

pollutemean("specdata", "nitrate", 23)

complete <- function(directory, id = 1:332) {
  result <- data.frame(id = integer(), nobs = integer())
  
  for (idx in id) {
    filename <- file.path(directory, paste0(sprintf("%03d", idx), ".csv"))
    data <- read.csv(filename)
    nobs <- sum(complete.cases(data))
    result <- rbind(result, data.frame(id = idx, nobs = nobs))
  }
  
  return(result)
}
complete("specdata", 1)
complete("specdata", c(2, 4, 8, 10, 12))

corr <- function(directory, threshold = 0) {
  result <- numeric(0)
  files <- list.files(directory, full.names = TRUE)
  
  for (file in files) {
    data <- read.csv(file)
    complete_data <- data[complete.cases(data[, c("sulfate", "nitrate")]), ]
    
    if (nrow(complete_data) > threshold) {
      corr_value <- cor(complete_data$sulfate, complete_data$nitrate)
      result <- c(result, corr_value)
    }
  }
  
  return(result)
}

cr <- corr("specdata", 150)
head(cr)
summary(cr)


cr <- corr("specdata", 400)
head(cr)


cr <- corr("specdata", 5000)
summary(cr)


cr <- corr("specdata")
summary(cr)


length(cr)