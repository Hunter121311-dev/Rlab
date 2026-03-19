best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", check.names = FALSE)
  
  if (!(state %in% data$State)) {
    stop("invalid state")
  }
  
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
  outcome_column <- c(
    "heart attack" = "Hospital 30-Day Death (Mortality) Rates from Heart Attack",
    "heart failure" = "Hospital 30-Day Death (Mortality) Rates from Heart Failure",
    "pneumonia" = "Hospital 30-Day Death (Mortality) Rates from Pneumonia"
  )
  
  state_data <- data[data$State == state, ]
  
  col <- outcome_column[outcome]
  state_data[, col] <- as.numeric(state_data[, col])
  
  state_data <- state_data[!is.na(state_data[, col]), ]
  
  state_data <- state_data[order(state_data[, col], state_data[["Hospital Name"]]), ]
  return(state_data[["Hospital Name"]][1])
}

best("SC", "heart attack")

best("NY", "pneumonia")

best("AK", "pneumonia")