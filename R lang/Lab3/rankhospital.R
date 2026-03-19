rankhospital <- function(state, outcome, num = "best") {
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
  
  if (num == "best") {
    rank_number <- 1
  } else if (num == "worst") {
    rank_number <- nrow(state_data)
  } else {
    rank_number <- as.numeric(num)
  }
  
  if (is.na(rank_number) || rank_number < 1 || rank_number > nrow(state_data)) {
    return(NA)
  }
  
  return(state_data[["Hospital Name"]][rank_number])
}

rankhospital("NC", "heart attack", "worst")

rankhospital("WA", "heart attack", 7)

rankhospital("TX", "pneumonia", 10)

rankhospital("NY", "heart attack", 7)