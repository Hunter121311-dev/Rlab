rankall <- function(outcome, num = "best") {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", check.names = FALSE)
  
  valid_outcomes <- c("heart attack", "heart failure", "pneumonia")
  
  if (!(outcome %in% valid_outcomes)) {
    stop("invalid outcome")
  }
  
  outcome_column <- c(
    "heart attack" = "Hospital 30-Day Death (Mortality) Rates from Heart Attack",
    "heart failure" = "Hospital 30-Day Death (Mortality) Rates from Heart Failure",
    "pneumonia" = "Hospital 30-Day Death (Mortality) Rates from Pneumonia"
  )
  
  states <- sort(unique(data$State))
  
  col <- outcome_column[outcome]
  data[, col] <- as.numeric(data[, col])
  data <- data[!is.na(data[, col]), ]
  
  result <- character(length(states))
  
  for (i in seq_along(states)) {
    state_data <- data[data$State == states[i], ]
    
    state_data <- state_data[order(state_data[, col], state_data[["Hospital Name"]]), ]
    
    if (num == "best") {
      rank_number <- 1
    } else if (num == "worst") {
      rank_number <- nrow(state_data)
    } else {
      rank_number <- as.numeric(num)
    }
    
    if (is.na(rank_number) || rank_number < 1 || rank_number > nrow(state_data)) {
      result[i] <- NA
    } else {
      result[i] <- state_data[["Hospital Name"]][rank_number]
    }
  }
  
  return(data.frame(hospital = result, state = states))
}



r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)



r <- rankall("pneumonia", "worst")
as.character(subset(r, state == "NJ")$hospital)


r <- rankall("heart failure", 10)
as.character(subset(r, state == "NV")$hospital)