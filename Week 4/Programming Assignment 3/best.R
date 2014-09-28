best <- function(state, outcome) {
    
  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Change Data type to numeric, suppress "NA"
  outcome_data[, 11] <- suppressWarnings( as.numeric(outcome_data[, 11]) )
  outcome_data[, 17] <- suppressWarnings( as.numeric(outcome_data[, 17]) )
  outcome_data[, 23] <- suppressWarnings( as.numeric(outcome_data[, 23]) )
  outcome_data <- na.omit( outcome_data )
                   
  ## Check that state and outcome are valid
  
  ## Valid States: 2-characters Names
  states <- c(outcome_data$State)  
  if(!(state %in% states))
    stop("invalid state")
  
  ## Valid Outcomes:  Only three "heart attack", "heart failure", "pneumonia"
  outcomes <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  if(!(outcome %in% names(outcomes)))
    stop("invalid outcome")  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  state_data <- subset(outcome_data, State == state)
  state_data <- state_data[ order(state_data[,outcomes[[outcome]]], na.last = TRUE),c("Hospital.Name")]
  state_data <- na.omit(state_data)
  state_data[1]
}