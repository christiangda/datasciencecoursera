rankall <- function(outcome, num = "best") {

  ## Read outcome data
  outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Change Data type to numeric, suppress "NA"
  outcome_data[, 11] <- suppressWarnings( as.numeric(outcome_data[, 11]) )
  outcome_data[, 17] <- suppressWarnings( as.numeric(outcome_data[, 17]) )
  outcome_data[, 23] <- suppressWarnings( as.numeric(outcome_data[, 23]) )
  
  ## Check that outcome are valid
  
  ## Valid Outcomes:  Only three "heart attack", "heart failure", "pneumonia"
  outcomes <- list("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  if(!(outcome %in% names(outcomes)))
    stop("invalid outcome")
  
  
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  outcome_col_name <- colnames(outcome_data)[outcomes[[outcome]]]
  data_cols <- c("Hospital.Name", "State", outcome_col_name)
  
  data_states <- split(outcome_data[,data_cols],
                       outcome_data["State"])
  
  ## Ordering data.frame data base on Rank and column
  rank_order <- function(states_list, num, column_to_order){
    
    data <- order(states_list[column_to_order],
                  states_list["Hospital.Name"], 
                  na.last = NA)
    #data <- na.omit(data)
    
    ## Valid num: best = 1, worst = lenght(outcome_data)
    if(num == "best"){
      num <- as.numeric(1)
    } else if(num == "worst"){
      num <- as.numeric(length(data))
    } else if(as.numeric(num)){
      num <- as.numeric(num)
    } else {
      stop("invalid num")
    }
    #Return Hospital Name from Index
    states_list$Hospital.Name[data[num]]
  }  
  
  #Apply rank_order function to list data_states
  data_states_ranked <-lapply(data_states,rank_order, num, outcome_col_name)
  
  hospital <- unlist(data_states_ranked)
  state <- names(data_states_ranked)
  
  data.frame(hospital, state, row.names = state)
  
}