rankhospital <- function(state, outcome, num) {
  
  outcome.df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  outcome.df[, 11] <- as.numeric(outcome.df[,11])
  outcome.df[, 17] <- as.numeric(outcome.df[,17])
  outcome.df[, 23] <- as.numeric(outcome.df[,23])
  
  choosenState <- outcome.df[outcome.df$State == state,]
  if (nrow(choosenState) == 0)
    stop("invalid state")
  
  outcomeMatrix <- data.frame(Outcome = c("heart attack", "heart failure","pneumonia"), Col = c(11,17,23))
  
  choosenOutcome <- outcomeMatrix[outcomeMatrix$Outcome == outcome, 2]
  
  if (nrow(outcomeMatrix[outcomeMatrix$Outcome == outcome, ]) == 0) 
    stop("invalid outcome")
  
  choosenState <- choosenState[complete.cases(choosenState[, choosenOutcome]),]
  choosenState <- choosenState[order(choosenState[, choosenOutcome], choosenState$Hospital.Name),]
   
  if (num == "best")
    num <- 1
  if (num == "worst")
      num <- nrow(choosenState)
     
  return(choosenState[num,]$Hospital.Name)
}

