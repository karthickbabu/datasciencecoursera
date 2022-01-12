best <- function(state, outcome) {
  ## Read outcome data
  outcomeDF <- read.csv("data/outcome-of-care-measures.csv")
  validStates <- unique(outcomeDF["State"])$State
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  colNum <- list()
  colNum[["heart attack"]] <- 11
  colNum[["heart failure"]] <- 17
  colNum[["pneumonia"]] <- 23

  ## Check that state and outcome are valid
  if (!state %in% validStates) {
    stop("invalid state")
  }
  
  if (!outcome %in% validOutcomes) {
    stop("invalid outcome")
  }
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  stateOutcomes <- subset(outcomeDF, outcomeDF$State == state)
  outcomeColNum <- as.numeric(colNum[outcome])


  validStateOutcomes <- subset(stateOutcomes, 
                               !is.na(stateOutcomes[outcomeColNum]),
                               select = c(2, outcomeColNum))
  validStateOutcomes[ ,2] <- sapply(validStateOutcomes[ ,2], as.numeric)

  resultDF <- validStateOutcomes[
    order(validStateOutcomes[,2], validStateOutcomes[,1]),
  ]
  
  return (resultDF[1, ][1])
}