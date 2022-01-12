rankhospital <- function(state, outcome, num = "best") {
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
  
  resultIndex <- NULL
  if (!is.na(as.numeric(num))) {
    # num is a number
    resultIndex <- "middle"
  } else {
    # possibly a string
    if (num == "best") {
      resultIndex <- "first"
    } else if (num == "worst") {
      resultIndex <- "last"
    }
  }
  
  if (is.null(resultIndex)) {
    stop("invalid num")
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
  
  if (resultIndex == "best") {
    num <- 1
  } else if (resultIndex == "worst") {
    num <- nrow(resultDF)
  } 
  
  if (num > nrow(resultDF) || num < 1) {
    return(NA)
  }
  
  return (resultDF[num, ][1])
}