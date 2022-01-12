rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeDF <- read.csv("data/outcome-of-care-measures.csv")
  
  validOutcomes <- c("heart attack", "heart failure", "pneumonia")
  colNum <- list()
  colNum[["heart attack"]] <- 11
  colNum[["heart failure"]] <- 17
  colNum[["pneumonia"]] <- 23
  
  validNums <- c("best", "worst", 1:nrow(outcomeDF))
  
  ## Check that num and outcome are valid
  if (!outcome %in% validOutcomes) {
    stop("invalid outcome")
  }
  
  if (!num %in% validNums) {
    stop("invalid num")
  }
  
  ## Filter columns
  outcomeColNum <- as.numeric(colNum[outcome])
  outcomeDF <- subset(outcomeDF, select = c(2, 7, outcomeColNum))
  colnames(outcomeDF) <- c("hospital", "state", outcome)
  
  ## Cleanup data (remove NA)
  outcomeDF[outcome] <- sapply(outcomeDF[outcome], as.numeric)
  
  
  ## Split Data frame by states
  outcomeByState <- split(outcomeDF, outcomeDF$state)

  hospitalAtNum <- lapply(outcomeByState, function(odf, num) {
    # Order by outcome and then hospital
    odf <- odf[order(odf[, 3], odf[, 1]), ]
    
    # Remove NAs
    odf <- na.omit(odf)
    
    if(class(num) == "character") {
      if (num == "best") {
        return (odf$hospital[1])
      }
      else if (num == "worst") {
        print(nrow(odf))
        print(odf$hospital[nrow(odf)])
        return (odf$hospital[nrow(odf)])
      }
    }
    else {
      # Check boundary
      if (num > nrow(odf)) {
        return(NA)
      }
      
      return(odf$hospital[num])
    }
  }, num)
  
  #Return data.frame with format
  return ( data.frame(hospital = unlist(hospitalAtNum), 
                      state = names(hospitalAtNum)) )
  
  
}

