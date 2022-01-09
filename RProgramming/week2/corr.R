source("complete.R")

corr <- function(directory, threshold = 0) {
  corrResult <- vector()
  
  completeDF <- complete(directory)
  completeDFAboveThresh <- subset(completeDF, nobs > threshold)
  for (monitorId in completeDFAboveThresh$id) {
    fileName <- if (monitorId < 10) { 
      paste(directory, "/00", as.character(monitorId), ".csv", sep = "")
    } else if (monitorId < 100) {
      paste(directory, "/0", as.character(monitorId), ".csv", sep = "")
    } else {
      paste(directory, "/", as.character(monitorId), ".csv", sep = "")
    }
    
    currentMonitorDF <- read.csv(fileName)
    filteredMonitorDF <- subset(currentMonitorDF, 
                                complete.cases(currentMonitorDF))
    
    corrResult <- c(corrResult, 
                    cor(filteredMonitorDF$sulfate, filteredMonitorDF$nitrate))
  }
  
  return(corrResult)
}