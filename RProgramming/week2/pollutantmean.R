pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  monitorMean <- c()
  for (monitorId in id) {
    fileName <- if (monitorId < 10) { 
      paste(directory, "/00", as.character(monitorId), ".csv", sep = "")
    } else if (monitorId < 100) {
      paste(directory, "/0", as.character(monitorId), ".csv", sep = "")
    } else {
      paste(directory, "/", as.character(monitorId), ".csv", sep = "")
    }
    
    currentMonitorDF <- read.csv(fileName)
    pollutantCol <- currentMonitorDF[pollutant]
    validPollutantCol <- pollutantCol[!is.na(pollutantCol)]
    
    monitorMean <- c(monitorMean, validPollutantCol)
  }
  
  return(mean(monitorMean))
}