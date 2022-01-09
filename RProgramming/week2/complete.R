complete <- function(directory, id = 1:332) {
  fileid <- vector()
  compcases <- vector()
  
  for (monitorId in id) {
    fileName <- if (monitorId < 10) { 
      paste(directory, "/00", as.character(monitorId), ".csv", sep = "")
    } else if (monitorId < 100) {
      paste(directory, "/0", as.character(monitorId), ".csv", sep = "")
    } else {
      paste(directory, "/", as.character(monitorId), ".csv", sep = "")
    }
    
    currentMonitorDF <- read.csv(fileName)
    fileid <- c(fileid, monitorId)
    compcases <- c(compcases, sum(complete.cases(currentMonitorDF)))
  }
  
  return(data.frame(id = fileid, nobs = compcases))
}