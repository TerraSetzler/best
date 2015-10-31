best <- function(state, outcome) {
  
  data2 <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  data1 <- data2[which(data2$State == state),]
  
  if(outcome != "heart attack" || "heart failure" || "pnumonia") {
    stop("invalid outcome")
  }
  
  if(outcome == "heart attack") {
    z <- min(as.numeric(paste(data1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)), na.rm = TRUE)
    
    data4 <- data1[which(as.numeric(data1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack) == z),]
    return(as.character(data4$Hospital.Name))
  }
  if(outcome == "heart failure") {
    z <- min(as.numeric(paste(data1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)), na.rm = TRUE)
    
    data4 <- data1[which(as.numeric(data1$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure) == z),]
    return(as.character(data4$Hospital.Name))
  }
  if(outcome == "pneumonia") {
    z <- min(as.numeric(paste(data1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)), na.rm = TRUE)
    
    data4 <- data1[which(as.numeric(data1$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia) == z),]
    return(as.character(data4$Hospital.Name))
  }
}