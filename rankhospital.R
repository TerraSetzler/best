rankhospital <- function(state, outcome, num = best) {
  
  file.path("Users", "Terra", "Desktop", "Coursera", "outcome-of-care-measures.csv")
  data2 <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
  
  if(outcome != "heart attack" || "heart failure" || "pnumonia") {
    stop("invalid outcome")
  }
  
  data1 <- data2[which(data2$State == state),]
  
  if(outcome == "heart attack") {
    data3 <- data1[c(2, 11)]
    orderdata <- data3[order(data3[, 2], data3[, 1]),]
    orderdata <- orderdata[complete.cases(orderdata),]
    if(num == "best"){
      return(as.character(orderdata[1, 1]))
    } 
    if(num == "worst") {
      return(as.character(orderdata[nrow(orderdata), 1]))
    }
    return(as.character(orderdata[num, 1]))
  }
  if(outcome == "heart failure") {
    data3 <- data1[c(2, 17)]
    orderdata <- data3[order(data3[, 2], data3[, 1]),]
    orderdata <- orderdata[complete.cases(orderdata),]
    if(num == "best"){
      return(as.character(orderdata[1, 1]))
    } 
    if(num == "worst") {
      return(as.character(orderdata[nrow(orderdata), 1]))
    }
    return(as.character(orderdata[num, 1]))
  }
  if(outcome == "pnemonia") {
    data3 <- data1[c(2, 23)]
    orderdata <- data3[order(data3[, 2], data3[, 1]),]
    orderdata <- orderdata[complete.cases(orderdata),]
    if(num == "best"){
      return(as.character(orderdata[1, 1]))
    } 
    if(num == "worst") {
      return(as.character(orderdata[nrow(orderdata), 1]))
    }
    return(as.character(orderdata[num, 1]))
  }
}