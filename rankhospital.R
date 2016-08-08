rankhospital <- function(state,outcome, num = "best"){
  olddir <- getwd()
  setwd("/Users/rklarpit/Documents/Arpitbackup/R Working Directory/rprog-data-ProgAssignment3-data")
  datastore <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  datastore[is.na(datastore)]<- "Not Available"
  outcomeflag <- 100
  stateflag <- 100
  setwd(olddir)
  t1 <- NULL
  if( ! state %in% datastore$State){
    stateflag <- 1
  }
  if( ! outcome %in% c("heart attack", "heart failure", "pneumonia")){
    outcomeflag <- 1
  }
  if (stateflag == 1 & outcomeflag == 1){
    stop("invalid state and outcome")
  }
  if(stateflag == 1){
    stop("invalid state")
    
  }
  if(outcomeflag == 1){
    stop("invalid outcome")
  }
  newdatastore <- subset(datastore,State ==state)
  newdatastore[ newdatastore == "Not Available" ] = NA
 
  
  if (outcome == "heart attack"){
    neworderdatastore <-newdatastore[order(as.numeric(newdatastore[,11]),newdatastore[,2]),]
    neworderdatastore <- neworderdatastore[complete.cases(neworderdatastore[, 11]),]
    #print(tail(neworderdatastore[,11],na.rm = TRUE))
    if (num == "best"){
      t1 <- neworderdatastore[1,2]
    }
   else if (num == "worst"){
      #print("worst")
      t1 <- neworderdatastore[nrow(neworderdatastore),2]
      #print(t1)
    }
   
   else if (is.numeric(num) && num < nrow(neworderdatastore)){
      t1 <- neworderdatastore[num,2]
   }
    else{
      #print("none")
      t1 <- NA
      
  }}
  if (outcome == "heart failure"){
    neworderdatastore <-newdatastore[order(as.numeric(newdatastore[,17]),newdatastore[,2]),]
    neworderdatastore <- neworderdatastore[complete.cases(neworderdatastore[, 17]),]
    #print(tail(neworderdatastore[,11],na.rm = TRUE))
    if (num == "best"){
      t1 <- neworderdatastore[1,2]
    }
    else if (num == "worst"){
      #print("worst")
      t1 <- neworderdatastore[nrow(neworderdatastore),2]
      print(t1)
    }
    
    else if (is.numeric(num) && num < nrow(neworderdatastore)){
      t1 <- neworderdatastore[num,2]
    }
    else{
      #print("none")
      t1 <- NA
      
    }
  }
  if (outcome == "pneumonia"){
    neworderdatastore <-newdatastore[order(as.numeric(newdatastore[,23]),newdatastore[,2]),]
    neworderdatastore <- neworderdatastore[complete.cases(neworderdatastore[, 23]),]
    #print(tail(neworderdatastore[,11],na.rm = TRUE))
    if (num == "best"){
      t1 <- neworderdatastore[1,2]
    }
    else if (num == "worst"){
     # print("worst")
      t1 <- neworderdatastore[nrow(neworderdatastore),2]
      print(t1)
    }
    
    else if (is.numeric(num) && num < nrow(neworderdatastore)){
      t1 <- neworderdatastore[num,2]
    }
    else{
      #print("none")
      t1 <- NA
      
    }
  }
  
  t1
}