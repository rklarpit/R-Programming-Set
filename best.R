best <- function(state, outcome) {
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
  if(outcome == "heart attack"){
    #print(outcome)
    #t1 <- min(datastore[,11],na.rm = TRUE)
    #t1 <-min(datastore[,11][datastore$State == state],na.rm = TRUE)
    t1 <- suppressWarnings(which(as.numeric(newdatastore[,11])==min(as.numeric(newdatastore[,11])[newdatastore$State == state],na.rm = TRUE)))
    #t1 <-datastore[which(datastore[,11]==min(datastore[,11][datastore$State == state],na.rm = TRUE)),2]
    #t2 <-datastore[which(datastore[,11]==min(datastore[,11][datastore$State == state],na.rm = TRUE)),11]
    #print(t1)
    t2 <- newdatastore[t1,2]
  }
  if(outcome == "heart failure"){
    #print(outcome)
    
    #t1 <- min(datastore[,17],na.rm = TRUE)
    #t2 <- datastore[,1][datastore[,11]== t1]
    #print("I am here")
    #print(min(as.numeric(newdatastore[,17])[newdatastore$State == state],na.rm = FALSE))
    t1 <- suppressWarnings(which(as.numeric(newdatastore[,17])==min(as.numeric(newdatastore[,17])[newdatastore$State == state],na.rm = TRUE)))
    t2 <- newdatastore[t1,2]
    #print(t1)
  }
 if(outcome == "pneumonia"){
   #print(outcome)
   
   #t1 <- min(datastore[,17],na.rm = TRUE)
   #t2 <- datastore[,1][datastore[,11]== t1]
   t1 <- suppressWarnings(which(as.numeric(newdatastore[,23])==min(as.numeric(newdatastore[,23])[newdatastore$State == state],na.rm = TRUE)))
   t2 <- newdatastore[t1,2]
   #print(t1)
 }
 head(sort(t2),1)
  
}

