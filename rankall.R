rankall <- function(outcome, num = "best"){
  olddir <- getwd()
  setwd("/Users/rklarpit/Documents/Arpitbackup/R Working Directory/rprog-data-ProgAssignment3-data")
  datastore <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  datastore[is.na(datastore)]<- "Not Available"
  outcomeflag <- 100
  stateflag <- 100
  setwd(olddir)
  t1 <- NULL
  t2 <- NULL
 
  if( ! outcome %in% c("heart attack", "heart failure", "pneumonia")){
    outcomeflag <- 1
  }
  
  if(outcomeflag == 1){
    stop("invalid outcome")
  }
  u = sort(unique(datastore$State))
  #print (u)
  for (i in u){
  newdatastore <- subset(datastore,State == i)
  #print("i am in")
  #print(i)
  newdatastore[ newdatastore == "Not Available" ] = NA
  #print(head(newdatastore[,2]),1)
  
  if (outcome == "heart attack"){
    neworderdatastore <-newdatastore[order(as.numeric(newdatastore[,11]),newdatastore[,2]),]
    neworderdatastore <- neworderdatastore[complete.cases(neworderdatastore[, 11]),]
    #print(tail(neworderdatastore[,11],na.rm = TRUE))
    if (num == "best"){
      t1 <- c(neworderdatastore[1,2],i)
    }
    else if (num == "worst"){
      # print("worst")
      t1 <- c(neworderdatastore[nrow(neworderdatastore),2],i)
      
    }
    
    else if (is.numeric(num) && num < nrow(neworderdatastore)){
     # print("i am checking")
      t1 <- c(neworderdatastore[num,2],i)
      #print("t1 is")
      #print(t1)
      
    }
    else{
     # print("none")
      t1 <- c(NA,i)
      
    }
    print(t1)
    }
  if (outcome == "heart failure"){
    neworderdatastore <-newdatastore[order(as.numeric(newdatastore[,17]),newdatastore[,2]),]
    neworderdatastore <- neworderdatastore[complete.cases(neworderdatastore[, 17]),]
    #print(tail(neworderdatastore[,11],na.rm = TRUE))
    if (num == "best"){
      t1 <- c(neworderdatastore[1,2],i)
    }
    else if (num == "worst"){
      # print("worst")
      t1 <- c(neworderdatastore[nrow(neworderdatastore),2],i)
      #print(t1)
    }
    
    else if (is.numeric(num) && num < nrow(neworderdatastore)){
      t1 <- c(neworderdatastore[num,2],i)
      print(t1)
    }
    else{
      #print("none")
      t1 <- c(NA,i)
      
    }
  }
  if (outcome == "pneumonia"){
    neworderdatastore <-newdatastore[order(as.numeric(newdatastore[,23]),newdatastore[,2]),]
    neworderdatastore <- neworderdatastore[complete.cases(neworderdatastore[, 23]),]
    #print(tail(neworderdatastore[,11],na.rm = TRUE))
    if (num == "best"){
      t1 <- c(neworderdatastore[1,2],i)
    }
    else if (num == "worst"){
      # print("worst")
      t1 <- c(neworderdatastore[nrow(neworderdatastore),2],i)
      #print(t1)
    }
    
    else if (is.numeric(num) && num < nrow(neworderdatastore)){
      t1 <- c(neworderdatastore[num,2],i)
    }
    else{
      #print("none")
      t1 <- c(NA,i)
      
    }
  }
  t2 <- rbind(t2,t1)
  
  }
  t2
}