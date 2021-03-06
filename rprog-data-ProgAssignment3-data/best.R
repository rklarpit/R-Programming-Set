best <- function(state, outcome){
  olddir <- getwd()
  setwd("/Users/rklarpit/Documents/Arpitbackup/R Working Directory/rprog-data-ProgAssignment3-data")
  datastore <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  stateflag <- NULL
  outcomeflag <- NULL
  if(  state %in% datastore$State){
    stateflag <- 1
  }
  if(  outcome %in% c(“heart attack”, “heart failure”, or “pneumonia”)){
    outcomeflag <- 1
  }
  if (stateflag == 1 && outcomeflag == 1){
    stop("invalid state and outcome")
  }
  if(stateflag == 1){
    stop("invalid state")
  }
  if(outcomeflag == 1){
    stop("invalid outcome")
  }
}
