complete <- function(directory, id = 1:332){
  olddir <- getwd()
  setwd(directory)
  z <- NULL

  complete_ans <-NULL
  #print(pollutant)
  for (i in  list.files()){
    #print(read.csv(i))
    z <-rbind(z,read.csv(i))
  }
  ncount <-NULL
  s<-NULL
 t<-NULL
 
 
  for (j in id){
    #print(sum(!is.na(z$sulfate[z$ID == j])))
   #print(sum(!is.na(z$nitrate[z$ID == j])))
  if (sum(!is.na(z$sulfate[z$ID == j])) <= sum(!is.na(z$nitrate[z$ID == j]))){
    #print("I am in if")
    s<- cbind(j,sum(!is.na(z$sulfate[z$ID == j ])))
   
  }
    else{
     # print("I am in else")
      s<- cbind(j,sum(!is.na(z$nitrate[z$ID == j] )))
    }
    t<- data.frame(rbind(t,s))
  }
  colnames(s)<- c("id","nobs")
  colnames(t)<- c("id","nobs")
  setwd(olddir)
  #print(class(t))
 t
}

