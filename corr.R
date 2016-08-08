corr <- function(directory, threshold = 0){
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
  m <- NULL
  d<- NULL
  e <-NULL
  for (j in unique(z$ID)){
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
   for (k in unique(t$id)){
   # print(k,t)
    if(t$nobs[t$id == k]> threshold){
     #print(t[t$id == k])
      #print (k)
      d<- cor(z$sulfate[z$ID == k ], z$nitrate[z$ID == k],use ="complete.obs")
      #print(d)
      e <- c(e,d)
    }
    
  }
  #head (m)
  #names(t)
  #print(e)
  e
}
