pollutantmean <- function(directory, pollutant, id = 1:332){
  olddir <- getwd()
  setwd(directory)
  z <- NULL
  mean_ans <- NULL
  #print(pollutant)
  for (i in  list.files()){
    #print(read.csv(i))
    z <-rbind(z,read.csv(i))
  }
  if (pollutant == "sulfate"){
    #for(j in id){
    mean_ans <-mean(z$sulfate[z$ID >= id[1] & z$ID <= id[length(id)]],na.rm = TRUE)
    #}
  }    
  if (pollutant == "nitrate"){
    #for(j in id){
    mean_ans <-mean(z$nitrate[z$ID >= id[1] & z$ID <= id[length(id)]],na.rm = TRUE)
    #}
  }
  
  setwd(olddir)
  mean_ans
}
