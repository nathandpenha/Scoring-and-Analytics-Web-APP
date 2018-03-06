# myfile.R

#' @filter cors
cors <-  function(res){
    res$setHeader("Access-Control-Allow-Origin","*")
    plumber::forward()    
}



#* @get /mean
normalMean <- function(samples=10){
    data <- rnorm(samples)
    mean(data)
}

#* @get /name
name<-function()
{
  D<-read.csv("Data/Names.csv",sep = ",")
  print(D$Names)
  
}


#* @get /sum
addTwo <- function(a, b){
    as.numeric(a) + as.numeric(b)
} 


#* @get /plotex
#* @png(width=400,400)
plot1 <- function(name,format){
  D<-read.csv(paste("data/",name,"_",format,"_Batting.csv",sep = ""))
  plot(D$Ground,D$SR)
} 

#* @get /StrikeRateVsGround
#* @png(width=400,400)
StrikeRateVsGround <- function(name,format){
  D<-read.csv(paste("data/",name,"_",format,"_Batting.csv",sep = ""))
  plot(D$Ground,D$SR)
} 


