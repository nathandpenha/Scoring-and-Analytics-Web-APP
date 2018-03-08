# myfile.R

#' @filter cors
cors <-  function(res){
    res$setHeader("Access-Control-Allow-Origin","*")
    plumber::forward()    
}

#* @get /clean
clean <- function(file) {
  
  df <- read.csv(file,stringsAsFactor=FALSE,na.strings=c(NA,"-"))
  # Remove rows where the batsman 'did not bat' - DNB
  a <- df$Runs != "DNB"
  batsman <- df[a,]
  
  # Remove rows with 'TDNB'
  c <- batsman$Runs != "TDNB"
  batsman <- batsman[c,]
  
  # Remove rows with absent
  d <- batsman$Runs != "absent"
  batsman <- batsman[d,]
  
  # Remove the "* indicating not out
  batsman$Runs <- as.numeric(gsub("\\*","",batsman$Runs))
  batsman
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


#* @get /StrikeRateVsGround
#* @png(width=400,400)
StrikeRateVsGround <- function(name,format){
  D<-read.csv(paste("data/",name,"_",format,"_Batting.csv",sep = ""))
  plot(D$Ground,D$SR)
} 

#* @get /check
check<-function(file){
D<-clean(file)
D
}

BoundariesVsSingles<-function(name, format){
  D<-read.csv(paste("data/",name,"_",format,"_Batting.csv",sep = ""))
  plot(batsman$Match.No, (batsman$Runs-(batsman$X4s*4 + batsman$X6s*6)))
}

