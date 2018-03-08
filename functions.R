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
<<<<<<< HEAD
#* @get /batsmanAvgRunsOpposition
#* @png(width=400,400)
batsmanAvgRunsOpposition <- function(name="A Latecut",format){
  Opposition <-Runs <- NULL
  file<-paste("data/",name,"_",format,"_Batting.csv",sep = "")
  batsman <- clean(file)
  # Use dplyr's summarise to group by Opposition and compute mean runs and count
  meanRuns <- batsman %>% group_by(Opposition) %>% summarise(m= mean(Runs))
  countInnings <- batsman %>% group_by(Opposition) %>% summarise(len=length(Runs))
  # Set margins
  par(mar=c(9,4,3,2))
  opposition <- as.vector(meanRuns$Opposition)
  values <- paste(opposition,"-",countInnings$len)
  atitle <- paste(name,"'s Average Runs versus Opposition")
  barplot(meanRuns$m,names=values,las=2,ylab="Average Runs", 
          col=rainbow(length(meanRuns$m)),main=atitle)
  abline(h=50,lty=2,lwd=2)
  mtext("Opposition - No of innings", side=1, line=7.5, adj=1.0, cex=1.0, col="black")
  mtext("Data source-Courtesy:ESPN Cricinfo", side=3, line=0, adj=1.0, cex=0.8, col="blue")
}
=======

BoundariesVsSingles<-function(name, format){
  D<-read.csv(paste("data/",name,"_",format,"_Batting.csv",sep = ""))
  plot(batsman$Match.No, (batsman$Runs-(batsman$X4s*4 + batsman$X6s*6)))
}

>>>>>>> 0eb9e725718f376437b22a98938d98f5b896b424
