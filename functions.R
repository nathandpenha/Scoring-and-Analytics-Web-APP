# functions.R

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
  file<-paste("data/",name,"_",format,"_Batting.csv",sep = "")
  D <- clean(file)
  plot(D$Ground,D$SR)
} 


#* @get /check
check<-function(file){
D<-clean(file)
D
}


#* @get /batsmanAvgRunsGround
#* @png(width=400,400)
batsmanAvgRunsGround <- function( name="A Latecut",format){
  file<-paste("data/",name,"_",format,"_Batting.csv",sep = "")
  batsman <- clean(file)
    # use dplyr's summarise function to group by Ground and calculate mean & count
  meanRuns <- batsman %>% group_by(Ground) %>% summarise(m= mean(Runs))
  countInnings <- batsman %>% group_by(Ground) %>% summarise(len=length(Runs))
    # Set the margins
  par(mar=c(9,4,3,2))
  ground <- as.vector(meanRuns$Ground)
  values <- paste(ground,"-",countInnings$len)
  atitle <- paste(name,"'s Average Runs at Ground")
  barplot(meanRuns$m,names=values,las=2,ylab="Average Runs", 
          col=rainbow(length(meanRuns$m)),main=atitle,cex.names=0.8)
  abline(h=50,lty=3,lwd=2)
  abline(h=100,lty=3,lwd=2,col="blue")
  mtext("Ground - No of innings", side=1, line=7.5, adj=1.0, cex=1.0, col="black")
  mtext("Data source-Courtesy:ESPN Cricinfo", side=3, line=0, adj=1.0, cex=0.8, col="blue")
}


#* @get /batsmanAvgRunsOpposition
#* @png(width=400,400)
batsmanAvgRunsOpposition <- function(name,format){
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


#* @get /BoundariesVsSingles
#* @png(width=400,400)
BoundariesVsSingles<-function(name, format){
  file<-paste("data/",name,"_",format,"_Batting.csv",sep = "")
  D <- clean(file)
  plot(batsman$Match.No, (batsman$Runs-(batsman$X4s*4 + batsman$X6s*6)))
}


#* @get /batsmanPerfForecast
#* @png(width=400,400)
batsmanPerfForecast <- function( name,format) {
  file<-paste("data/",name,"_",format,"_Batting.csv",sep = "")
  b <- clean(file)
  library(lubridate)
  library(forecast)
  library(quadprog)
  library(quantmod)
  # Read day, month and year
  date <- dmy(b$Start.Date)
  runs <- b$Runs
  
  # Create a training and a test set
  # Subset 90 percent of the rows of the time series
  rows <- length(runs)
  i <- floor(0.9 * rows)
  
  # Note the start/end month and year
  startMonth = month(date[1])
  startYear = year(date[1])
  endMonth = month(date[i])
  endYear = year(date[i])
  
  # Create training set with the 90 percent career 
  ts.train <- ts(runs, start = c(startYear,startMonth), end = c(endYear,endMonth),frequency=12)
  
  
  # Make a test set with the remaining 10 percent
  startMonth1 <- month(date[i+1])
  startYear1 = year(date[i+1])
  endMonth1 = month(date[rows])
  endYear1 = year(date[rows])
  
  ts.test <- ts(runs, start = c(startYear1,startMonth1), end = c(endYear1,endMonth1),frequency=12)
  
  # Fit a Holt Winters Model with the training set
  fit <-HoltWinters(ts.train)
  
  # Forecast based on the model
  fcast <- forecast(fit)
  atitle = paste(name,"-","Runs forecast" )
  plot(fcast,main=atitle,col="blue",lwd=1.5,xlab="Year",ylab="Runs scored")
  lines(ts.train,col="magenta")
  
  # Draw the test set
  lines(ts.test,col="red",lwd=1.5)
  
  vals <- c("forecasted runs","actual runs scored")
  col1 <- c("blue","red")
  legend(x="topleft",vals, lty=c(1,1),   
         lwd=c(1.5,1.5),col=col1,bty="n",cex=0.8)
  accuracy(fcast,ts.test)
  
  
  mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
}

