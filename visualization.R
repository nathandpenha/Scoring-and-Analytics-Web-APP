library("cricketr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.2")
pathToFile<-system.file("data","tendulkar.csv",package = "cricketr")
batsman4s(pathToFile,"Sachin Tendulkar")