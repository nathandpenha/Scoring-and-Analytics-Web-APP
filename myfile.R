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
  
  
  
}


#* @get /sum
addTwo <- function(a, b){
    as.numeric(a) + as.numeric(b)
} 
