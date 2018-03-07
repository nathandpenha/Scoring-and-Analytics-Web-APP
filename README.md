# Scoring and Analytics Web APP

---
Plumber allows you to create a web API by merely decorating your existing R source code with special comments. Take a look at an example.

# plumber.R

#* @get /mean
normalMean <- function(samples=10){
  data <- rnorm(samples)
  mean(data)
}

#* @post /sum
addTwo <- function(a, b){
  as.numeric(a) + as.numeric(b)
}
These comments allow plumber to make your R functions available as API endpoints. You can use either #* as the prefix or #', but we recommend the former since #' will collide with Roxygen.

> library(plumber)
> r <- plumb("plumber.R")  # Where 'plumber.R' is the location of the file shown above
> r$run(port=9191)
You can visit this URL using a browser or a terminal to run your R function and get the results. Here we're using curl via a Mac/Linux terminal.

$ curl "http://localhost:9191/mean"
 [-0.254]
$ curl "http://localhost:9191/mean?samples=10000"
 [-0.0038]
As you might have guessed, the request's query string parameters are forwarded to the R function as arguments (as character strings).

$ curl --data "a=4&b=3" "http://localhost:9191/sum"
 [7]
You can also send your data as JSON:

$ curl --data '{"a":4, "b":5}' http://localhost:9191/sum
 [9]
Installation
You can install the latest stable version from CRAN using the following command:

install.packages("plumber")
If you want to try out the latest development version, you can install it from GitHub. The easiest way to do that is by using devtools.

library(devtools)
install_github("trestletech/plumber")
library(plumber)
### Credits
- ESPNCricinfo Statsguru
