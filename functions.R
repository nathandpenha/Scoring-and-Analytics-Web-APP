# functions.R

#' @filter cors
cors <-  function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
}
library(dplyr)
library(EDAWR)
library(tidyr)
library(scatterplot3d)
library(lubridate)
library(forecast)
library(quadprog)
library(quantmod)
library(magrittr)
library(ggplot2)
#* @get /clean
clean <- function(name, format) {
  file <- paste("data/", name, "_", format, "_Batting.csv", sep = "")
  
  df <- read.csv(file,
                 stringsAsFactor = FALSE,
                 na.strings = c(NA, "-"))
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
  batsman$Runs <- as.numeric(gsub("\\*", "", batsman$Runs))
  batsman
}

#* @get /mean
normalMean <- function(samples = 10) {
  data <- rnorm(samples)
  mean(data)
}

#* @get /bowlingname
bowlingname <- function()
{
  D <- read.csv("Data/BowlingNames.csv", sep = ",")
  print(D$Names)
}

#* @get /name
name <- function()
{
  D <- read.csv("Data/Names.csv", sep = ",")
  print(D$Names)
}


#* @get /sum
addTwo <- function(a, b) {
  as.numeric(a) + as.numeric(b)
}


#* @get /plotex
#* @png(width=800,800)
plot1 <- function(name, format) {
  D <- read.csv(paste("data/", name, "_", format, "_Batting.csv", sep = ""))
  plot(D$Ground, D$SR)
}

#* @get /StrikeRateVsGround
#* @png(width=800,800)
StrikeRateVsGround <- function(name, format) {
  D <- clean(name, format)
  plot(D$Ground, D$SR)
}


#* @get /check
check <- function(file) {
  D <- clean(file)
  D
}


#* @get /batsmanAvgRunsGround
#* @png(width=800,800)
batsmanAvgRunsGround <- function(name, format) {
  batsman <- clean(name, format)
  # use dplyr's summarise function to group by Ground and calculate mean & count
  meanRuns <-
    batsman %>% group_by(Ground) %>% summarise(m = mean(Runs))
  countInnings <-
    batsman %>% group_by(Ground) %>% summarise(len = length(Runs))
  # Set the margins
  par(mar = c(9, 4, 3, 2))
  ground <- as.vector(meanRuns$Ground)
  values <- paste(ground, "-", countInnings$len)
  atitle <- paste(name, "'s Average Runs at Ground")
  barplot(
    meanRuns$m,
    names = values,
    las = 2,
    ylab = "Average Runs",
    col = rainbow(length(meanRuns$m)),
    main = atitle,
    cex.names = 0.8
  )
  abline(h = 50, lty = 3, lwd = 2)
  abline(
    h = 100,
    lty = 3,
    lwd = 2,
    col = "blue"
  )
  mtext(
    "Ground - No of innings",
    side = 1,
    line = 7.5,
    adj = 1.0,
    cex = 1.0,
    col = "black"
  )
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 3,
    line = 0,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
}


#* @get /batsmanAvgRunsOpposition
#* @png(width=800,800)
batsmanAvgRunsOpposition <- function(name, format) {
  Opposition <- Runs <- NULL
  batsman <- clean(name, format)
  # Use dplyr's summarise to group by Opposition and compute mean runs and count
  meanRuns <-
    batsman %>% group_by(Opposition) %>% summarise(m = mean(Runs))
  countInnings <-
    batsman %>% group_by(Opposition) %>% summarise(len = length(Runs))
  # Set margins
  par(mar = c(9, 4, 3, 2))
  opposition <- as.vector(meanRuns$Opposition)
  values <- paste(opposition, "-", countInnings$len)
  atitle <- paste(name, "'s Average Runs versus Opposition")
  barplot(
    meanRuns$m,
    names = values,
    las = 2,
    ylab = "Average Runs",
    col = rainbow(length(meanRuns$m)),
    main = atitle
  )
  abline(h = 50, lty = 2, lwd = 2)
  mtext(
    "Opposition - No of innings",
    side = 1,
    line = 7.5,
    adj = 1.0,
    cex = 1.0,
    col = "black"
  )
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 3,
    line = 0,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
}


#* @get /BoundariesVsSingles
#* @png(width=800,800)
BoundariesVsSingles <- function(name, format) {
  D <- clean(name, format)
  plot(batsman$Match.No, (batsman$Runs - (batsman$X4s * 4 + batsman$X6s *
                                            6)))
}


#* @get /batsmanPerfForecast
#* @png(width=800,800)
batsmanPerfForecast <- function(name, format) {
  b <- clean(name, format)
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
  ts.train <-
    ts(
      runs,
      start = c(startYear, startMonth),
      end = c(endYear, endMonth),
      frequency = 12
    )
  
  
  # Make a test set with the remaining 10 percent
  startMonth1 <- month(date[i + 1])
  startYear1 = year(date[i + 1])
  endMonth1 = month(date[rows])
  endYear1 = year(date[rows])
  
  ts.test <-
    ts(
      runs,
      start = c(startYear1, startMonth1),
      end = c(endYear1, endMonth1),
      frequency = 12
    )
  
  # Fit a Holt Winters Model with the training set
  fit <- HoltWinters(ts.train)
  
  # Forecast based on the model
  fcast <- forecast(fit)
  atitle = paste(name, "-", "Runs forecast")
  plot(
    fcast,
    main = atitle,
    col = "blue",
    lwd = 1.5,
    xlab = "Year",
    ylab = "Runs scored"
  )
  lines(ts.train, col = "magenta")
  
  # Draw the test set
  lines(ts.test, col = "red", lwd = 1.5)
  
  vals <- c("forecasted runs", "actual runs scored")
  col1 <- c("blue", "red")
  legend(
    x = "topleft",
    vals,
    lty = c(1, 1),
    lwd = c(1.5, 1.5),
    col = col1,
    bty = "n",
    cex = 0.8
  )
  accuracy(fcast, ts.test)
  
  
  #mtext("Data source-Courtesy:ESPN Cricinfo", side=1, line=4, adj=1.0, cex=0.8, col="blue")
}

#* @get /batsmanfours
#* @png(width=800,800)
batsmanfours <- function(name, format) {
  # Clean the batsman file and create a complete data frame
  df <- clean(name, format)
  
  # Get numnber of 4s and runs scored
  x4s <- as.numeric(as.vector(df$X4s))
  runs <- as.numeric(df$Runs)
  
  # Set margins
  par(mar = c(4, 4, 2, 2))
  
  atitle = paste(name, "-", "Runs scored vs No of 4s")
  
  # Plot no of 4s and a 2nd order curve fit
  plot(
    runs,
    x4s,
    xlab = "Runs",
    ylab = "Number of 4's",
    main = atitle,
    pch = 20,
    col = adjustcolor("red", alpha.f = 0.5)
  )
  
  # Second order polynomial used
  fit2 <- lm(x4s ~ poly(runs, 2, raw = TRUE))
  
  xx <- seq(from = 0,
            to = max(runs),
            by = 20)
  yy <- NULL
  for (i in seq_along(xx)) {
    yy[i] <-
      fit2$coefficients[3] * xx[i] ^ 2 + fit2$coefficients[2] * xx[i] + fit2$coefficients[1]
    
  }
  lines(xx, yy, col = "blue", lwd = 2.0)
  # Plot predicted 4s in 50 runs
  a <- predict(fit2, data.frame(runs = 50))
  abline(v = 50, lty = 3)
  abline(h = a, lty = 3)
  
  # Plot predicted 4s in 100 runs
  a <- predict(fit2, data.frame(runs = 100))
  abline(v = 100, lty = 4)
  abline(h = a, lty = 4)
  
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 1,
    line = 4,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
  
  
}



#* @get /batsmanContributionWonLost
#* @png(width=800,800)
batsmanContributionWonLost <- function(name, format) {
  result <- NULL
  playersp <- clean(name, format)
  
  won <- filter(playersp, result == 1)
  lost <- filter(playersp, result == 2 | result == 4)
  won$status = "won"
  lost$status = "lost"
  wonLost <- rbind(won, lost)
  atitle <- paste(name, "- Runs in games won/lost-drawn")
  
  # Create boxplots
  boxplot(
    Runs ~ status,
    data = wonLost,
    col = c("red", "green"),
    xlab = "Status of game",
    ylab = "Runs scored",
    main = atitle
  )
  
  
  a <- dim(won)
  b <- dim(lost)
  
  
  val1 <- paste(b[1], "games lost/drawn")
  val2 <- paste(a[1], "games won")
  vals <- list(val1, val2)
  legend(
    x = "top",
    legend = vals,
    lty = c(1, 1),
    lwd = c(7, 7),
    col = c("red", "green"),
    bty = "n"
  )
  
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 1,
    line = 4,
    adj = 1.0,
    cex = 1,
    col = "blue"
  )
  
}



#* @get /batsmansixs
#* @png(width=800,800)
batsmansixs <- function(name, format) {
  X6s <- NULL
  # Clean the batsman file and create a complete data frame
  df <- clean (name, format)
  
  # Remove all rows which have 0 6's
  b <- filter(df, X6s != 0)
  x6s <- as.numeric((b$X6s))
  runs <- as.numeric(b$Runs)
  
  # Set margins
  par(mar = c(4, 4, 2, 2))
  
  # Create a color palette
  p1 <- colorRampPalette(c("red", "blue"))
  palette <- p1(max(x6s))
  
  atitle = paste(name, "-", "Runs scored vs No of 6s")
  
  # Create box plot of number of 6s and the runs range
  boxplot(
    runs ~ x6s,
    main = atitle,
    xlab = "Number of 6s",
    ylab = "Runs scored",
    col = as.vector(palette)
  )
  
  
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 1,
    line = 4,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
  
}



#* @get /batsmanCumulativeAverageRuns
#* @png(width=800,800)
batsmanCumulativeAverageRuns <- function(name, format) {
  Runs = cs = no = BF = NULL
  df <- clean(name, format)
  b <- select(df, Runs)
  b$no <- seq.int(nrow(b))
  c <- select(b, no, Runs)
  
  d <- mutate(c, cs = cumsum(Runs) / no)
  
  ggplot(d) + geom_line(aes(x = no, y = cs))
  plot.title = paste(name, "- Cumulative Average vs No of innings")
}



#* @get /batsmanMeanStrikeRate
#* @png(width=800,800)
batsmanMeanStrikeRate <- function(name, format) {
  dataset <- clean(name, format)
  
  # Create a vector of runs with intervals of 15
  maxi <- (max(dataset$Runs / 15) + 1) * 15
  v <- seq(0, maxi, by = 15)
  a <- hist(dataset$Runs, breaks = v, plot = FALSE)
  
  
  # Compute the Mean Strike Rate for each run range
  SR <- NULL
  for (i in 2:length(a$breaks))  {
    b <- dataset$Runs > a$breaks[i - 1] & dataset$Runs <= a$breaks[i]
    c <- dataset[b,]
    SR[i - 1] <- mean(as.numeric(as.character(c$SR)))
  }
  
  # Find all intervals where there is no data i.e. NA
  b <- !is.na(SR)
  
  #Subset and remove the NAs for counts
  c <- a$mid[b]
  
  #Subset and remove the NAs for Strike Rate
  SR <- SR[b]
  
  
  
  par(mar = c(4, 4, 2, 2))
  atitle <- paste(name, "'s Mean Strike Rate vs Runs")
  plot(
    c,
    SR,
    pch = 16,
    xlab = "Runs",
    ylab = "Mean Strike Rate",
    ylim = c(0, 90),
    main = atitle
  )
  lines(c, predict(loess(SR ~ c)), col = "blue", lwd = 3)
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 1,
    line = 2,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
  
}



#* @get /batsmanMovingAverage
#* @png(width=800,800)
batsmanMovingAverage <- function(name, format) {
  # Compute the moving average of the time series
  dataset <- clean(name, format)
  
  #Subset runs and career dates
  runs <- dataset$Runs
  date <- dmy(dataset$Start.Date)
  
  timeframe <- data.frame(runs, date)
  
  
  atitle <- paste(name, "'s Moving average (Runs)")
  plot(
    timeframe$date,
    timeframe$runs,
    type = "o",
    col = "grey",
    xlab = "Year",
    ylab = "Runs",
    main = atitle
  )
  
  # Use loess regression to fit the moving average
  lines(timeframe$date,
        predict(loess(runs ~ as.numeric(date), timeframe)),
        col = "blue",
        lwd = 2)
  
  vals <- list("Runs scored", "Moving Average")
  legend(
    x = "topleft",
    legend = vals,
    lty = c(1, 1),
    lwd = c(2, 2),
    col = c("grey", "blue"),
    bty = "n",
    cex = 0.8
  )
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 1,
    line = 4,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
  
}

#* @get /batsmanPerfHomeAway
#* @png(width=800,800)
batsmanPerfHomeAway <- function(name, format) {
  ha <- NULL
  dataset <- clean(name, format)
  home <- filter(dataset, ha == 1)
  away <- filter(dataset, ha == 2)
  
  
  home$venue = "Home"
  away$venue = "Overseas"
  homeAway <- rbind(home, away)
  atitle <- paste(name, "- Runs-Home & overseas")
  
  # Create boxplots
  boxplot(
    Runs ~ venue,
    data = homeAway,
    col = c("blue", "green"),
    xlab = "Match venue",
    ylab = "Runs scored",
    main = atitle
  )
  
  
  a <- dim(home)
  b <- dim(away)
  
  
  par(mar = c(9, 7, 2, 2))
  val1 <- paste(a[1], "Home venue")
  val2 <- paste(b[1], "Overseas")
  vals <- list(val1, val2)
  legend(
    x = "top",
    legend = vals,
    lty = c(1, 1),
    lwd = c(7, 7),
    col = c("blue", "green"),
    bty = "n"
  )
  
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 1,
    line = 4,
    adj = 1.0,
    cex = 1,
    col = "blue"
  )
  
}

#* @get /batsmanRunsRanges
#* @png(width=800,800)
batsmanRunsRanges <- function(name, format) {
  # Clean file
  dataset <- clean(name, format)
  
  # Divide the runs into 20 run ranges from 0 to 400
  f <- cut(dataset$Runs, breaks = seq(from = 0, to = 400, by = 20))
  
  # Create a table
  g <- table(f)
  
  # Create a vector to store the runs frequency
  v <- as.vector(g)
  
  # Compute percentage of runs in the overall run total
  percentRuns <- (g / sum(g)) * 100
  runfreq <- c(name, round(percentRuns, 1), "\n")
  
  # Add a title
  atitle <- paste(name, "Runs %  vs Run ranges")
  
  # Plot the batting perormance
  barplot(
    percentRuns,
    main = atitle ,
    xlab = "Runs scored",
    ylab = "% times runs scored in range (%)",
    ylim = c(0, 100),
    col = "blue"
  )
  axis(side = 2, at = seq(0, 100, by = 5))
  
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 1,
    line = 2,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
  
}


#* @get /batsmanRunsFreqPerf
#* @png(width=800,800)
batsmanRunsFreqPerf <- function(name, format) {
  dataset <- clean(name, format)
  # Create breaks in intervals of 10
  maxi <- (max(dataset$Runs / 10) + 1) * 10
  v <- seq(0, maxi, by = 10)
  a <- hist(dataset$Runs, breaks = v, plot = FALSE)
  # Create mid points
  Runs <- a$mids
  RunFrequency <- a$counts
  df1 <- data.frame(Runs, RunFrequency)
  # Create a plot
  atitle <- paste(name, "'s", " Runs frequency vs Runs")
  plot(
    df1$Runs,
    df1$RunFrequency,
    pch = 16,
    xlab = "Runs",
    ylab = "Runs Frequency",
    main = atitle
  )
  lines(df1$Runs, predict(loess(df1$RunFrequency ~ df1$Runs)), col = "blue", lwd =
          3)
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 1,
    line = 2,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
  
  
}

#* @get /batsmanPerfBoxHist
#* @png(width=800,800)
batsmanPerfBoxHist <- function(name, format) {
  dataset <- clean(name, format)
  atitle <- paste(name, "'s", " - Runs Frequency vs Runs")
  nf <-
    layout(mat = matrix(c(1, 2), 2, 1, byrow = TRUE),
           heights = c(1, 3))
  par(mar = c(2, 2, 1, 1))
  boxplot(
    dataset$Runs,
    horizontal = TRUE,
    outline = TRUE,
    ylim = c(0, max(dataset$Runs)),
    frame = F,
    col = "green1"
  )
  # Drawing mean and meadian
  abline(v = median(dataset$Runs),
         col = "blue",
         lwd = 3.0)
  abline(v = mean(dataset$Runs),
         col = "red",
         lwd = 3.0)
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 3,
    line = 4,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
  maxi <- (max(dataset$Runs / 10) + 1) * 10
  v <- seq(0, maxi, by = 10)
  hist(
    dataset$Runs,
    breaks = v,
    xlab = "Runs",
    ylab = "Runs frequency",
    main = atitle,
    labels = TRUE,
    col = "grey"
  )
  abline(v = median(dataset$Runs),
         col = "blue",
         lwd = 3.0)
  abline(v = mean(dataset$Runs),
         col = "red",
         lwd = 3.0)
  abline(
    v = quantile(dataset$Runs, .25),
    col = "black",
    lwd = 3.0,
    lty = 2
  )
  abline(
    v = quantile(dataset$Runs, .75),
    col = "black",
    lwd = 3.0,
    lty = 2
  )
  rug(dataset$Runs, col = "blue", lwd = 2)
  mn <-
    paste("Mean runs over career:", round(mean(dataset$Runs), 2))
  md <-
    paste("Median runs over career:", round(median(dataset$Runs), 2))
  a <- hist(dataset$Runs, breaks = v, plot = FALSE)
  ht <- max(a$counts)
  text(200, ht - 15, mn, col = "brown")
  text(200, ht - 20, md, col = "brown")
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 3,
    line = 4,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
  par(mfrow = c(1, 1))
  
}

#* @get /battingPerfd
#* @png(width=800,800)
battingPerfd <- function(name, format) {
  # Clean the batsman file and create a complete data frame
  batsman <- clean(name, format)
  # Make a 3 D plot and fit a regression plane
  atitle <- paste(name, "- Runs  vs BF & Mins")
  s <-
    with(
      data = batsman,
      scatterplot3d(
        BF,
        Mins,
        Runs,
        color = rgb(0, 0, 255, 50, maxColorValue = 255),
        xlab = "Balls Faced",
        ylab = "Minutes in crease",
        zlab = "Runs scored",
        main = atitle,
        pch = 16
      )
    )
  # make a regression plabe
  fit <- with(data = batsman, lm(Runs ~ BF + Mins))
  # Draw the plane
  s$plane3d(fit)
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 1,
    line = 4,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
  
  
}

#* @get /batsmanDismissals
#* @png(width=800,800)
batsmanDismissals <- function(name, format) {
  dataset <- clean(name, format)
  lbls <- NULL
  d <- dataset$Dismissal
  dismissal <- data.frame(table(d))
  par(mar = c(0, 0, 2, 2))
  # Generate a 3D pie chart
  lbls <- dismissal$d
  slices <- dismissal$Freq
  pct <- round(slices / sum(slices) * 100)
  lbls <- paste(lbls, pct) # add percents to labels
  lbls <- paste(lbls, "%", sep = "") # ad % to labels
  atitle <- paste(name, "-Pie chart of dismissals")
  #we have to Ensure the number of labels & slices match
  plotrix::pie3D(
    slices,
    labels = lbls,
    explode = 0.1,
    main = atitle,
    pty = "s",
    labelcex = 0.8
  )
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 1,
    line = 4,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
  
}
#* @get /batsmanScoringRateODTT
#* @png(width=800,800)
batsmanScoringRateODTT <- function(name, format) {
  dataset <- clean(name, format)
  
  atitle <- paste(name, "- Runs vs Balls Faced")
  with(data = dataset, plot(BF, Runs, main = atitle))
  fit2 <- with(data = dataset, lm(Runs ~ poly(BF, 2, raw = TRUE)))
  xx <- seq(from = 0,
            to = max(dataset$BF),
            by = 5)
  # generate the predicted runs
  yy <- NULL
  #running for loop
  for (i in seq_along(xx)) {
    yy[i] <-
      fit2$coefficients[3] * xx[i] ^ 2 + fit2$coefficients[2] * xx[i] + fit2$coefficients[1]
    
  }
  # Draw the predicted runs
  lines(xx, yy, col = "blue", lwd = 2.0)
  bf = 50
  runs = fit2$coefficients[3] * bf ^ 2 + fit2$coefficients[2] * bf + fit2$coefficients[1]
  abline(
    v = 50,
    lty = 2,
    lwd = 2,
    col = "blue"
  )
  abline(
    h = runs,
    lty = 2,
    lwd = 2,
    col = "blue"
  )
  bf = 100
  runs = fit2$coefficients[3] * bf ^ 2 + fit2$coefficients[2] * bf + fit2$coefficients[1]
  abline(
    v = 100,
    lty = 3,
    lwd = 2,
    col = "red"
  )
  abline(
    h = runs,
    lty = 3,
    lwd = 2,
    col = "red"
  )
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 1,
    line = 4,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
  
}


cleanBowlerData <- function(name, format) {
  BPO <- Overs <- NULL
  # Read the <bowler>.csv file
  file <-
    paste("data/", name, "_", format, "_Bowling.csv", sep = "")
  
  df <- read.csv(file,
                 stringsAsFactor = FALSE,
                 na.strings = c(NA, "-"))
  
  # Remove rows with did not bowl
  a <- df$Overs != "DNB"
  bowler <- df[a,]
  
  # Remove rows with 'TDNB' - team did not bowl
  c <- bowler$Overs != "TDNB"
  bowler <- bowler[c,]
  
  # Get all complete cases
  c <- complete.cases(bowler)
  bowlerComplete <- bowler[c,]
  
  # Normalize overs which had 8 balls per over to the number of overs if there 8 balls per over
  if (names(bowlerComplete)[3] == "BPO") {
    bowlerComplete <-
      mutate(bowlerComplete, Overs = ifelse(BPO == 8, as.numeric(Overs) * 8 /
                                              6, Overs))
  }
  
  #Return the data frame
  bowlerComplete
}


#* @get /bowlerHistWickets
#* @png(width=800,800)
bowlerHistWickets <- function(name, format) {
  bowler <- cleanBowlerData(name, format)
  
  
  # Create a table of wickets
  wktsTable <- table(bowler$Wkts)
  
  #Convert to dataframe for easy processing
  wktsDF <- as.data.frame(wktsTable)
  
  #Remove column with "-"
  wktsDF <- wktsDF[2:nrow(wktsDF),]
  
  #Rename columns
  colnames(wktsDF) <- c("Wickets", "Freq")
  
  #Calculate wickets percentage
  wktsDF$freqPercent <- (wktsDF$Freq / sum(wktsDF$Freq)) * 100
  
  # Ensure ascending order of wickets
  wktsDF <-
    wktsDF[order(as.numeric(as.character(wktsDF$Wickets))),]
  #wktsDF
  atitle <- paste(name, "'s", " - Wkts percentage (%) vs Wkts")
  plot(
    as.numeric(as.character(wktsDF$Wickets)),
    wktsDF$freqPercent,
    type = "o",
    xlab = "Wickets",
    ylab = "Wicket percentages (%)",
    main = atitle,
    ylim = c(0, 50),
    pch = 15,
    col = "blue",
    lwd = "2"
  )
  
  mtext(
    "Data source-Courtesy:ESPN Cricinfo",
    side = 1,
    line = 4,
    adj = 1.0,
    cex = 0.8,
    col = "blue"
  )
}



