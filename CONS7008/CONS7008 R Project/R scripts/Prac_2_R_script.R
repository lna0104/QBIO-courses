##Practical 2: Estimation and descriptive statistics

##2.2.Calculating descriptive statistics
{
  ##Enter the data from Team A
  ControlA <- c(0.2, 0.4, 0.5, 0.38, 0.6, 0.2, 0.8, 0.4, 0.4, 0.2)
  TreatmentA <- c(0.6, 0.8, 0.7, 0.8, 0.7, 0.6, 0.3, 0.6, 0.5, 0.9)
  
  ## Enter the data from Team B
  ControlB <- c(0.3, 0.6, 0.2)
  TreatmentB <- c(0.99, 0.7, 0.6)
  
  ## Inspect the data vectors to check the data input correctly
  ControlA
  TreatmentA
  ControlB
  TreatmentB
}

##2.3.Measure central tendency (mean and median)
{
  ##if you put () around a line of code, the output will be stored AND appear in the console!
  (meanControlA <- mean(ControlA))     ## Calculate the mean of TeamA control group
  (meanTreatmentA <- mean(TreatmentA)) ## Calculate the mean of TeamA treatment group
  (meanControlB <- mean(ControlB))     ## Calculate the mean of TeamB control group
  (meanTreatmentB <- mean(TreatmentB)) ## Calculate the mean of TeamB treatment group
  
  meanControlA <- mean(ControlA)       ## Here, note that the output does not appear in the console, but it is stored!
  
  (medianControlA <- median(ControlA))     
  (medianTreatmentA <- median(TreatmentA)) 
  (medianControlB <- median(ControlB))     
  (medianTreatmentB <- median(TreatmentB)) 
}

##2.4.Measures of "spread" 
var(ControlA)    ## Variance
sd(ControlA)     ## Standard deviation
range(ControlA)  ## Range

##2.5.Calculating summary statistics across an entire data set
{
  teamA <- cbind.data.frame(ControlA, TreatmentA)    ## Column bind the results for Team A and convert the data matrix to a data frame
  teamA #check if teamA has both sets of results
  teamB <- cbind.data.frame(ControlB, TreatmentB)
  teamB
  
  ##apply funcion(1-by row, 2-by column)
  apply(teamA, 2, mean)  ## Calculates the mean of each column (or variable), which tells us the average effect of the drug
  apply(teamA, 1, mean)  ## Calculates the mean of each row (or object) which isn't very informative
  
  sapply(teamA, mean)      ## Calculates the mean of each variable
  
  apply(teamA, 2, var)  ## Calculates the variance
  apply(teamA, 2, min)  ## Calculates the min for each column
  apply(teamA, 2, quantile) ## Calculates the quantile for each column
  apply(teamA, 2, length) ## Calculates the number of observation for each column
  
  
  apply(teamB, 2, mean)
}

##2.6.Writing your own functions
##Sum function
myMean <- function(x){       ## Define the function and its input; an arbitrary set of numbers ('x')
  Total <- sum(x)            ## Sum the elements of 'x' and store as 'Total'
  SampleSize <- length(x)    ## Count how many elements there are in 'x'
  Mean <- Total/SampleSize   ## Calculate the mean of 'x' by dividing the sum by the number of samples
  return(Mean)               ## Tell R what to 'spit out'
}                            ## End the definition of the function

myMean(ControlA)       ## Use the function to calculate the mean of 'ControlA'
mean(ControlA)         ## How does it compare to Rs in-built function?
sapply(teamA, myMean)  ## Apply the function to each column of a data frame
##Mode
mode <- function(x){                               ## Name the function ('mode') and its input: an arbitrary vector ('x')
  u <- unique(x)                                   ## 'u' has duplicate elements removed, keeping only `unique` values of your variable
  y <- lapply(u, function(y) length(x[x == y]))    ## Count the duplicates of each element of 'x';  that is, find the `length` of the variable) 
  u[which(unlist(y) == max(unlist(y)))]            ## Find the most duplicated element (the 'mode')
}                                                  ## End function definition

##tapply function
tapply(ControlA, ControlA, length)
tapply(ControlA, ControlA, sum)
tapply(TreatmentA, TreatmentA, length)

table(ControlA)

##2.7.Simulating data based on "population" parameters

##Generate random normal distribution
x <- rnorm(100, mean = 100, sd = 10)
hist(x)

## Simulate 6,000,000 data points with mean = 1.704 and sd = 0.066
SimData <- rnorm(n = 6000000, mean = 1.704, sd = 0.066)
hist(SimData)

## "sample" 10 objects from that population, making sure to sample any object only once (replace = FALSE) 
SimData10 <- sample(SimData, 10, replace = FALSE)
SimData10 ## Look at the heights of your 10 sampled individuals


## Calculate basic, descriptive statistics
mean(SimData10)
sd(SimData10)

{
  Means <- mean(SimData10)
  SampleSize <- 10
  for (n in c(30, 50, 70, 90)){
    SimDataSample <- sample(SimData, n, replace = FALSE)
    Means <- c(Means, mean(SimDataSample))
    SampleSize <- c(SampleSize, n)
  }
  
  plot(SampleSize, Means)
  abline(h = 1.704)  ## Add a line for the TRUE mean
}

{
  Ns <- c(10, 25, 85)  ## Sample sizes for R to work with
  Nreps <- 50 ## Number of reps per sample size (i.e. number of sample means per sample size)
  
  Results <- matrix(nrow = Nreps, ncol = length(Ns)) ## Set up a blank matrix to store results
  colnames(Results) <- Ns
  
  for(i in 1:length(Ns)){  ## For each sample size (c(10, 25, 85))
    for(k in 1:Nreps){     ## For each replicate
      Results[k, i] <- mean(sample(SimData, Ns[i], replace = FALSE))  ## Record the sample mean in the kth row based on the ith sample size
    }
  }
  
  Results  ## Look at the results
}

{
  library(tidyverse)  ## Load the library
  ## use the wonderful "pivot_longer" function
  ResultsLong<-pivot_longer(as.data.frame(Results), everything(), names_to = "SampleSize", values_to = "SampleMean", names_transform = as.numeric)
  
  ## now plot the results
  with(ResultsLong, plot(SampleMean ~ jitter(SampleSize, 0.5), col = SampleSize))  ## Plot the sample means by the sample sizes
  abline(h = 1.704)  ## Add the true population mean
}
##Increase number of samples => Mean values are closer to true mean and less spread

{
  ## Prepare the density plots
  D10 <- density(Results[, 1], adjust = 2)
  D25 <- density(Results[, 2], adjust = 2)
  D85 <- density(Results[, 3], adjust = 2)
  
  ## Scale the density plots so that they are visible
  D10$y <- D10$y * 2 + 10
  D25$y <- D25$y * 2 + 25
  D85$y <- D85$y * 2 + 85
  
  ## now make the plot
  with(ResultsLong, plot(SampleMean, SampleSize, col = SampleSize, 
                         ylim = c(0, max(D85$y)), 
                         xlim = c(min(D10$x), max(D10$x))))
  abline(v = 1.704)
  lines(D10, col = 10)
  lines(D25, col = 25)
  lines(D85, col = 85)
}

##2.8.Standard error of the mean
SimData10 <- sample(SimData, 10, replace = FALSE)     ## Sample 10 individuals 
SimData100 <- sample(SimData, 100, replace = FALSE)   ## Sample 100 individuals 
SimData1000 <- sample(SimData, 1000, replace = FALSE) ## Sample 1000 individuals 

sd(SimData10)   ## Calculate the standard deviation for SimData10
sd(SimData100)  ## Calculate the standard deviation for SimData100
sd(SimData1000) ## Calculate the standard deviation for SimData10000

## The standard error is calculated as follows (there is no inbuilt function):
sd(SimData10)/sqrt(length(SimData10))     ## Calculate the standard error for SimData10
sd(SimData100)/sqrt(length(SimData100))   ## Calculate the standard error for SimData100
sd(SimData1000)/sqrt(length(SimData1000)) ## Calculate the standard error for SimData10000

rm(list = ls()) 
