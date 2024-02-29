##Practical 3: Introduction to hypothesis testing

##3.3 Estimating the test statistic
## Enter the data from Team A
ControlA <- c(0.2, 0.4, 0.5, 0.38, 0.6, 0.2, 0.8, 0.4, 0.4, 0.2)
TreatmentA <- c(0.6, 0.8, 0.7, 0.8, 0.7, 0.6, 0.3, 0.6, 0.5, 0.9)
teamA <- cbind.data.frame(ControlA, TreatmentA)

## Enter the data from Team B
ControlB <- c(0.3, 0.6, 0.2)
TreatmentB <- c(0.99, 0.7, 0.6)
teamB <- cbind.data.frame(ControlB, TreatmentB)

EffectA <- mean(teamA$TreatmentA) - mean(teamA$ControlA)  
## Difference in control and treatment means for team A

EffectB <- mean(teamB$TreatmentB) - mean(teamB$ControlB)   
## Difference in control and treatment means for team B

#The value of the test statistic (t) depends not only on this difference in mean, but also on the standard error.
#The standard error (SE) tells us how confident we are in the estimate of the parameter.

{
  teamA.mean <- apply(teamA, 2, mean) ## First, calculate the mean of each variable (treatment vs control)
  teamA.sd <- apply(teamA, 2, sd) ## Then calculate the standard deviation of each variable
  teamA.n<-nrow(teamA) ## calculate the sample size for teamA (number of rows)
  teamA.se <- teamA.sd/(sqrt(teamA.n)) ## Calculate the se by dividing the sd by the square root of the sample size 
  ## Note that there is no inbuilt R function to calculate SE. 
  
  
  ## There is no inbuilt function we can call to plot mean +/- SE, so we need to write it out
  n.se <- 2 ## height of error bars (as number of standard errors) - you can change this! See how far apart the means are.
  
  ## Set up plot initially without any points or lines (type = "n") to define x and y axis limits. 
  ## We will create custom x axis ticks (xaxt="n").
  plot(x = c(0.5, 2.5),  ## The values along to the x-axis
       y = range(c(teamA.mean + n.se * teamA.se, teamA.mean - n.se * teamA.se)), ## The values along to the y-axis
       type = "n",     ## Do not plot any points or lines
       xlab = "Group", ## x-axis label
       ylab = "Survival (Mean +/- SE)",  ## y-axis label
       xaxt = "n") ## x-axis tick mark labels
  
  ## Add x-axis ticks and their labels
  axis(1, 1:2, labels = c("Control", "Treatment"))
  
  # Plot the means on the plot
  points(1:2, teamA.mean, pch = 19)
  
  ## Add error bars as "arrows" with heads at each end (code=3) and flat ends (angle=90) 
  for(i in 1:2){
    arrows(x0 = i, ## The x value for the ith error bar
           y0 = teamA.mean[i] + n.se * teamA.se[i],  ## The upper value for the ith error bar
           y1 = teamA.mean[i] - n.se * teamA.se[i],  ## The lower value for the ith error bar
           angle = 90,
           code = 3)
  }
}

##3.4. One sample t-test
t.stat <- (EffectA)/teamA.se[2]  ## Convert mean of the treatment group to "t-statistic"
t.stat                           ## Look at the value of the t-statistic?
t.test(TreatmentA, mu = mean(ControlA))  ## Use R's inbuilt function to implement this one-sample t-test. 
## Here, "mu" is a fixed point against which we test whether the observed TreatmentA mean is the same.

##3.5. Two sample t-test

seJoint <- sqrt(teamA.se[2]^2 + teamA.se[1]^2 )  ## Standard error of the difference between the two means
Tval <- (EffectA)/seJoint     ## Convert difference of means to "T"
Tval                          ## Look at the value of T
t.test(TreatmentA, ControlA)  ## Same thing using R's generic "t.test" program


##3.8. The power of the t-test

power.t.test(                               ## Calculate Power of T-test, and output the variable set "NULL"
  n = length(TreatmentA),                     ## Sample size of each group
  delta = mean(TreatmentA) - mean(ControlA),  ## Difference between the means (effect size)
  sd = sd(ControlA) ,                         ## Set a common standard deviation
  sig.level = 0.05,                           ## Set a Type I error rate
  power = NULL,                               ## Set the statistical power (1 - Type II error rate)
  type = "two.sample",                        ## Request a T-test comparing means of two groups
  alternative = "one.sided"                   ## Alternative to null hypothesis is Treatment > Control
)
#Given an effect size of 0.242, and a sample size of 10 per group, the probability of correctly rejecting 
#the null hypothesis is 0.8584562.


power.t.test(                               ## Calculate Power of T-test, and output the variable set "NULL"
  n = length(TreatmentB),                     ## Sample size of each group
  delta = mean(TreatmentB) - mean(ControlB),  ## Difference between the means (effect size)
  sd = sd(ControlA) ,                         ## Set a common standard deviation
  sig.level = 0.05,                           ## Set a Type I error rate
  power = NULL,                               ## Set the statistical power (1 - Type II error rate)
  type = "two.sample",                        ## Request a T-test comparing means of two groups
  alternative = "one.sided"                   ## Alternative to null hypothesis is Treatment > Control
)

#Effect Size to Power
EffectSize <- seq(0.0, 0.3, 0.01)     ## Set various effects sizes
Power <- vector()                     ## Request space to hold the power estimates
length(Power) = length(EffectSize)    ## Allocate space to hold the power estimates
for(i in 1:length(EffectSize)){       ## Loop through the effect sizes
  Power[i] <- power.t.test(             ## Record the output at the "ith" place in "Power"
    n = 10,                               ## Set the sample size of each group
    delta = EffectSize[i],                ## Difference between the means (effect size)
    sd = sd(ControlA) ,                   ## Set a common standard deviation
    sig.level = 0.05,                     ## Set a Type I error rate
    power = NULL,                         ## Set the statistical power (1 - Type II error rate)
    type = "two.sample",                  ## Request a T-test comparing means of two groups
    alternative = "one.sided")$power      ## Alternative to null hypothesis is Treatment > Control
}                                     ## Output the element called "power", then re-loop
plot(EffectSize, Power, type = "b")   ## Graph power against effect size

#Sample Size to Power
SampleSize <- seq(2, 30, 2)                 ## Set various ssmple sizes
Power <- vector()                           ## Request space to hold the power estimates
length(Power) = length(SampleSize)          ## Allocate space to hold the power estimates
for(i in 1:length(SampleSize) ){            ## Loop through the sample sizes
  Power[i] <- power.t.test(                   ## Record the output at the "ith" place in "Power"
    n = SampleSize[i],                          ## Set the sample size of each group
    delta = mean(TreatmentA) - mean(ControlA),  ## Difference between the two means
    sd = sd(ControlA) ,                         ## Set a common standard deviation
    sig.level = 0.05,                           ## Set a Type I error rate
    power = NULL,                               ## Set the statistical power (1 - Type II error rate)
    type = "two.sample",                        ## Request a T-test comparing means of two groups
    alternative = "one.sided")$power            ## Alternative to null hypothesis is Treatment > Control
}                                           ## Output the element called "power", then re-loop
plot(SampleSize, Power, type = "b")         ## Graph power against sample size

#If Team A found an effect size of 0.463 (the effect size Team B found), 
#what sample size per group should leave Team A confident of not making a Type II error??
#change delta to 0.463
SampleSize <- seq(2, 30, 2)                 ## Set various ssmple sizes
Power <- vector()                           ## Request space to hold the power estimates
length(Power) = length(SampleSize)          ## Allocate space to hold the power estimates
for(i in 1:length(SampleSize) ){            ## Loop through the sample sizes
  Power[i] <- power.t.test(                   ## Record the output at the "ith" place in "Power"
    n = SampleSize[i],                          ## Set the sample size of each group
    delta = 0.463,  ## Difference between the two means
    sd = sd(ControlA) ,                         ## Set a common standard deviation
    sig.level = 0.05,                           ## Set a Type I error rate
    power = NULL,                               ## Set the statistical power (1 - Type II error rate)
    type = "two.sample",                        ## Request a T-test comparing means of two groups
    alternative = "one.sided")$power            ## Alternative to null hypothesis is Treatment > Control
}                                           ## Output the element called "power", then re-loop
plot(SampleSize, Power, type = "b")         ## Graph power against sample size


rm(list = ls()) 

