## simulate some data
##n<-300
##predictor = rnorm(n, 20, 5)
##my_data<-data.frame(predictor, response = (1 + 0.5*predictor + rnorm(n, 0, 2)))
##write to file
##write.csv(my_data, "Data/my_data.csv", row.names=F)

library(tidyverse)##install.packages(("DHARMa"))
library(DHARMa)

my_data<-read_csv("Data/my_data.csv")
head(my_data)

## plot the relationship
with(my_data, plot(response ~ predictor, pch=16, col="grey"))

## fit a linear regression
m1<-lm(response ~ predictor, data = my_data)

## before we get carried away looking for significant p values (I know you want them!!!)

## have we met assumptions?
## (1) normally distributed residuals
## (2) constant variance (AKA homogeneity of variance)
## (3) observations are independent (we can't test this one unless we know how the data was collected)

## we can look at a simple histogram of the residuals
## this will show us if the residuals are normally distributed or not
hist(resid(m1))
abline(v=0, col="red", lwd=3)

## alternatively we can look at a "Quantile-Quantile Plot" of the residuals
qqnorm(resid(m1))
qqline(resid(m1), col="red", lwd=3)

## This is how we check for constant variance
plot(fitted(m1), resid(m1))
## remember, the fitted values are the values along the fitted line
## its important that we DO NOT see a pattern in these plots
## if we do, it means that the spread of the residuals changes depending on the fitted value
## in other words, the spread of the residuals is different depending on where we look along the fitted line (bad news)

model.matrix(m1)

## In this course we will use the wonderfully flexible functions from the DHARMa package (different spelling to Jeffrey)
## these can be used on all types of models, not just those that assume normally distributed residuals
plot(simulateResiduals(m1))


## so it looks like we're ok to proceed with INFERENCE!!!
summary(m1)
## we have a significant positive relationship between the response and the predictor


## let's learn more about the model by using the curve() function
with(my_data, plot(response ~ predictor, pch=16, col="grey"))
abline(coef(m1), col="seagreen", lwd=3)

with(my_data, plot(response ~ predictor, pch=16, col="grey"))
curve(cbind(1,x)%*%coef(m1), col="salmon", add=T, lwd=3)
points(fitted(m1) ~ my_data$predictor, col="dodgerblue4")


## what the hell is going on with cbind, %*% and coef?
coef(m1)






## why transform???
n<-100
x<-rnorm(n, 20, 5)
y<- exp(1 + 0.5*x + rnorm(n, 0, 2))






plot(y~x, pch=16, col="grey")

m1<-lm(y~x)
curve(cbind(1,x)%*%coef(m1), col="salmon", add=T, lwd=3)
hist(resid(m1))
plot(fitted(m1), resid(m1))
qqnorm(resid(m1))
qqline(resid(m1))

m2<-lm(log(y)~x)
par(mfrow=c(1,2))

hist(x)
plot(y~x, log="y", pch=16, col="grey")
curve(exp(cbind(1,x)%*%coef(m2)), col="salmon", add=T, lwd=3)

plot(y~x, pch=16, col="grey")
curve(exp(cbind(1,x)%*%coef(m2)), col="salmon", add=T, lwd=3)


qqnorm(resid(m2))
qqline(resid(m2))
hist(resid(m2))
plot(fitted(m2), resid(m2))








## Going back to m1, we can use R to calculate the regression manually (not that we would ever do this in real life)
## calculate slope 
beta<-with(my_data, sum((response-mean(response))*(predictor-mean(predictor))) / sum((predictor-mean(predictor))*(predictor-mean(predictor))))

## calculate intercept (using the slope)
alpha<-with(my_data, mean(response) - beta*mean(predictor))

#Residual Standard error (Like Standard Deviation)
k=length(m1$coef)-1 #Subtract one to ignore intercept (once we calculate the slope, we know the intercept)
SSE=sum(m1$resid^2)
n=length(m1$resid)
sqrt(SSE/(n-(1+k))) #Residual Standard Error


MS_resid<-sum(m1$resid^2) / (length(my_data$response)-2)
SSpredictor<-sum((my_data$predictor-mean(my_data$predictor))^2)

SE_beta<-sqrt(MS_resid/SSpredictor)
t_val_beta<-beta/SE_beta

## the p value for a TWO-TAILED test is:
2 * pt(q = abs(t_val_beta), df = n-2, lower=FALSE)

MS_regression<-sum((m1$fitted - mean(my_data$response))^2)

## r2 value = 1-(SSE/SST)
SSE<-sum(m1$resid^2)
SST<-sum((my_data$response - mean(my_data$response))^2)

##r2
1-(SSE/SST)

## alternatively
sum((m1$fitted - mean(m1$fitted))^2) / SST

