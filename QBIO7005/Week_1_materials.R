curve(pnorm(x),-3,3)
arrows(-1,0,-1,pnorm(-1),col="red")
arrows(-1,pnorm(-1),-3,pnorm(-1),col="green")

#If you take repeated samples from a population with finite variance and calculate their averages, 
#then the averages will be normally distributed. 
#This is called the central limit theorem.

hist(runif(10000)*10,main="")

means<-numeric(10000)

for (i in 1:10000){
  means[i]<-mean(runif(5)*10)
}
hist(means,ylim=c(0,1600))
mean(means)
sd(means)

#To create a normal curve with our particular mean and standard deviation.
#A good rule of thumb is that for a smooth curve you need at least 100 values, so let’s try this:
xv<-seq(0,10,0.1)
yv<-dnorm(xv,mean=4.998581,sd=1.28996)*5000 # mutiple number depends on bin width - here 0.5
lines(xv,yv)


#3.2 A Gaussian model of height -----
library(tidyverse)
d<-read_csv("Desktop/Courses/QBIO7005/data/iKung_HeightWeight.csv") 
head(d) 
tail(d)
str(d)

d2 <- d %>% filter(age>=18)
dim(d2)
hist(d2$height,breaks = 20,freq=F,ylim=c(0,0.1),main="Histogram of Adult Height",xlab="height (cm)")

meanGuess <- 155
sdGuess <-10
curve((1/(sdGuess * sqrt(2 * pi))) * exp(-0.5 * ((x - meanGuess) / sdGuess)^2), 
      add = TRUE, col = "red", lwd = 2)
#By coding this expression as the first argument, expr=, of the curve() command, 
#leaving x as x in this equation as the variable that will represent individual measurements of adult height

meanHeight <- mean(d2$height, na.rm = T)
sdHeight <- sd(d2$height, na.rm = T)

#3.4 Fitting the model in R -----
# Using Ordinary Least Squares estimate
m1 <- lm(height~1,data=d2) # this is the R syntax for fitting a linear model with Gaussian errors. The syntax "height~1", and without the addition of predictor variables, means that we are fitting an 'intercept-only' or constants-only model. 
summary(m1) # this provides a summary of the model fit, including parameter estimates. 
str(summary(m1))
m1Summary = summary(m1)
m1Summary$coefficients
m1Summary$sigma

# Using maximum likelihood estimation
{
  normalF <- function(parvec) {
    # Log of likelihood of a normal distribution
    # parvec[1] - mean
    # parvec[2] - standard deviation
    # x - set of observations. Should be initialized before MLE
    sum ( -0.5* log(parvec[2]) - 0.5*(x - parvec[1])^2/parvec[2] )
  }
  
  x = c(1,2,3,4) # set of observations
  normalF(c(1,1)) # log likelihood function value for given x and mu=sd=1 
}

library(bbmle)
x <- na.omit(d2$height)
m <- mle2(x~dnorm(mean=mu,sd=sd),start=list(mu=145,sd=6),data=data.frame(x))
m
confint(m)

#4 Extending the linear model (part 1) ------

#4.1 Adding a continuous predictor variable ------
plot(d2$weight, d2$height, ylab = "Adult height (cm)", xlab = "Adult weight (kg)")

m1 <- lm(formula = height ~ weight, data=d2) # assign a linear model (lm) fit to the object 'm1'. We are modeling height as a function of weight. The `lm` command assumes a normally distributed data (i.e. that a normal likelihood is justified)
m1.sum <- summary(m1)

plot(d2$weight, d2$height, ylab = "Adult height (cm)", xlab = "Adult weight (kg)")
abline(lm(height ~ weight, data = d2), col = "red")

str(m1)
plot(m1$residuals ~ m1$fitted.values)
abline(h=0)

coef(m1)

newdat <- as.data.frame(59) # i.e. I would like to know the predicted height for an individual who weighs 59kg.
names(newdat) <- "weight"
predict(m1,newdata=newdat)

predict(m1,newdata=newdat,interval="p")
