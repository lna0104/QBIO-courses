library(tidyverse)
library(ggplot2)
library(GGally)

#Load data
cocoaDat <- read_csv("data/cocoa_data_02.csv")
head(cocoaDat)
summary(cocoaDat)

#Explore the data
pairs(cocoaDat[,c(2,5,6,10)])

hist(cocoaDat[["yield"]])
hist(cocoaDat[["aboveground_carbon"]])
hist(cocoaDat[["max_temperature"]])

str(cocoaDat)

boxplot(cocoaDat$max_temperature)

#Define the response and predictor variables

X <- cocoaDat[["shade_tree_cover"]]
y <- cocoaDat[["yield"]]


#Model
#yield ~ shade_tree_cover
m1 <- lm(yield ~ shade_tree_cover, data = cocoaDat) # this is the R syntax for fitting a linear model with Gaussian errors. The syntax "height~1", and without the addition of predictor variables, means that we are fitting an 'intercept-only' or constants-only model. 

par(mfrow=c(1,1))
plot(yield ~ shade_tree_cover, data = cocoaDat, xlab = "Shade tree cover", ylab = "Yield")
abline(m1, col = "red")

#Assess assumptions
par(mfrow=c(1,2))
plot(m1$residuals~m1$fitted, main="residual plot")
abline(h=0)
qqnorm(m1$residuals, main = "qq-plot")
qqline(m1$residuals)

par(mfrow=c(2,2))
plot(m1)

summary(m1)
confint(m1)

m2 <- lm(yield ~ poly(shade_tree_cover, degree = 2), data = cocoaDat)
par(mfrow=c(2,2))
plot(m2)

newdat <- data.frame(shade_tree_cover=seq(min(cocoaDat$shade_tree_cover, na.rm = TRUE),max(cocoaDat$shade_tree_cover, na.rm = TRUE),length.out=50))
head(newdat)
predCI <- as.data.frame(predict(m2,newdata = newdat,interval = "c"))
head(predCI)

par(mfrow=c(1,1))
plot(yield ~ shade_tree_cover, data = cocoaDat)
lines(predCI$fit~newdat$shade_tree_cover,lwd=2) # add fitted/predicted line
lines(predCI$lwr~newdat$shade_tree_cover,lty=3,col="red",lwd=2) # add line for lower bound on CI
lines(predCI$upr~newdat$shade_tree_cover,lty=3,col="red",lwd=2) # add line for upper bound on CI
