### 5. Hypothesis testing for regression
d <- read.csv("data/iKung_HeightWeight.csv")
d2 <- d[d$age>=18,]

m1 <- lm(formula = height ~ weight, data=d2) 
(m1.anova <- anova(m1))

d2 <- d[d$age>=18,c(5,6)] # subset the dataframe to only include the height and weight variables of adults over the age of 18
d2 <- na.omit(d2) # remove the rows of the new dataframe that contain NAs.

m1 <- lm(formula = height ~ weight, data=d2)
m2 <- lm(formula = height ~ 1, data=d2)

anova(m2,m1) # reduced model before full model


### 6. Effect sizes and uncertainty  -----

library(tidyverse)

d <- read_csv("data/iKung_HeightWeight.csv")
head(d) 
tail(d)
str(d)

d2 <- d %>% filter(age>=18) 

m1 <- lm(formula = height ~ weight, data=d2)

#Calculate CIs
alpha <- 0.05
m1$coef[2] - (qt(1-alpha/2,m1$df.residual)) * summary(m1)$coefficients[2,2] # lower
m1$coef[2] + (qt(1-alpha/2,m1$df.residual)) * summary(m1)$coefficients[2,2] # upper

confint(m1)

newdat <- data.frame(weight=seq(min(d2$weight, na.rm = TRUE),max(d2$weight, na.rm = TRUE),length.out=400))
head(newdat)

#predict() command to predict new values of height (according to the fitted model), 
# and to generate confidence intervals, using the argument interval ="c"
predCI <- as.data.frame(predict(m1,newdata = newdat,interval = "c"))
head(predCI)

plot(height~weight,data = d2)
lines(predCI$fit~newdat$weight,lwd=2) # add fitted/predicted line
lines(predCI$lwr~newdat$weight,lty=3,col="red",lwd=2) # add line for lower bound on CI
lines(predCI$upr~newdat$weight,lty=3,col="red",lwd=2) # add line for upper bound on CI

predPI <- as.data.frame(predict(m1,newdata = newdat,interval = "p"))
head(predPI)

plot(height~weight,data = d2)
lines(predCI$fit~newdat$weight,lwd=2) # add fitted/predicted line

# confidence intervals
lines(predCI$lwr~newdat$weight,lty=3,col="red",lwd=2) # add line for lower bound on CI
lines(predCI$upr~newdat$weight,lty=3,col="red",lwd=2) # add line for upper bound on CI

# prediction intervals
lines(predPI$lwr~newdat$weight,lty=3,col="blue",lwd=2) # add line for lower bound on CI
lines(predPI$upr~newdat$weight,lty=3,col="blue",lwd=2) # add line for upper bound on CI



