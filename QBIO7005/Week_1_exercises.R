#1. Simulate data -----

x <- seq(0, 20, length.out = 20)
y <- rnorm(length(x), 5, 15)
d1 <- cbind.data.frame(x,y)

#2. ------

plot(y~x)
m1 <- lm(y~x, data = d1)
summary(m1)

curve(m1$coefficients[1] + m1$coefficients[2]*x,add=T,col="black",lwd=10)
lines(m1$fitted.values~x,col="red",lwd=5)
abline(a=m1$coefficients[1],b=m1$coefficients[2],col="blue",lwd=2)
lines(predict(m1)~x,col="orange",lwd=1,lty=3)

par(mfrow=c(1,2))
plot(m1$residuals~m1$fitted, main="residual plot")
abline(h=0)
qqnorm(m1$residuals, main = "qq-plot")
qqline(m1$residuals)

#3.a. small vs. large variance -----
x <- seq(0,20,length.out=100)
B0 <- 5
B1 <- 2.7
y1 <- rnorm(length(x),B0+B1*x,5) # sd = 5
y2 <- rnorm(length(x),B0+B1*x,30) # sd = 30
d2 <- cbind.data.frame(x,y1,y2)

m2 <- lm(y1~x,data=d2)
summary(m2)

m3 <- lm(y2~x,data=d2)
summary(m3)

par(mfrow=c(2,2))
plot(y1~x,ylim=c(-20,100))
curve(m2$coefficients[1]+m2$coefficients[2]*x,col="red",lwd=3,add=T)
plot(m2$residuals~m2$fitted)
abline(h=0)
plot(y2~x,ylim=c(-20,100))
curve(m3$coefficients[1]+m3$coefficients[2]*x,col="red",lwd=3,add=T)
plot(m3$residuals~m3$fitted)
abline(h=0)

#3b. small vs. large sample size

x1 <- seq(0,20,length.out=5) # small sample size
B0 <- 5
B1 <- 2.7
y1 <- rnorm(length(x1),B0+B1*x1,15)
x2 <- seq(0,20,length.out=100) # large sample size
y2 <- rnorm(length(x2),B0+B1*x2,15)
d3 <- cbind.data.frame(x1,y1,x2,y2)

m4 <- lm(y1~x1,data=d3)
summary(m4)

m5 <- lm(y2~x2,data=d3)
summary(m5)

par(mfrow=c(2,2))
plot(y1~x1,ylim=c(-20,100))
curve(m4$coefficients[1]+m4$coefficients[2]*x,col="red",lwd=3,add=T)
plot(m4$residuals~m4$fitted)
abline(h=0)
plot(y2~x2,ylim=c(-20,100))
curve(m5$coefficients[1]+m5$coefficients[2]*x,col="red",lwd=3,add=T)
plot(m5$residuals~m5$fitted)
abline(h=0)


#3c. subset of the range of x-values vs. full range of x-values
