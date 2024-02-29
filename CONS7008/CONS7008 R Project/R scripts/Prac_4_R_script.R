##Practical 4: linear models - linear regression
##4.1.Starting out nice and simple
Y <- c(-3, -2, -2, -1, -1, -1, -1, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3)
X <- c(-2, -1.5, -1, -2, -1.5, -0.5, 0, -1, -0.5, 0.5, 1, 0, 0.5, 1.5, 2, 1, 1.5, 2)

plot(X, Y)

## Let's customize that graph a little! 
par(                           ## Customize the graphical window 
  mar = c(5, 8, 4, 2),   ## Set margin sizes (in lines) 
  las = 1,                ## Print axis tick labels horizontally 
  xpd = TRUE)               ## Allow printing outside the graph region 

plot(X, Y,                  ## Generate a scatter plot of 'Y' versus 'X' 
     bty = "n",               ## Do not draw a box around the graph 
     font = 2,                ## Use 'bold face' font within the graph 
     cex = 2,                ## Double the size of all characters within the graph 
     pch = 16,                ## Make the plot symbol a black filled circle 
     cex.lab = 3)         ## Triple the size of the axis titles 
##4.2 Fit a regression line
Model1 <- lm(Y ~ X)  ## Regress Y against X and store the result in an object called "Model1"
plot(Y ~ X)          ## We can also use ~  (called a "tilde")  for plotting in the same way! Draw a plot where Y depends on X.
curve(cbind(1,x)%*%coef(Model1), add =T, col = "red", lwd=2) ## Use the coefficients 
## (intercept and slope) from Model1 to draw the regression line through the data

##4.2.1 Understanding and using lm outputs
Model1               ## Here are the results - the two parameters that define the line!
names(Model1)        ## List the elements of Model1
str(Model1)          ## List the elements of Model1 AND their attributes (this is the 'structure' of Model1)
Model1$coefficients  ## Look at the contents of "coefficients"
Model1$residuals     ## Look at the contents of "residuals"
residuals(Model1)    ## Another way of looking at the residuals
Model1[8]            ## Look at the 8th element
Model1$fitted.values ## Look at the contents of "fitted.values"
round(Model1$fitted.values, 1)    ## Round those numbers off to one decimal place
model.matrix(Model1)

##4.3 Calculating F-statistics and their degrees of freedom
#In regression, the null hypothesis is: "the value of Y does not depend on the value of X", 
#which is equivalent to: "the true slope of the line is zero". 

anova(Model1)   ## Generate an ANOVA table
#Calculate RegressionSS and ResidualSS 
## calculate the deviations of the fitted values from the mean of Y
## square these and then sum them
RegressionSS <- sum((Model1$fitted.values - mean(Y))^2)
RegressionSS ## View the residual sum of squares

## Square the residuals and then sum the squares
ResidualSS <- sum(Model1$residuals^2)
ResidualSS ## View the residual sum of squares

(RegressionMS <- RegressionSS/1)
(ResdualMS <- ResidualSS/(18-2))

(Fratio <- RegressionMS/ResdualMS) ## Calculate the F-statistic
1 - pf(Fratio, 1, 16)    ## Calculate the P-values for the statistic, based on the F-distribution, the F-ratio, and the numerator (regression) and denominator (residual) degrees of freedom

summary(Model1)    ## Display T-tests for the regression coefficients


##4.5.Analysing the Turnstone data
Turnstone <- read.csv("Data/Turnstone.csv", header = TRUE)
plot(FlockSize ~ TimeHighTide, data=Turnstone)        ## Simple scatter plot
fit <- lm(FlockSize ~ TimeHighTide, data=Turnstone)   ## Fit a linear model (regression line) to the data and store the results in the R object "fit"
curve(cbind(1,x)%*%coef(fit), add = T, col = "red", lwd=2)  ## Draw the regression line, calculated from the model fit parameters, on your scatterplot
summary(fit)
anova(fit) 

##4.6 Diagnostics

##4.6.1 Normally distributed residuals
#qqnorm generates a plot where the residuals are plotted against values they would have had if they were perfectly NORMAL.
qqnorm(fit$residuals,  ## Check if the regression residuals are normally distributed
       main = "Residuals of regression of flock size on high tide time")        
qqline(fit$residuals)  ## Add the 1:1 line for visualisation     

qqnorm(Turnstone$FlockSize, main = "Flock size")   ## Check if Y is normally distributed
qqline(Turnstone$FlockSize)   ## This just adds a 1:1 line to make it easier to visualise if the observed (y-axis) values correspond to the predicted (x-axis) values

qqnorm(log(Turnstone$FlockSize))                ##log transform and plot against expected values of normal distribution
qqline(log(Turnstone$FlockSize))                ## add 1:1 line to plot
plot(log(FlockSize) ~ TimeHighTide, data=Turnstone)   ## Simple scatter plot of our hypothesis

fit2 <- lm(log(FlockSize) ~ TimeHighTide, data=Turnstone)

qqnorm(fit2$residuals)
qqline(fit2$residuals)

##4.6.2 Homogeneity of variance

## Check that residuals and the fitted values are independent, and that the residual variance is constant across fitted values
plot(fit2$fitted.values, fit2$residuals)
plot(log(Turnstone$FlockSize) ~ Turnstone$TimeHighTide)
curve(cbind(1,x)%*%coef(fit2), add = T, col = "red", lwd=2)  ## fitted line
anova(fit2)
summary(fit2)

##4.7. Do it on your own
head(iris)
str(iris)
iris$Species<-as.factor(iris$Species)
summary(iris)

iris_model<-lm(iris$Petal.Length~iris$Sepal.Length)
summary(iris_model)
with(iris, plot(Petal.Length~Sepal.Length, col="grey", pch=16))
#Add a line 
abline(coef(iris_model), col="saddlebrown", lwd=2)
curve(cbind(1,x)%*%coef(iris_model), add = T, col="saddlebrown", lwd=3)



###############################
###MUTIPLE LINEAR REGRESSION###
###############################


iris_model2<-lm(Petal.Length ~ Sepal.Length + Sepal.Width,  data = iris)
summary(iris_model2)
coef(iris_model2)
with(iris, plot(Petal.Length ~ Sepal.Width, col="grey", pch=16))

curve(cbind(1, mean(iris$Sepal.Length), x) %*% coef(iris_model2), add = T, col="red", lwd=3)

########################################################
###COMPLEX MUTIPLE LINEAR REGRESSION WITH INTERACTION###
########################################################

iris_model3<-lm(Petal.Length ~ Sepal.Width + Sepal.Length + Species +  Sepal.Width:Species,  data = iris)
#We expect that the relationship bw Sepal.Width and Petal.Length depends on/shifts with Species
#A specification of the form first:second indicates the set of terms obtained by taking the interactions of all terms in first with all terms in second. 
#The specification first*second indicates the cross of first and second. This is the same as first + second + first:second.
summary(iris_model3)
#When coming to factor variable, R treats factors alphabetically and setosa Species come first 
#=> intercept for setosa Species, Sepal.Width is the slope for setosa. Sepal.Length isn't be affected by Species 

patch_cols<-c("tomato","goldenrod","dodgerblue4")
with(iris, plot(Petal.Length ~ Sepal.Width, col=patch_cols[Species], pch=16))

#line for setosa
curve(cbind(1,x,mean(iris$Sepal.Length),0,0,x*0,x*0)%*% coef(iris_model3), add = T, col=patch_cols[1], lwd=3)

#line for versicolor   
curve(cbind(1,x,mean(iris$Sepal.Length), 1,0,x*1,x*0)%*% coef(iris_model3), add = T, col=patch_cols[2], lwd=3)

#line for virginica  

curve(cbind(1,x,mean(iris$Sepal.Length), 0,1,x*0,x*1)%*% coef(iris_model3), add = T, col=patch_cols[3], lwd=3)


rm(list = ls()) 
