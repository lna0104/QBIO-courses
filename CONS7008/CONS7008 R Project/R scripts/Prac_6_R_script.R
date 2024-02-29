##Practical 6 Linear models - Multiple regression
#6.1 Choosing variables to include in multiple regressions
Turnstone <- read.csv("Data/Turnstone.csv", header = TRUE)

library(DHARMa) ## load this package for awesome diagnostics
with(Turnstone, pairs(FlockSize ~ Temperature + TimeHighTide + NumPecks, upper.panel=panel.smooth))

fullModel <- lm(FlockSize ~ Temperature * TimeHighTide * NumPecks, data=Turnstone)  ## The full model
plot(simulateResiduals(fullModel))
#check normality assumption
qqnorm(resid(fullModel))
qqline(resid(fullModel))
#check constant variance assumption
plot(fitted(fullModel), resid(fullModel))
## Fit models with different transformations of Y

## sqrt
sqrtFullModel <- lm(sqrt(FlockSize) ~ Temperature * TimeHighTide * NumPecks, data=Turnstone) ## sqrt transform
## look at diagnostics
plot(simulateResiduals(sqrtFullModel))

## log
logFullModel <- lm(log(FlockSize) ~ Temperature * TimeHighTide * NumPecks, data=Turnstone)   ## log transform
## look at diagnostics
plot(simulateResiduals(logFullModel))

##6.3 Interactions in multiple regression

Turnstone$Temperature_by_NumPecks_interaction <- Turnstone$Temperature * Turnstone$NumPecks  ## Add the new column to the data set
head(Turnstone)      ## Check that the new variable has been added
add_model <- lm(log(FlockSize) ~ Temperature + NumPecks + Temperature_by_NumPecks_interaction, data=Turnstone)
anova(add_model)   ## Inspect the ANOVA table
intxn_model <- lm(log(FlockSize) ~ Temperature + NumPecks + Temperature:NumPecks, data=Turnstone)
anova(intxn_model)      ## Inspect the ANOVA table
model.matrix(intxn_model)
summary(intxn_model)

star_model <- lm(log(FlockSize) ~ Temperature * NumPecks, data=Turnstone)
anova(star_model)        ## Display the ANOVA table
summary(star_model)      ## Inspect regression coefficients

#6.3.1 Quickly visualising interactions
## first, lets grab three logical values of Temperature; low, medium and high
(Temps_to_plot<-quantile(Turnstone$Temperature, p = c(0.1, 0.5, 0.9))) ## extract the 10th, 50th and 90th percentiles of Temperature

{
  ## Now make an "empty" plot between NumPecks and log(FlockSize)
  with(Turnstone, plot(log(FlockSize) ~ NumPecks, type = "n"))
  
  ## plot the curve for the low temperature (2.5 deg C)
  ## remember, curve know what 'x' is in the plot above, so we use x here for Numpecks
  curve(cbind(1,Temps_to_plot[1],x, x*Temps_to_plot[1])%*%coef(intxn_model), add = T, col = "skyblue", lwd=3)
  
  ## median temperature (6.5 deg C)
  curve(cbind(1,Temps_to_plot[2],x, x*Temps_to_plot[2])%*%coef(intxn_model), add = T, col = "goldenrod", lwd=3)
  
  ## high temperature (11.3 deg C)
  curve(cbind(1,Temps_to_plot[3],x, x*Temps_to_plot[3])%*%coef(intxn_model), add = T, col = "tomato", lwd=3)
  
  ## add a legend
  legend("topleft", legend=c("2.5 deg C", "6.5 deg C", "11.3 deg C"),
         col=c("skyblue", "goldenrod", "tomato"), lty=1, lwd=2, cex=1)
  
}
{
  with(Turnstone, plot(FlockSize ~ NumPecks, log="y", type = "n"))
  
  ## plot the curve for the low temperature (2.5 deg C)
  curve(exp(cbind(1,Temps_to_plot[1],x, x*Temps_to_plot[1])%*%coef(intxn_model)), add = T, col = "skyblue", lwd=3)
  
  ## median temperature (6.5 deg C)
  curve(exp(cbind(1,Temps_to_plot[2],x, x*Temps_to_plot[2])%*%coef(intxn_model)), add = T, col = "goldenrod", lwd=3)
  
  ## high temperature (11.3 deg C)
  curve(exp(cbind(1,Temps_to_plot[3],x,  x*Temps_to_plot[3])%*%coef(intxn_model)), add = T, col = "tomato", lwd=3)
  
  ## add a legend
  legend("topleft", legend=c("2.5 deg C", "6.5 deg C", "11.3 deg C"),
         col=c("skyblue", "goldenrod", "tomato"), lty=1, lwd=2, cex=1)
}

##6.4 Correlations among predictor variables
fullModel1 <- lm(log(FlockSize) ~ Temperature * TimeHighTide * NumPecks, data=Turnstone)  ## Model 1
fullModel2 <-  lm(log(FlockSize) ~ TimeHighTide * Temperature * NumPecks, data=Turnstone) ## Model 2
fullModel3 <- lm(log(FlockSize) ~ NumPecks * TimeHighTide * Temperature, data=Turnstone) ## Model 3

## Now have a look at the results
anova(fullModel1)   ## Results of Model 1
anova(fullModel2)   ## Results of Model 2
anova(fullModel3)   ## Results of Model 3
#They are for the 3-way interaction, but not for the 2-way interactions. How frustrating!
#Let's explore how to deal with this problem.

##6.5 Diagnostics for a potential collinearity problem among predictor variables
##6.5.1 Scatter plots

with(Turnstone, pairs(log(FlockSize) ~ Temperature + TimeHighTide + NumPecks, panel.lower=NULL, panel = panel.smooth))     ## Produce a matrix of all pairwise scatter plots for the variables of interest
##6.5.2 Correlation coefficient
## create a subset of just the predictors of interest
Turnstone.subset<-Turnstone[,c("NumPecks", "TimeHighTide", "Temperature")]
cor(Turnstone.subset)             ## use the `cor` function from base R to generate the matrix of correlation coefficients
round(cor(Turnstone.subset), 3)   ## use the `round` function to make it easier to inspect by truncating the number of decimal places to 3

##6.5.3 Variance inflation factor (VIF)

## Manually calculate 'vif' for Temperature (NB: R's 'vif' command does all this for you!)  
fit <- lm(Temperature ~ TimeHighTide + NumPecks                 ## All main effects 
          + Temperature:TimeHighTide                          ## All 2-way interactions 
          + Temperature:NumPecks + TimeHighTide:NumPecks  
          + Temperature:TimeHighTide:NumPecks, data=Turnstone)## 3-way interaction 

1/(1 - summary(fit)$r.squared)                            ## Calculate "vif" for the 'Temperature' main effect 

library(car)         ## Load the library that contains the "vif" command
vif(logFullModel)    ## Look at the variance inflation factors
#When  vif>  10, correlation among predictor variables is large, 
#and caution is required when interpreting the parameter estimates in a multiple regression analysis for any response variable.

##6.6 Robust multiple regression and model simplification
##6.6.1 Omnibus tests
no_3_way <- with(Turnstone, update(logFullModel, ~ . - Temperature:TimeHighTide:NumPecks))  ## Refit without the 3-way interaction term
anova(logFullModel, no_3_way)                                              ## Compare the two models
## Refit the updated model ("no_3_way"), but exclude the 2-way interactions
mains_only <- with(Turnstone, update(no_3_way, ~ . - Temperature:TimeHighTide - Temperature:NumPecks - TimeHighTide:NumPecks))
anova(no_3_way, mains_only) ## Compare the two models

##6.6.2 Stepwise regression
#Stepwise regression is a cyclic process of removing terms one-by-one, reassessing the fit of the model, 
#re-adding terms that were deleted at a previous step, and repeating
stepModel <- step(logFullModel, direction = "both")
#step uses the Akaike information criterion (AIC) to decide if a model is a better (smaller AIC) or worse (bigger AIC) fit to the data.
anova(stepModel) 

##6.6.3 Type II sum-of-squares
Anova(logFullModel, Type = "II")
## compare to Type I (typical lm() summary)
summary(logFullModel)


rm(list = ls()) 

