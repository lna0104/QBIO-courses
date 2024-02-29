##Practical 5: Linear models - Analysis of Variance (ANOVA)
#5.1. Simple example of ANOVA
## Enter these made up numbers:
water_loss <- c(1, 2, 3, 4, 3, 4, 5, 6, 5, 6, 7, 8)

## Enter their treatment groups:
wind_speed <- rep(c("Low", "Med", "High"), each = 4)

## cbind together as a data frame
Simple <- cbind.data.frame(water_loss, wind_speed)  
Simple  ## View the data
summary(Simple)## how is R treating each variable?

## Create x-axis locations for the "wind speed" levels
x <- rep(1:3, each = 4)

## Draw a scatter plot of "water_loss" versus "x" without axes
plot(x, Simple$water_loss, xaxt = "n", ylab = "Water loss", xlab = "Wind speed", cex = 1.5)

## Now add a customised axis
axis(side = 1, at = 1:3, labels = c("Low", "Medium", "High"), cex = 2, cex.axis = 1.75)

#Which aspect(s) of the graph tells you that 'wind_speed' predicts 'water_loss'?
#The dots tend to rise as you scan from left to right, with water loss low when wind speed is low and high when wind speed is high.

#Which aspect(s) of the graph tells you that 'wind_speed' is not the only thing that affects 'water_loss'?
#Within each level of 'wind_speed', there is a range (spread) of values of 'water_loss'.

#If we leave it to R, it will order the levels of wind_speed alphabetically as "High", "Low" and "Med" which is not ideal! 
#We can force R to think like us instead!

Simple$wind_speed<-factor(Simple$wind_speed, levels = c("Low", "Med", "High"))
summary(Simple)## Did R obey our command?

fit_simple <- lm(water_loss ~ wind_speed, data=Simple)   ## Fit a linear model
anova(fit_simple)                           ## Generate an ANOVA table

#Which aspect of the ANOVA table tells you that 'wind_speed' is not the only thing that affects 'water_loss'?
#The residual variance (Residual Mean Sq) is not zero.

##5.2. The SAME model summarised using "model coefficients" instead of an "ANOVA table"
model.matrix(fit_simple)## see how R is actually treating the different categories in the fit_simple model
summary(fit_simple)## look at summary of model coefficients and associated t-tests

##5.3. One-way ANOVA - more complex data
PlantGrowth <- read.csv("Data/PlantGrowth.csv", header = TRUE)
install.packages("gplots")
library(gplots)  ## Load the package "gplots"

## Plot mean biomass, with error bars, across different levels of pruning
plotmeans(PlantGrowth$biomass ~ PlantGrowth$pruning, connect = FALSE)
fit <- lm(biomass ~ pruning, data=PlantGrowth) ## Fit a linear model between biomass and pruning
anova(fit) ## Generate an ANOVA table
beta <- fit$coefficients  ## Extract the regression coefficients
beta
model.matrix(fit)
summary(fit)


##5.4.Diagnostics
#As for any regression technique involving F-statistics, the assumptions of normally-distributed residuals and constant residual variance must be verified for your data. 
##5.4.1 Normally-distributed residuals
hist(fit$residuals)    ## Do the residuals look normal?
qqnorm(fit$residuals)  ## Does the qqplot look straight?
qqline(fit$residuals)  ## Add a straight line for comparison.

##5.4.2 Constant residual variance
plot(fit$fitted, fit$residuals)
#Bartlett's statistical test for "homogeneity of variances" gives us an objective guide; 
#if the P-value is small, then the within-group variances are not the same among the groups and we have a problem.
bartlett.test(biomass ~ pruning, data = PlantGrowth)

##5.4.3 Using plot to view diagnostic plots
plot(fit)

##5.4.4 Post hoc multiple comparisons
TukeyHSD(aov(fit))
#This provides tests on all pairwise combinations of our pruning treatments.

##5.5 Two-way ANOVA - now we're getting serious!

fish_data<-read.csv("Data/fish_data.csv")
summary(fish_data)
table(fish_data$reef, fish_data$region)
## re-define the reef variable specifying the order of the levels that we want
fish_data$reef <- factor(fish_data$reef, levels=c("healthy", "bleached"))
fish_data$region <- factor(fish_data$region, levels=c("north", "south"))

library(tidyverse)
ggplot(fish_data, aes(y=shannon_diversity, x=region, fill=reef))+
  geom_boxplot() +
  geom_point(position=position_jitterdodge(jitter.width=0.25), alpha = 0.3)

fish_mod <- lm(shannon_diversity ~ reef + region + reef:region, fish_data)
plot(fish_mod$fitted.values, fish_mod$residuals) ## Plot the residuals against the fitted values to assess constant variance
qqnorm(fish_mod$residuals)   ## Check normality of residuals
qqline(fish_mod$residuals)   ## Plot expected fit for comparison

##5.5.1 Look at the results
anova(fish_mod) ## ANOVA summary first
## we can plot interactions using plotmeans, like this:
plotmeans(shannon_diversity ~ interaction(reef, region), 
          data = fish_data, connect = list(1:2,3:4), ylim = c(1.4, 3.1))
## we tell plotmeans to connect up the first 2 points with a line 
## and the last two points with a line
## these lines represent the slope of the bleaching effect in each region!!!
summary(fish_mod)
## pairwise Tukey test
TukeyHSD(aov(fish_mod))

## install the package first
install.packages("emmeans")
library(emmeans)
emmeans(fish_mod, specs = pairwise ~ reef:region)

##5.6 Do it on your own
seedling_data<-read.csv("Data/seedling_data.csv") ## read in the data
seedling_data
summary(seedling_data)
seedling_data$water<-factor(seedling_data$water, levels=c("watered","ambient"))
seedling_data$light<-factor(seedling_data$light, levels=c("shade","no_shade"))
ggplot(seedling_data, aes(y=biomass, x=water, fill=light))+
  geom_boxplot() +
  geom_point(position=position_jitterdodge(jitter.width=0.25), alpha = 0.3)

seedling_model<-lm(biomass ~ water + light + water:light, seedling_data)

plot(seedling_model$fitted.values, seedling_model$residuals) ## Plot the residuals against the fitted values to assess constant variance
qqnorm(seedling_model$residuals)   ## Check normality of residuals
qqline(seedling_model$residuals)   ## Plot expected fit for comparison

plot(seedling_model$fitted.values, seedling_model$residuals)

anova(seedling_model)
summary(seedling_model)
TukeyHSD(aov(seedling_model))

