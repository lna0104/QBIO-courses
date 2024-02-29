rm(list=ls())

## I hope this works for everyone now!
library(DHARMa)

## define some colours
cols_to_use<-c("skyblue", "goldenrod", "salmon")

#################
### EXAMPLE 1 ###
#################
data1<-read.csv("Data/int_example_1.csv")
pairs(data1)

## notice how bird_diversity is negatively related to fragmentation (a measure of how fragmented the habitat is) and lantana_cover (Lantana is a bad environmental weed)

## but fragmentation and lantana_cover are not at all related to each other!!!

## this means that fragmentation and lantana_cover MUST explain different parts of the variation in bird diversity!!!

## we can test this in a few steps

## (1) first fit a simple regression of bird_diversity ~ fragmentation
m1<-lm(bird_diversity ~ fragmentation, data1)
## plot this to see what we have
with(data1, plot(bird_diversity ~ fragmentation))
abline(lm(bird_diversity ~ fragmentation, data1), col="red", lwd=2)

##So the red line is what fragmentation explains about bird_diversity, but look at all of the scatter!!!

## we can look at the scatter as the residuals, like this
arrows(data1$fragmentation, data1$bird_diversity, data1$fragmentation, fitted(m1), length=0, col = "cyan")

## (2) grab the scatter by extracting the residuals
## I have called this "bird_diversity_not_explained_by_fragmentation"
bird_diversity_not_explained_by_fragmentation<- residuals(m1)


## (3) look at the relationship between "bird_diversity_not_explained_by_fragmentation" and lantana_cover
plot(bird_diversity_not_explained_by_fragmentation ~ data1$lantana_cover)

## so lantana_cover clearly explains variation in bird diversity that is not explained by fragmentation!!!

     
## There is actually more going on in this dataset!
## I embedded an interaction between fragmentation and lantana_cover...

## fit model with fragmentation, lantana_cover and their two-way interaction
m2<-lm(bird_diversity ~ fragmentation + lantana_cover + fragmentation:lantana_cover, data1)

## diagnostics
plot(simulateResiduals(m2))## looks great

## look at summary
summary(m2)

## We visualise this to understand what's going on
################
### OPTION 1 ###
################
## In 2 dimensions
## plot the slopes of bird_diversity ~ fragmentation for different values of lantana_cover
## First make a categorical version of lantana_cover
## this is JUST FOR PLOTTING
lantana_cover_cut<-cut(data1$lantana_cover, breaks = 3, labels = c("low", "med", "high"))

## Next choose values of lantana_cover to plot the curves for
## here I am just grabbing low, medium and high values of lantana_cover
lantana_cover_vals_to_plot<-quantile(data1$lantana_cover, p = c(0.17, 0.5, 0.83))

## make the plot
with(data1, plot(bird_diversity~fragmentation,col=cols_to_use[lantana_cover_cut], pch=16))

## line for low lantana_cover
curve(cbind(1,x,lantana_cover_vals_to_plot[1], x*lantana_cover_vals_to_plot[1])%*%coef(m2), add=T, col=cols_to_use[1], lwd=3)

## line for med lantana_cover
curve(cbind(1,x,lantana_cover_vals_to_plot[2], x*lantana_cover_vals_to_plot[2])%*%coef(m2), add=T, col=cols_to_use[2], lwd=3)

## line for high lantana_cover
curve(cbind(1,x,lantana_cover_vals_to_plot[3], x*lantana_cover_vals_to_plot[3])%*%coef(m2), add=T, col=cols_to_use[3], lwd=3)

## add a legend
legend("bottomleft", col=cols_to_use, lwd=2, legend = c("low lantana_cover", "medium lantana_cover", "high lantana_cover"))




## plots in 3 dimensions
plot_func<-function(fragmentation,lantana_cover) (cbind(1,fragmentation,lantana_cover,fragmentation*lantana_cover)%*%coef(m2))

##perspective plot
plot_fragmentation<- seq(min(data1$fragmentation), max(data1$fragmentation), length.out=15)
plot_lantana_cover<- seq(min(data1$lantana_cover), max(data1$lantana_cover), length.out=15)
plot_bird_diversity<-outer(X = plot_fragmentation, Y=plot_lantana_cover,  FUN=plot_func) 
persp(y = plot_lantana_cover, x = plot_fragmentation, z=plot_bird_diversity, 
      theta = 130, phi = 30, col="goldenrod")


## plot a surface using 'image'
## first, define a function to get fitted values for surfaces
plot_fragmentation<- seq(min(data1$fragmentation), max(data1$fragmentation), length.out=100)
plot_lantana_cover<- seq(min(data1$lantana_cover), max(data1$lantana_cover), length.out=100)
plot_bird_diversity<-outer(X = plot_fragmentation, Y=plot_lantana_cover,  FUN=plot_func) 
image(y = plot_lantana_cover, x = plot_fragmentation, z=plot_bird_diversity, col = colorRampPalette(cols_to_use)(100), cex.lab=1.25, cex.axis=1, useRaster=T, 
      xlab="Fragmentation", ylab="Lantana cover (%)")
contour(y = plot_lantana_cover, x = plot_fragmentation, z=plot_bird_diversity, add=T)

## show where the lines from our first plot cross the surface

## look at our chosen lantana values again (we defined these above)
lantana_cover_vals_to_plot

abline(h=lantana_cover_vals_to_plot[1], lty=2)
abline(h=lantana_cover_vals_to_plot[2], lty=3)
abline(h=lantana_cover_vals_to_plot[3], lty=4)





#################
### EXAMPLE 2 ###
#################
data2<-read.csv("Data/int_example_2.csv")
pairs(data2)

## now we are looking at the possible effect of patch size and rainfall on bird diversity


m3<-lm(bird_diversity ~ patch_size + rainfall + patch_size:rainfall, data2)
## diagnostics
plot(simulateResiduals(m3))

## look at summary
summary(m3)


## using curve in 2 dimensions
## make a categorical version of rainfall
rainfall_cut<-cut(data2$rainfall, breaks = 3, labels = c("low", "med", "high"))
## choose values of rainfall to plot the curves for
rainfall_vals_to_plot<-quantile(data2$rainfall, p = c(0.17, 0.5, 0.83))
## make the plot
with(data2, plot(bird_diversity~patch_size,col=cols_to_use[rainfall_cut], pch=16))
## line for low
curve(cbind(1,x,rainfall_vals_to_plot[1], x*rainfall_vals_to_plot[1])%*%coef(m3), add=T, col=cols_to_use[1], lwd=3)
## line for med
curve(cbind(1,x,rainfall_vals_to_plot[2], x*rainfall_vals_to_plot[2])%*%coef(m3), add=T, col=cols_to_use[2], lwd=3)
## line for high
curve(cbind(1,x,rainfall_vals_to_plot[3], x*rainfall_vals_to_plot[3])%*%coef(m3), add=T, col=cols_to_use[3], lwd=3)
## legend
legend("topleft", col=cols_to_use, lwd=2, legend = c("low rainfall", "medium rainfall", "high rainfall"))


## plots of 3 dimensions
plot_func<-function(patch_size,rainfall) (cbind(1,patch_size,rainfall,patch_size*rainfall)%*%coef(m3))

##perspective plot
plot_patch_size<- seq(min(data2$patch_size), max(data2$patch_size), length.out=15)
plot_rainfall<- seq(min(data2$rainfall), max(data2$rainfall), length.out=15)
plot_bird_diversity<-outer(X = plot_patch_size, Y=plot_rainfall,  FUN=plot_func) 
persp(y = plot_rainfall, x = plot_patch_size, z=plot_bird_diversity, 
      theta = -50, phi = 30, col="goldenrod")

## surface
plot_patch_size<- seq(min(data2$patch_size), max(data2$patch_size), length.out=100)
plot_rainfall<- seq(min(data2$rainfall), max(data2$rainfall), length.out=100)
plot_bird_diversity<-outer(X = plot_patch_size, Y=plot_rainfall,  FUN=plot_func) 
image(y = plot_rainfall, x = plot_patch_size, z=plot_bird_diversity, col = colorRampPalette(cols_to_use)(100), cex.lab=1.25, cex.axis=1, useRaster=T, 
      xlab="patch_size", ylab="rainfall")
contour(y = plot_rainfall, x = plot_patch_size, z=plot_bird_diversity, add=T)

abline(h=rainfall_vals_to_plot[1], lty=2)
abline(h=rainfall_vals_to_plot[2], lty=3)
abline(h=rainfall_vals_to_plot[3], lty=4)







#################
### EXAMPLE 3 ###
#################
## This example looks at the effects of temperature and rainfall on bird diversity
data3a<-read.csv("Data/int_example_3a.csv")
pairs(data3a)

## oh dear!!!

m_silly1<-lm(bird_diversity ~ temperature + rainfall, data3a)
anova(m_silly1)
summary(m_silly1)

m_silly2<-lm(bird_diversity ~ rainfall + temperature, data3a)
anova(m_silly2)
summary(m_silly2)




## Here is a scenario where temperature and rainfall are NOT correlated
data3b<-read.csv("Data/int_example_3b.csv")
pairs(data3b)
m4<-lm(bird_diversity ~ temperature + rainfall + temperature:rainfall, data3b)
## diagnostics
plot(simulateResiduals(m4))

## look at summary
summary(m4)
## diagnostics

## using curve in 2 dimensions
## make a categorical version of rainfall
rainfall_cut<-cut(data3b$rainfall, breaks = 3, labels = c("low", "med", "high"))
## choose values of rainfall to plot the curves for
rainfall_vals_to_plot<-quantile(data3b$rainfall, p = c(0.10, 0.5, 0.90))
## make the plot
with(data3b, plot(bird_diversity~temperature,col=cols_to_use[rainfall_cut], pch=16))
## line for low
curve(cbind(1,x,rainfall_vals_to_plot[1], x*rainfall_vals_to_plot[1])%*%coef(m4), add=T, col=cols_to_use[1], lwd=3)
## line for med
curve(cbind(1,x,rainfall_vals_to_plot[2], x*rainfall_vals_to_plot[2])%*%coef(m4), add=T, col=cols_to_use[2], lwd=3)
## line for high
curve(cbind(1,x,rainfall_vals_to_plot[3], x*rainfall_vals_to_plot[3])%*%coef(m4), add=T, col=cols_to_use[3], lwd=3)
## legend
legend("topleft", col=cols_to_use, lwd=2, legend = c("low rainfall", "medium rainfall", "high rainfall"))


## plots of 3 dimensions - surface and perspective plot
plot_func<-function(temperature,rainfall) (cbind(1,temperature,rainfall,temperature*rainfall)%*%coef(m4))

##perspective plot
plot_temperature<- seq(min(data3b$temperature), max(data3b$temperature), length.out=15)
plot_rainfall<- seq(min(data3b$rainfall), max(data3b$rainfall), length.out=15)
plot_bird_diversity<-outer(X = plot_temperature, Y=plot_rainfall,  FUN=plot_func) 
persp(y = plot_rainfall, x = plot_temperature, z=plot_bird_diversity, 
      theta = 40, phi = 30, col="goldenrod")

## surface
plot_temperature<- seq(min(data3b$temperature), max(data3b$temperature), length.out=100)
plot_rainfall<- seq(min(data3b$rainfall), max(data3b$rainfall), length.out=100)
plot_bird_diversity<-outer(X = plot_temperature, Y=plot_rainfall,  FUN=plot_func) 
image(y = plot_rainfall, x = plot_temperature, z=plot_bird_diversity, col = colorRampPalette(cols_to_use)(100), cex.lab=1.25, cex.axis=1, useRaster=T, 
      xlab="temperature", ylab="rainfall")
contour(y = plot_rainfall, x = plot_temperature, z=plot_bird_diversity, add=T)

abline(h=rainfall_vals_to_plot[1], lty=2)
abline(h=rainfall_vals_to_plot[2], lty=3)
abline(h=rainfall_vals_to_plot[3], lty=4)



