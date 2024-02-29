##Practical 7 Linear mixed-effects models (LLMs)
##7.1 Simple nested data
dunnartData<-read.csv("Data/dunnartData.csv")
dunnartData$site<-as.factor(dunnartData$site) # make R treat site as a factor instead of a character
## visually explore the data
pairs(dunnartData[,c("bodyMass", "cover", "length", "site")])
## what can we say just from looking at the pairs plot?

## are dunnarts similarly sized in the different areas?
with(dunnartData, boxplot(bodyMass ~ site))

## are sample sizes similar across sites?
table(dunnartData$site)

## fit a linear model
dunnartCompPool<- lm(bodyMass ~ cover + length, data = dunnartData)

## let's look at some plot diagnostics using DHARMa
library(DHARMa)
plot(simulateResiduals(dunnartCompPool))

## if nothing looks too bad (which it doesn't), we can check the model summary
summary(dunnartCompPool)

## now hold on, let's go back and check those residuals again, this time looking
## for whether they're similar at each site
boxplot(residuals(dunnartCompPool) ~ dunnartData$site)

##Adding sites as a predictor in our model
## fit a linear model
dunnartNoPool <- lm(bodyMass ~ cover + length + site, data = dunnartData)

## does this fix our issue with residuals being linked to site
boxplot(residuals(dunnartNoPool) ~ dunnartData$site)

## check the summary
summary(dunnartNoPool)

##Linear mixed-effect model
## install some new and exciting packages!!!
install.packages('lme4', 'lmerTest', 'performance')
## load these (and DHARMa)
library(lme4) # for mixed-effects models
library(lmerTest) # to get p-values
library(DHARMa) # flexible model diagnostics
library(performance) # a good package to measure model performance

## Mixed-effects model structure
dunnartPartPool <- lmer(bodyMass ~ cover + length + (1|site), data = dunnartData)

## diagnostics using the 'simulateResiduals' function in DHARMa
plot(simulateResiduals(dunnartPartPool))

## NOW! The mixed-effects model summary
summary(dunnartPartPool)

## we can get the "random" intercepts for each site like this
coef(dunnartPartPool)

## we can also estimate how much variance is explained by the fixed effects ("marginal R2") and fixed + random effects ("conditional R2")
r2(dunnartPartPool)


pdf("Outputs/dunnartPopPreds.pdf", width=12) #this sets up a blank .pdf file in the ???Outputs??? folder called ???dunnartPopPreds.pdf???
# The plot you make will be ???projected??? onto this blank PDF

par(mfrow=c(1,2)) # we want two side-by-side figure panels

## plot the raw data as ugly open circles
with(dunnartData, plot(bodyMass ~ cover))

## add the regression slope
## (we need to use fixef() to get the "fixed effects"), coef() doesn't give us the overall intercept
curve(cbind(1,x,mean(dunnartData$length))%*%fixef(dunnartPartPool),
      from=min(dunnartData$cover), to=max(dunnartData$cover), add=TRUE,
      lwd=2, col="goldenrod") 

with(dunnartData, plot(bodyMass ~ length))

curve(cbind(1,mean(dunnartData$cover),x)%*%fixef(dunnartPartPool),
      from=min(dunnartData$length), to=max(dunnartData$length), add=TRUE,
      lwd=2, col="goldenrod") 

dev.off() # the PDF won't appear in your Outputs foler until you close the device, like this.
# NOTE if you re-run this code, it will overwrite the old version of your PDF! To make a new one you will need to change the file name

pdf("Outputs/dunnartSitePreds.pdf")

with(dunnartData, plot(bodyMass ~ length, col=site)) # colour points by site

## now we'll loop through our sites, generating a curve for each site
lapply(1:length(unique(dunnartData$site)),
       function(n){
         
         # we're looping through sites, let's get the data for just our site
         tempSite <- dunnartData[dunnartData$site == levels(dunnartData$site)[n],]
         
         # now here we're using the cover for the actual site, and we're using the
         # site-specific coefficients instead of the global coefficients from fixef().
         # This will give us site intercepts
         curve(cbind(1,mean(tempSite$cover), x) %*% unlist(coef(dunnartPartPool)$site[n,]),
               from=min(tempSite$length), to=max(tempSite$length), col=n, add=TRUE)
         
       })

## and let's add the overall average (fixed effect) intercept for good measure!
curve(cbind(1,mean(dunnartData$cover),x)%*%fixef(dunnartPartPool),
      from=min(dunnartData$length), to=max(dunnartData$length), add=TRUE,
      lwd=4, col="goldenrod", lty="dashed") 

dev.off()


##7.2 Nesting in experiments
bleachingData<-read.csv("Data/bleachingData.csv", header = T, stringsAsFactors = TRUE) # this time we use "stringsAsFactors = TRUE" to tell R to treat character variables as factors
## visually explore the data
pairs(bleachingData[,c("bleaching", "tempTreatment", "fragmentSize", "raceway")])
# what can we say just from looking at the pairs plot?

## Are bleaching levels similar across raceways?
with(bleachingData, boxplot(bleaching ~ raceway))

## Is the experimental design balanced?
table(bleachingData$raceway, bleachingData$tempTreatment)

summary(bleachingData)

## fit a linear model
bleachingCompPool<- lm(bleaching ~ tempTreatment, data = bleachingData)

## first - diagnostics.
plot(bleachingCompPool, which=1)
plot(bleachingCompPool, which=2)

## okay. Now summary.
summary(bleachingCompPool)

## look at residuals based on treatment, coloured by raceway
plot(bleachingCompPool, which=1, pch=16, col=bleachingData$raceway)
## we could also do a boxplot of residuals per site
boxplot(residuals(bleachingCompPool) ~ bleachingData$raceway)

## Mixed-effects model structure
bleachingPartPool <- lmer(bleaching ~ tempTreatment + (1|raceway), data = bleachingData)

## DHARMa diagnostics
plot(simulateResiduals(bleachingPartPool), asFactor=FALSE)
## looks like there's a problem! Some of our residuals deviate from expectations. What's going on?
## convert temperature to a factor and choose the reference level we want
bleachingData$tempFact <- factor(bleachingData$tempTreatment,
                                 levels=c(28,26,30)) #28 goes first to be the "reference"
str(bleachingData$tempFact)

## centre the fragment size variable on its own mean
bleachingData$c_fragmentSize <- bleachingData$fragmentSize - mean(bleachingData$fragmentSize)

## look at a histogram of c_fragmentSize to see what happened
hist(bleachingData$c_fragmentSize)# the mean is zero!

# update model with treatment as a factor and fragment size
bleachingPartPoolCat <- lmer(bleaching ~ tempFact * c_fragmentSize + (1|raceway), data = bleachingData)

# diagnostics
plot(simulateResiduals(bleachingPartPoolCat)) # ahhh that's better.

# we can now examine the model summary
summary(bleachingPartPoolCat)

# and the random intercepts
coef(bleachingPartPoolCat)

# and the model performance
r2(bleachingPartPoolCat)

## install if you haven't already
##install.packages('emmeans')
library(emmeans) # we need emmeans to generate confidence intervals for our model predictions

pdf("Outputs/bleachingPlot.pdf", width=7)
## first the plot set-up. We jitter the temperature values (x values) so we can see the raw observations, and scale the size of each point by the size of the coral fragment using "cex"
plot(x=jitter(as.numeric(bleachingData$tempFact), amount=0.2),
     y=bleachingData$bleaching,
     cex=bleachingData$fragmentSize/30, # here's where we scale point size by fragment size
     col="grey", pch=16, 
     ylab=expression("Bleaching area (cm"^-2*")"), xaxt="n", xlab="")
axis(side=1, at=1:3, labels=parse(text=paste0(c(28, 26, 30), "*degree~C")))
mtext(side=1, line=2.25, text="Experimental water temperature")

## now we'll use emmeans to get mean bleaching for each temperature treatment
bleachPred <- confint(emmeans(bleachingPartPoolCat, c("tempFact", "c_fragmentSize")))

## first add the mean points
points(bleachPred$emmean, pch=16, cex=1.5)

## now add lines connecting our lower and up confidence intervals
segments(x0=1:3,
         x1=1:3,
         y0=bleachPred$lower.CL,
         y1=bleachPred$upper.CL, lwd=2)

dev.off()

##7.3.Repeated measures study

biomassData<-read.csv("Data/biomassData.csv", header = T, stringsAsFactors = TRUE)
head(biomassData)
## visually explore the data
pairs(biomassData[,c("biomass", "block", "timeSample", "enclosure")])
## what can we say just from looking at the pairs plot?

## Does biomass vary by block?
with(biomassData, boxplot(biomass ~ block))

## Plot biomass through time for each block
pdf("Outputs/biomass_vs_time_per_block.pdf", height = 6, width=9)
par(mfrow=c(2,3))

# split data into each block
lapply(split(biomassData, f=biomassData$block), function(blockData){
  
  # plot the points for biomass, coloured by enclosure treatment
  with(blockData, plot(biomass ~ timeSample, pch=16, col=c("cadetblue1", "indianred1")[enclosure]))
  
  # now split the block data into separate plots
  lapply(split(blockData, f=blockData$plot), function(plotData){
    # add lines connecting the biomass of each plot
    with(plotData, lines(timeSample, biomass, lty=3, lwd=1,
                         col=ifelse(enclosure=="N", "cadetblue1", "indianred1")))
  }) # close off plot loop
  
}) # close off block loop

dev.off()

## fit a linear model
biomassCompPool<- lm(biomass ~ enclosure * timeSample, data = biomassData)

# diagnostics (always)
plot(simulateResiduals(biomassCompPool))

# These are ok. We can examine the summary
summary(biomassCompPool)

# residuals by block
boxplot(residuals(biomassCompPool) ~ biomassData$block)

# residuals by sample time (our time predictor has explained most of the time variation)
boxplot(residuals(biomassCompPool) ~ biomassData$timeSample)

# mixed-effect model with observations nested in plots, which are nested in blocks
biomassPartPool <- lmer(biomass ~ enclosure * timeSample + (1|block/plot), data = biomassData)

# DHARMa diagnostics
plot(simulateResiduals(biomassPartPool))

# model summary
summary(biomassPartPool)

# look at the random effects (look closely)
coef(biomassPartPool)
# there are two entries in this list ("$block" and "$plot:block") with different intercepts. 

# mixed-effects models don't really calculate actual intercepts. They calculate "deviations from the global intercept", which we can see if we use ranef().

# global intercept
fixef(biomassPartPool)[1]

# and the intercept deviations from this global intercept (some are higher (positive) and some are lower (negative))
ranef(biomassPartPool)

# we can add the "intercept deviations" to the global intercept and compare them to coef()
cbind(coef(biomassPartPool)$block[,1],
      ranef(biomassPartPool)$block + fixef(biomassPartPool)[1])
# Hopefully it's clear that they're the same thing.

pdf("Outputs/biomass_model_fixed_effects.pdf", height = 6, width=6)

## plot data (jitter time slightly)
with(biomassData, plot(biomass ~ jitter(timeSample, amount =0.1), 
                       xlab="Time since start (months)",
                       pch=16, col=c("cadetblue1", "indianred1")[enclosure]))
# curve for open plots    
curve(cbind(1,0, x, x*0)%*%fixef(biomassPartPool), add=T, col="cadetblue1", lwd=3)
# curve for warming chambers
curve(cbind(1,1, x, x*1)%*%fixef(biomassPartPool), add=T,col="indianred1", lwd=3)  
# legend
legend("topleft", legend = c("Warming chamber", "No warming chamber"), 
       col=c("indianred1", "cadetblue1"), lwd=3)

dev.off()

rm(list = ls()) 

