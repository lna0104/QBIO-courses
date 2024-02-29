##Practical 8 Generalised linear models (GLMs)

##8.1 An example with count data
beetle_data<-read.csv("Data/beetle_data.csv", header = T)
## visually explore the data
pairs(beetle_data)
## what can we say just from looking at the pairs plot?

## how is the "counts" variable distributed?
hist(beetle_data$counts)## As Wanda Sykes would say, "that #$%& ain't normal!"

## what is the range?
range(beetle_data$counts)
## so we are looking at an integer variable (counts, with no negative numbers and no decimal places)

## install DHARMa and emmeans first if you haven't already
library(DHARMa) ## for great diagnostic plots for GLM and LMMs
library(emmeans) ## for all kinds of reasons that will become clear

## log-transform and fit a linear model
log_beetle_lm<-lm(log(counts) ~ Tmax + dist_to_edge, data = beetle_data)
## do we meet the assumptions of linear modelling?
qqnorm(resid(log_beetle_lm))
qqline(resid(log_beetle_lm))

## or check diagnostics using the simulateResiduals() function from the DHARMa package
plot(simulateResiduals(log_beetle_lm))

## the log link is the default for Poisson but its good practice to specify it in brackets
beetle_glm<-glm(counts ~ Tmax + dist_to_edge, family = poisson(link = log), data = beetle_data)

testDispersion(simulateResiduals(beetle_glm))
## red line is not in extreme location - all good!

## What about the other DHARMa diagnostics?
plot(simulateResiduals(beetle_glm))

summary(beetle_glm)


## we might as well output this nice plot to our "Outputs" folder in the CONS7008 R project!
pdf("Outputs/Beetle_counts_plot.pdf", height = 6, width = 11)
par(mfrow=c(1,2)) ## open a plot canvas with two panels (side-by-side)
## first, plot on "modelled" scale (log link so we use log scale)
with(beetle_data, plot(counts ~ Tmax, log="y", pch = 16, col=ifelse(dist_to_edge>100, "dodgerblue4", "goldenrod")))

## curve for Tmax at 50 m from edge
curve(exp(cbind(1,x,50)%*%coef(beetle_glm)), add=T, col = "goldenrod", lwd=3)
## curve for Tmax at 150 m from edge
curve(exp(cbind(1,x,150)%*%coef(beetle_glm)), add=T, col = "dodgerblue4", lwd=3)
## add a legend
text(x=24, y = c(1.5, 2), labels = c("50 m from edge", "150 m from edge"), col = c("goldenrod", "dodgerblue4"), pos=4)
mtext(side = 3, adj = 0, line=0.75, text="(a) Log scale (inside model)", cex=1.5)

## now plot on the original scale of the counts variable (don't have log="y")
with(beetle_data, plot(counts ~ Tmax, pch = 16, col=ifelse(dist_to_edge>100, "dodgerblue4", "goldenrod")))
## curve for Tmax at 50 m from edge
curve(exp(cbind(1,x,50)%*%coef(beetle_glm)), add=T, col = "goldenrod", lwd=3)
## curve for Tmax at 150 m from edge
curve(exp(cbind(1,x,150)%*%coef(beetle_glm)), add=T, col = "dodgerblue4", lwd=3)
mtext(side = 3, adj = 0, line=0.75, text="(b) Original scale (backtransformed)", cex=1.5)
dev.off()


##8.2.An example with proportion data
## read in rhodanthe_data
rhodanthe_data<-read.csv("Data/rhodanthe_data.csv")

## make a proportion variable for the response variable
rhodanthe_data$germ_prop<- rhodanthe_data$n_germ / rhodanthe_data$n_seeds_in_dish

## re-level the treatment factor so that control is the "reference" level 
rhodanthe_data$treatment <- factor(rhodanthe_data$treatment , levels = c("control","AR", "smoke"))

## fit a linear model
rhodanthe_lm<-lm(germ_prop ~ treatment, data = rhodanthe_data)

## look at diagnostics using DHARMa
plot(simulateResiduals(rhodanthe_lm))

rhodanthe_data$n_no_germ <- rhodanthe_data$n_seeds_in_dish - rhodanthe_data$n_germ

## this time, the response variable isn't just a single variable!
## its a concatenation of the n_germ and n_no_germ!!!
rhodanthe_glm<-glm(cbind(n_germ, n_no_germ) ~ treatment, family="binomial", data = rhodanthe_data)

summary(rhodanthe_glm)

testDispersion(simulateResiduals(rhodanthe_glm))
## the red line is in extreme location!
## what should we do???

## Option 1: quasibinomial
rhodanthe_glm2<-glm(cbind(n_germ, n_no_germ) ~ treatment, family="quasibinomial", data = rhodanthe_data)
summary(rhodanthe_glm2)

## this approach calculates a "dispersion parameter"
## for this model it is 6.06, indicating that the actual amount of unexplained variance is 6 times the amount that the binomial distribution explains

## Option 2: mixed-effects model
## load lme4 package so that we can use the glmer() function
library(lme4)
## create a dish ID column (a unique number for each row of data, which correspond to unique petri dishes)
rhodanthe_data$dish_ID = c(1:dim(rhodanthe_data)[1])

## fit the model with "+ (1|dish_ID)" as the random effect
rhodanthe_glmer<-glmer(cbind(n_germ, n_no_germ) ~ treatment + (1|dish_ID), family="binomial", data = rhodanthe_data)

## did this work?
testDispersion(simulateResiduals(rhodanthe_glmer))
## Yes, this is a great solution!

## look at the model summary
summary(rhodanthe_glmer)

emmeans(object = rhodanthe_glmer, specs = pairwise~treatment)
## emmeans to the rescue again!
plot(emmeans(rhodanthe_glmer, ~treatment, type = "response"), horizontal=F)
## we include type = "response" to plot on the probability scale, not the logit scale

##8.3. An example with binary (0/1) data

## read in cossinia_data
cossinia_data<-read.csv("Data/cossinia_data.csv")

## look at a summary
summary(cossinia_data)

cossinia_glm<-glm(occurrences ~ soil_depth + vpd + geology, family="binomial", data = cossinia_data)

## we still need to check our diagnostics (other than overdispersion)
plot(simulateResiduals(cossinia_glm))

## safe to proceed to inference via summary
summary(cossinia_glm)

## John extracted the hexcodes for some colours in the Cossinia photograph...what a nerd.
cossinia_cols<-c("#469652", "#e4e782")

## Plot on the "observed" scale of the counts variable
pdf("Outputs/Cossinia_occurrences_plot.pdf", height = 6, width = 6)
with(cossinia_data, plot(jitter(occurrences, amount=0.05) ~ vpd, pch = 16, 
                         col=ifelse(geology=="basalt", cossinia_cols[1], cossinia_cols[2]),
                         ylab = "Probability of Cossinia occurrence",
                         xlab = "Mean dry-season vapour-pressure deficit (kPa)"))
## curve for Basalt
curve(plogis(cbind(1,mean(cossinia_data$soil_depth), x,0)%*%coef(cossinia_glm)), add=T, col = cossinia_cols[1], lwd=3)
## curve for Granite
curve(plogis(cbind(1,mean(cossinia_data$soil_depth), x,1)%*%coef(cossinia_glm)), add=T, col = cossinia_cols[2], lwd=3)
## add a legend
text(x=0.2, y = c(0.15, 0.225), labels = c("Basalt", "Granite"), col = cossinia_cols, pos=4)
dev.off()

rm(list = ls()) 
