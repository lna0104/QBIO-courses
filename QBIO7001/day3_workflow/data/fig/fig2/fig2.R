### Figure 2 in Yamamichi et al. 
### A shady phytoplankton paradox: When phytoplankton increases under low light

panelLabel=function(text) mtext(text,side=3,adj=0,cex=1.25,line=0.5); 
dev.new(width=13,height=10); 
par(mfrow = c(2,2), mar=c(5, 5.5, 4, 2) + 0.1)
cols <- c("black", "black", "blue", "blue", "red", "red")
pchs <- c(19, 15, 19, 15, 19, 15)

# Data
read.csv("chla.csv",header=TRUE)->dat
da<-t(dat[,-1])
read.csv("35ponds.csv",header=FALSE)->data
d<-data[-35,]


## Fig. 2a
# Time-series of chlorophyll a
date<-c("2015/6/24","2015/7/10","2015/7/22","2015/8/3","2015/8/16","2015/8/27","2015/9/8","2015/9/21")
type <- unique(dat[, 1])

plot(as.POSIXct(date), log10(dat[1,-1]), type = "n", ylim = c(-0.1, 1.5), xlab = "Date in 2015", ylab = expression(paste("log"[10], "(Chl ", italic(a), " (", mu, "g ", L^{-1}, "))")), cex.lab=2, xaxt="n")
panelLabel("(a)");
axis.POSIXct(1, at=date, format="%b/%d")
for (i in 1:length(type)) {
  m <- apply(dat[dat[, 1] == type[i], -1], 2, mean)
  points(as.POSIXct(date), log10(m), col = cols[i], pch = pchs[i], cex=2)
  lines(as.POSIXct(date), log10(m), col = cols[i], lwd=2)
}
legend("bottomleft", legend = dat[, 1], pch = pchs, lty = 1, col = cols)


## Fig. 2b
# Boxplot of chlorophyll a
boxplot(log10(da[-1,1]), log10(da[-1,2]), log10(da[-1,3]), log10(da[-1,4]), log10(da[-1,5]), log10(da[-1,6]), names=dat[,1], border=cols, xlab="Pond ID", ylab = expression(paste("log"[10], "(Chl ", italic(a), " (", mu, "g ", L^{-1}, "))")), cex.lab=2, ylim=c(-0.1, 1.5))
text(1.2, 1.2, "a", cex=1.5);text(2.2, 1.3, "a", cex=1.5);text(3.2, 0.65, "b", cex=1.5);text(4.2, 1.02, "a", cex=1.5);text(5.2, 1.33, "a", cex=1.5);text(6.2, 0.5, "b", cex=1.5)
panelLabel("(b)");

#202 a
#219 a
#204 b
#218 a
#203 a
#217 b


## Fig. 2c
# Chlorophyll a vs. macrophyte biomass
macro<-c(0, 0.221, 75.2, 31.5, 0, 94.3)
plank<-c(mean(da[-1,1]), mean(da[-1,2]), mean(da[-1,3]), mean(da[-1,4]), mean(da[-1,5]), mean(da[-1,6]))
plase<-c(sd(da[-1,1])/sqrt(nrow(da)-1), sd(da[-1,2])/sqrt(nrow(da)-1), sd(da[-1,3])/sqrt(nrow(da)-1), 
	sd(da[-1,4])/sqrt(nrow(da)-1), sd(da[-1,5])/sqrt(nrow(da)-1), sd(da[-1,6])/sqrt(nrow(da)-1))

# 202L
x<-macro[1]; y<-plank[1]; y1<-plase[1];
plot(x, y, xlim=c(0,100), ylim=c(0,15), xlab=expression(paste("Macrophyte biomass (g DW ", m^{-2}, ")")), ylab=expression(paste("Chl ", italic(a), " (", mu, "g ", L^{-1}, ")")), pch=19, col=1, cex=2, cex.lab=2)
arrows(x, y, x, y-y1, angle = 90, length = 0.1, col = 1)
arrows(x, y, x, y+y1, angle = 90, length = 0.1, col = 1)
panelLabel("(c)");
legend("topright", legend = dat[,1], pch = pchs, col = cols)

# 219L-217H
for(i in 2:6){
	x<-macro[i]; y<-plank[i]; y1<-plase[i];
	arrows(x, y, x, y-y1, angle = 90, length = 0.1, col = cols[i])
	arrows(x, y, x, y+y1, angle = 90, length = 0.1, col = cols[i])
	lines(x,y,pch=pchs[i],col=cols[i],type="p",cex=2)
}

cor.test(macro,plank,method="spearman")


## Fig. 2d
# Chlorophyll a vs. macrophyte coverage
# Data from 2016
library(KernSmooth)
est <- bkde2D(d, bandwidth=c(10,10), range.x=list(c(0,100),c(0,25)))

contour(est$x1, est$x2, est$fhat, xlab="Macrophyte coverage (%)", ylab=expression(paste("Chl ", italic(a), " (", mu, "g ", L^{-1}, ")")), drawlabels=FALSE, cex.lab=2, col="gray70") 
lines(d, type="p", pch=16, cex=1.5)
panelLabel("(d)");

# Data from 2015
cover<-c(0, 0, 48.4, 7.7, 0, 87.0)
for(i in 5:6){
	x<-cover[i]; y<-plank[i]; y1<-plase[i];
	arrows(x, y, x, y-y1, angle = 90, length = 0.1, col = cols[i])
	arrows(x, y, x, y+y1, angle = 90, length = 0.1, col = cols[i])
	lines(x,y,pch=pchs[i],col=cols[i],type="p",cex=2)
}


# Statistical analyses
library(tidyverse)
library(nlme)
library(multcomp)

read.csv("chla.csv",header=TRUE)->dat
dat<-dat[,-2]

dat2<- dat %>%
  rownames_to_column('id') %>%
  gather(date, chla, -id, -pond)


anov1<-aov(log10(chla)~pond/date, dat2)

nest1 <- lme(log10(chla) ~ pond, random=~1|date,
            data=dat2,
            method="REML")
anova.lme(nest1, type="sequential",  adjustSigma = FALSE)

posthoc<- glht(nest1, linfct=mcp(pond="Tukey"))
mcs<- summary(posthoc,  test=adjusted("single-step"))
mcs