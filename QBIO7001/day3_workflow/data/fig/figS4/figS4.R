### Figure S4 in Yamamichi et al. 
### A shady phytoplankton paradox: When phytoplankton increases under low light
# Chlorophyll a vs. TP, TN, zooplankton biomass, and zooplankton body length

panelLabel=function(text) mtext(text,side=3,adj=0,cex=1.25,line=0.5); 
read.csv("chla.csv",header=TRUE)->x
x1<-t(x[,-1])
x2<-c(mean(x1[-1,1]), mean(x1[-1,2]), mean(x1[-1,3]), mean(x1[-1,4]), mean(x1[-1,5]), mean(x1[-1,6]))
x3<-c(sd(x1[-1,1])/sqrt(nrow(x1)-1), sd(x1[-1,2])/sqrt(nrow(x1)-1), sd(x1[-1,3])/sqrt(nrow(x1)-1), 
	sd(x1[-1,4])/sqrt(nrow(x1)-1), sd(x1[-1,5])/sqrt(nrow(x1)-1), sd(x1[-1,6])/sqrt(nrow(x1)-1))

dev.new(width=10,height=10);
par(mfrow = c(2,2), mar=c(5, 5.5, 4, 2) + 0.1)
cols <- c("black", "black", "blue", "blue", "red", "red") # Low light = black, mid = blue, control = red
pchs <- c(19, 15, 19, 15, 19, 15) # Road side: circle (19), canal side: square (15)
type <- unique(x[, 1])

## Fig. S4a: total phosphorus
read.csv("TP.csv",header=FALSE)->y
y1<-t(y[,-1])
y2<-c(mean(y1[,1]), mean(y1[,2]), mean(y1[,3]), mean(y1[,4]), mean(y1[,5]), mean(y1[,6]))
y3<-c(sd(y1[,1])/sqrt(nrow(y1)), sd(y1[,2])/sqrt(nrow(y1)), sd(y1[,3])/sqrt(nrow(y1)), 
	sd(y1[,4])/sqrt(nrow(y1)), sd(y1[,5])/sqrt(nrow(y1)), sd(y1[,6])/sqrt(nrow(y1)))

x4<-x2[1]; x5<-x3[1]; y4<-y2[1]; y5<-y3[1];
plot(x4~y4,xlim=c(0.4,1.4),ylim=c(0,16),xlab=expression(paste("Total phosphorus (", mu, "mol ", L^{-1}, ")")),ylab=expression(paste("Chl ", italic(a), " (", mu, "g ", L^{-1}, ")")),pch=19,col=1,cex=2,cex.lab=2)
arrows(y4, x4, y4, x4-x5, angle = 90, length = 0.1, col = 1)
arrows(y4, x4, y4, x4+x5, angle = 90, length = 0.1, col = 1)
arrows(y4, x4, y4-y5, x4, angle = 90, length = 0.1, col = 1)
arrows(y4, x4, y4+y5, x4, angle = 90, length = 0.1, col = 1)
panelLabel("(a)");
legend("topleft", legend = type, pch = pchs, col = cols)
for(i in 2:6){
	x4<-x2[i]; x5<-x3[i]; y4<-y2[i]; y5<-y3[i];
	lines(x4~y4,pch=pchs[i],col=cols[i],cex=2,type="p")
	arrows(y4, x4, y4, x4-x5, angle = 90, length = 0.1, col = cols[i])
	arrows(y4, x4, y4, x4+x5, angle = 90, length = 0.1, col = cols[i])
	arrows(y4, x4, y4-y5, x4, angle = 90, length = 0.1, col = cols[i])
	arrows(y4, x4, y4+y5, x4, angle = 90, length = 0.1, col = cols[i])
}
cor.test(x2,y2,method="spearman")


## Fig. S4b: total nitrogen
read.csv("TN.csv",header=FALSE)->y
y1<-t(y[,-1])
y2<-c(mean(y1[,1]), mean(y1[,2]), mean(y1[,3]), mean(y1[,4]), mean(y1[,5]), mean(y1[,6]))
y3<-c(sd(y1[,1])/sqrt(nrow(y1)), sd(y1[,2])/sqrt(nrow(y1)), sd(y1[,3])/sqrt(nrow(y1)), 
	sd(y1[,4])/sqrt(nrow(y1)), sd(y1[,5])/sqrt(nrow(y1)), sd(y1[,6])/sqrt(nrow(y1)))

x4<-x2[1]; x5<-x3[1]; y4<-y2[1]; y5<-y3[1];
plot(x4~y4,xlim=c(35,70),ylim=c(0,16),xlab=expression(paste("Total nitrogen (", mu, "mol ", L^{-1}, ")")),ylab=expression(paste("Chl ", italic(a), " (", mu, "g ", L^{-1}, ")")),pch=19,col=1,cex=2,cex.lab=2)
arrows(y4, x4, y4, x4-x5, angle = 90, length = 0.1, col = 1)
arrows(y4, x4, y4, x4+x5, angle = 90, length = 0.1, col = 1)
arrows(y4, x4, y4-y5, x4, angle = 90, length = 0.1, col = 1)
arrows(y4, x4, y4+y5, x4, angle = 90, length = 0.1, col = 1)
panelLabel("(b)");
legend("bottomright", legend = type, pch = pchs, col = cols)
for(i in 2:6){
	x4<-x2[i]; x5<-x3[i]; y4<-y2[i]; y5<-y3[i];
	lines(x4~y4,pch=pchs[i],col=cols[i],cex=2,type="p")
	arrows(y4, x4, y4, x4-x5, angle = 90, length = 0.1, col = cols[i])
	arrows(y4, x4, y4, x4+x5, angle = 90, length = 0.1, col = cols[i])
	arrows(y4, x4, y4-y5, x4, angle = 90, length = 0.1, col = cols[i])
	arrows(y4, x4, y4+y5, x4, angle = 90, length = 0.1, col = cols[i])
}
cor.test(x2,y2,method="spearman")


## Fig. S4c: zooplankton biomass
read.csv("zop.csv",header=FALSE)->y
y1<-t(y[,-1])
y2<-c(mean(y1[,1]), mean(y1[,2]), mean(y1[,3]), mean(y1[,4]), mean(y1[,5]), mean(y1[,6]))
y3<-c(sd(y1[,1])/sqrt(nrow(y1)), sd(y1[,2])/sqrt(nrow(y1)), sd(y1[,3])/sqrt(nrow(y1)), 
	sd(y1[,4])/sqrt(nrow(y1)), sd(y1[,5])/sqrt(nrow(y1)), sd(y1[,6])/sqrt(nrow(y1)))

x4<-x2[1]; x5<-x3[1]; y4<-y2[1]; y5<-y3[1];
plot(x4~y4,xlim=c(0,600),ylim=c(0,16),xlab=expression(paste("Zooplankton biomass (", mu, "g DW ", L^{-1}, ")")),ylab=expression(paste("Chl ", italic(a), " (", mu, "g ", L^{-1}, ")")),pch=19,col=1,cex=2,cex.lab=2)
arrows(y4, x4, y4, x4-x5, angle = 90, length = 0.1, col = 1)
arrows(y4, x4, y4, x4+x5, angle = 90, length = 0.1, col = 1)
arrows(y4, x4, y4-y5, x4, angle = 90, length = 0.1, col = 1)
arrows(y4, x4, y4+y5, x4, angle = 90, length = 0.1, col = 1)
panelLabel("(c)");
legend("topright", legend = type, pch = pchs, col = cols)
for(i in 2:6){
	x4<-x2[i]; x5<-x3[i]; y4<-y2[i]; y5<-y3[i];
	lines(x4~y4,pch=pchs[i],col=cols[i],cex=2,type="p")
	arrows(y4, x4, y4, x4-x5, angle = 90, length = 0.1, col = cols[i])
	arrows(y4, x4, y4, x4+x5, angle = 90, length = 0.1, col = cols[i])
	arrows(y4, x4, y4-y5, x4, angle = 90, length = 0.1, col = cols[i])
	arrows(y4, x4, y4+y5, x4, angle = 90, length = 0.1, col = cols[i])
}
cor.test(x2,y2,method="spearman")


## Fig. S4d: zooplankton body length
read.csv("BodySize.csv",header=TRUE)->y
y1<-t(y[,-1])
y2<-c(mean(y1[-1,1]), mean(y1[-1,2]), mean(y1[-1,3]), mean(y1[-1,4]), mean(y1[-1,5]), mean(y1[-1,6]))
y3<-c(sd(y1[-1,1])/sqrt(nrow(y1)-1), sd(y1[-1,2])/sqrt(nrow(y1)-1), sd(y1[-1,3])/sqrt(nrow(y1)-1), 
	sd(y1[-1,4])/sqrt(nrow(y1)-1), sd(y1[-1,5])/sqrt(nrow(y1)-1), sd(y1[-1,6])/sqrt(nrow(y1)-1))

x4<-x2[1]; x5<-x3[1]; y4<-y2[1]; y5<-y3[1];
plot(x4~y4,xlim=c(300,600),ylim=c(0,16),xlab=expression(paste("Zooplankton body length (", mu, "m)")),ylab=expression(paste("Chl ", italic(a), " (", mu, "g ", L^{-1}, ")")),pch=19,col=1,cex=2,cex.lab=2)
arrows(y4, x4, y4, x4-x5, angle = 90, length = 0.1, col = 1)
arrows(y4, x4, y4, x4+x5, angle = 90, length = 0.1, col = 1)
arrows(y4, x4, y4-y5, x4, angle = 90, length = 0.1, col = 1)
arrows(y4, x4, y4+y5, x4, angle = 90, length = 0.1, col = 1)
panelLabel("(d)");
legend("topright", legend = type, pch = pchs, col = cols)
for(i in 2:6){
	x4<-x2[i]; x5<-x3[i]; y4<-y2[i]; y5<-y3[i];
	lines(x4~y4,pch=pchs[i],col=cols[i],cex=2,type="p")
	arrows(y4, x4, y4, x4-x5, angle = 90, length = 0.1, col = cols[i])
	arrows(y4, x4, y4, x4+x5, angle = 90, length = 0.1, col = cols[i])
	arrows(y4, x4, y4-y5, x4, angle = 90, length = 0.1, col = cols[i])
	arrows(y4, x4, y4+y5, x4, angle = 90, length = 0.1, col = cols[i])
}
cor.test(x2,y2,method="spearman")


# Checking TN:TP
read.csv("TP.csv",header=FALSE)->x
x1<-t(x[,-1])
x2<-c(mean(x1[,1]), mean(x1[,2]), mean(x1[,3]), mean(x1[,4]), mean(x1[,5]), mean(x1[,6]))
read.csv("TN.csv",header=FALSE)->y
y1<-t(y[,-1])
y2<-c(mean(y1[,1]), mean(y1[,2]), mean(y1[,3]), mean(y1[,4]), mean(y1[,5]), mean(y1[,6]))
y2/x2
max(y2/x2)
min(y2/x2)