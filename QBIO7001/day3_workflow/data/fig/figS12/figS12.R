### Figure S12 in Yamamichi et al. 
### A shady phytoplankton paradox: When phytoplankton increases under low light

panelLabel=function(text) mtext(text,side=3,adj=0,cex=1.25,line=0.5); 
dev.new(width=12,height=6); 
par(mfrow = c(1,2), mar=c(5, 5.5, 4, 2) + 0.1)
cols <- c("black", "black", "blue", "blue", "red", "red") # Low light = black, mid = blue, control = red
pchs <- c(19, 15, 19, 15, 19, 15) # Road side: circle (19), canal side: square (15)

read.csv("BodySize.csv",header=TRUE)->x
x1<-t(x[,-1])
x2<-c(mean(x1[-1,1]), mean(x1[-1,2]), mean(x1[-1,3]), mean(x1[-1,4]), mean(x1[-1,5]), mean(x1[-1,6]))
x3<-c(sd(x1[-1,1])/sqrt(nrow(x1)-1), sd(x1[-1,2])/sqrt(nrow(x1)-1), sd(x1[-1,3])/sqrt(nrow(x1)-1), 
	sd(x1[-1,4])/sqrt(nrow(x1)-1), sd(x1[-1,5])/sqrt(nrow(x1)-1), sd(x1[-1,6])/sqrt(nrow(x1)-1))
type <- unique(x[, 1])


## Fig. S12a
# Biovolume vs. zooplankton body length
read.csv("biomass.csv",header=TRUE)->y
y1<-t(y[,-1])
y2<-c(mean(y1[-1,1]), mean(y1[-1,2]), mean(y1[-1,3]), mean(y1[-1,4]), mean(y1[-1,5]), mean(y1[-1,6]))
y3<-c(sd(y1[-1,1])/sqrt(nrow(y1)-1), sd(y1[-1,2])/sqrt(nrow(y1)-1), sd(y1[-1,3])/sqrt(nrow(y1)-1), 
	sd(y1[-1,4])/sqrt(nrow(y1)-1), sd(y1[-1,5])/sqrt(nrow(y1)-1), sd(y1[-1,6])/sqrt(nrow(y1)-1))

x4<-x2[1]; x5<-x3[1]; y4<-y2[1]; y5<-y3[1];
plot(x4,y4,xlim=c(300,600),ylim=c(0,4500000),xlab=expression(paste("Zooplankton body length (", mu, "m)")),ylab=expression(paste("Biovolume (", mu, m^{3}, " m", L^{-1},  ")")),pch=19,col=1,cex=2,cex.lab=2)
arrows(x4, y4, x4, y4-y5, angle = 90, length = 0.1, col = 1)
arrows(x4, y4, x4, y4+y5, angle = 90, length = 0.1, col = 1)
arrows(x4, y4, x4-x5, y4, angle = 90, length = 0.1, col = 1)
arrows(x4, y4, x4+x5, y4, angle = 90, length = 0.1, col = 1)
panelLabel("(a)");
legend("topright", legend = type, pch = pchs, col = cols)
for(i in 2:6){
	x4<-x2[i]; x5<-x3[i]; y4<-y2[i]; y5<-y3[i];
	lines(x4,y4,pch=pchs[i],col=cols[i],cex=2,type="p")
	arrows(x4, y4, x4, y4-y5, angle = 90, length = 0.1, col = cols[i])
	arrows(x4, y4, x4, y4+y5, angle = 90, length = 0.1, col = cols[i])
	arrows(x4, y4, x4-x5, y4, angle = 90, length = 0.1, col = cols[i])
	arrows(x4, y4, x4+x5, y4, angle = 90, length = 0.1, col = cols[i])
}
cor.test(x2,y2,method="spearman")


## Fig. S12b
# Seston carbon vs. zooplankton body length
read.csv("sestonC.csv",header=TRUE)->y
y1<-t(y[,-1])
y2<-c(mean(y1[-1,1]), mean(y1[-1,2]), mean(y1[-1,3]), mean(y1[-1,4]), mean(y1[-1,5]), mean(y1[-1,6]))
y3<-c(sd(y1[-1,1])/sqrt(nrow(y1)-1), sd(y1[-1,2])/sqrt(nrow(y1)-1), sd(y1[-1,3])/sqrt(nrow(y1)-1), 
	sd(y1[-1,4])/sqrt(nrow(y1)-1), sd(y1[-1,5])/sqrt(nrow(y1)-1), sd(y1[-1,6])/sqrt(nrow(y1)-1))

x4<-x2[1]; x5<-x3[1]; y4<-y2[1]; y5<-y3[1];
plot(x4,y4,xlim=c(300,600),ylim=c(40,310),xlab=expression(paste("Zooplankton body length (", mu, "m)")),ylab=expression(paste("Seston carbon (", mu, "mol ", L^{-1},  ")")),pch=19,col=1,cex=2,cex.lab=2)
arrows(x4, y4, x4, y4-y5, angle = 90, length = 0.1, col = 1)
arrows(x4, y4, x4, y4+y5, angle = 90, length = 0.1, col = 1)
arrows(x4, y4, x4-x5, y4, angle = 90, length = 0.1, col = 1)
arrows(x4, y4, x4+x5, y4, angle = 90, length = 0.1, col = 1)
panelLabel("(b)");
legend("topright", legend = type, pch = pchs, col = cols)
for(i in 2:6){
	x4<-x2[i]; x5<-x3[i]; y4<-y2[i]; y5<-y3[i];
	lines(x4,y4,pch=pchs[i],col=cols[i],cex=2,type="p")
	arrows(x4, y4, x4, y4-y5, angle = 90, length = 0.1, col = cols[i])
	arrows(x4, y4, x4, y4+y5, angle = 90, length = 0.1, col = cols[i])
	arrows(x4, y4, x4-x5, y4, angle = 90, length = 0.1, col = cols[i])
	arrows(x4, y4, x4+x5, y4, angle = 90, length = 0.1, col = cols[i])
}
cor.test(x2,y2,method="spearman")