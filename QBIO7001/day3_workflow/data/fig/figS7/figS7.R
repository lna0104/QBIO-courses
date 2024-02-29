### Figure S7 in Yamamichi et al. 
### A shady phytoplankton paradox: When phytoplankton increases under low light

dev.new(width=8,height=6); 
par(mar=c(5, 5.5, 4, 2) + 0.1)
type <- c("202L", "219L", "204M", "218M", "203H", "217H") # Low light = black, mid = blue, 
cols <- c("black", "black", "blue", "blue", "red", "red") # Low light = black, mid = blue, control = red
pchs <- c(19, 15, 19, 15, 19, 15) # Road side: circle (19), canal side: square (15)
percentage <- c(0.246, 0.246, 0.435, 0.435, 1, 1)*100;

## Fig. S7a
# TP vs. light intensity
read.csv("TP.csv",header=FALSE)->y
y1<-t(y[,-1])
y2<-c(mean(y1[,1]), mean(y1[,2]), mean(y1[,3]), mean(y1[,4]), mean(y1[,5]), mean(y1[,6]))
y3<-c(sd(y1[,1])/sqrt(nrow(y1)), sd(y1[,2])/sqrt(nrow(y1)), sd(y1[,3])/sqrt(nrow(y1)), 
	sd(y1[,4])/sqrt(nrow(y1)), sd(y1[,5])/sqrt(nrow(y1)), sd(y1[,6])/sqrt(nrow(y1)))

x<-percentage[1]; y<-y2[1]; z<-y3[1];
plot(x,y,xlim=c(0,110),ylim=c(0.4,1.4),xlab="Percent surface light transmission",ylab=expression(paste("Total phosphorus (", mu, "mol ", L^{-1}, ")")),pch=pchs[1],col=cols[1],cex=2,cex.lab=1.7)
arrows(x, y+z, x, y-z, angle = 90, length = 0.1, col = cols[1])
arrows(x, y-z, x, y+z, angle = 90, length = 0.1, col = cols[1])
legend("topleft", legend = type, pch = pchs, col = cols)

for(i in 2:6){
	x<-percentage[i]; y<-y2[i]; z<-y3[i];
	lines(x,y,pch=pchs[i],col=cols[i],type="p",cex=2)
	arrows(x, y+z, x, y-z, angle = 90, length = 0.1, col = cols[i])
	arrows(x, y-z, x, y+z, angle = 90, length = 0.1, col = cols[i])
}


## Fig. S7b
# TN vs. light intensity
read.csv("TN.csv",header=FALSE)->y
y1<-t(y[,-1])
y2<-c(mean(y1[,1]), mean(y1[,2]), mean(y1[,3]), mean(y1[,4]), mean(y1[,5]), mean(y1[,6]))
y3<-c(sd(y1[,1])/sqrt(nrow(y1)), sd(y1[,2])/sqrt(nrow(y1)), sd(y1[,3])/sqrt(nrow(y1)), 
	sd(y1[,4])/sqrt(nrow(y1)), sd(y1[,5])/sqrt(nrow(y1)), sd(y1[,6])/sqrt(nrow(y1)))

x<-percentage[1]; y<-y2[1]; z<-y3[1];
plot(x,y,xlim=c(0,110),ylim=c(35,70),xlab="Percent surface light transmission",ylab=expression(paste("Total nitrogen (", mu, "mol ", L^{-1}, ")")),pch=pchs[1],col=cols[1],cex=2,cex.lab=1.7)
arrows(x, y+z, x, y-z, angle = 90, length = 0.1, col = cols[1])
arrows(x, y-z, x, y+z, angle = 90, length = 0.1, col = cols[1])
legend("topleft", legend = type, pch = pchs, col = cols)

for(i in 2:6){
	x<-percentage[i]; y<-y2[i]; z<-y3[i];
	lines(x,y,pch=pchs[i],col=cols[i],type="p",cex=2)
	arrows(x, y+z, x, y-z, angle = 90, length = 0.1, col = cols[i])
	arrows(x, y-z, x, y+z, angle = 90, length = 0.1, col = cols[i])
}