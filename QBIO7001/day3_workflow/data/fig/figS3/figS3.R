### Figure S3 in Yamamichi et al. 
### A shady phytoplankton paradox: When phytoplankton increases under low light
# Attenuation coefficients vs. phytoplankton abundance

panelLabel=function(text) mtext(text,side=3,adj=0,cex=1.25,line=0.5); 
read.csv("attenuation.csv",header=TRUE)->y
y1<-t(y[,-1])
y2<-c(mean(y1[,1]),mean(y1[,2]),mean(y1[,3]),mean(y1[,4]),mean(y1[,5]),mean(y1[,6]))
y3<-c(y1[,1],y1[,2],y1[,3],y1[,4],y1[,5],y1[,6])

dev.new(width=10,height=10); 
par(mfrow = c(2,2), mar=c(5, 6, 3, 2) + 0.1, mgp=c(3.5,1,0))
cols <- c("black", "black", "blue", "blue", "red", "red")
pchs <- c(19, 15, 19, 15, 19, 15)
type <- unique(y[, 1])


## Fig. S3a: chlorophyll a
read.csv("chla.csv",header=TRUE)->x
x1<-t(x[,-1])
x2<-c(mean(x1[,1]),mean(x1[,2]),mean(x1[,3]),mean(x1[,4]),mean(x1[,5]),mean(x1[,6]))
x3<-c(x1[,1],x1[,2],x1[,3],x1[,4],x1[,5],x1[,6])

plot(y1[,1]~x1[,1],xlim=c(0,32),ylim=c(0.3,3),xlab=expression(paste("Chl ", italic(a), " (", mu, "g ", L^{-1}, ")")),ylab=expression(paste("Attenuation coefficient (", m^{-1}, ")")),pch=19,col="black",cex=2,cex.lab=2)
for(i in 2:6){
	lines(y1[,i]~x1[,i],pch=pchs[i],col=cols[i],cex=2,type="p")
}
legend("bottomright", legend = type, pch = pchs, col = cols)
panelLabel("(a)");
#plot(x2,y2)
cor.test(x2,y2,method="spearman")
#plot(x3,y3)
cor.test(x3,y3,method="spearman")


## Fig. S3b: biovolume
read.csv("biomass.csv",header=TRUE)->x
x1<-t(x[,-1])
x2<-c(mean(x1[,1]),mean(x1[,2]),mean(x1[,3]),mean(x1[,4]),mean(x1[,5]),mean(x1[,6]))
x3<-c(x1[,1],x1[,2],x1[,3],x1[,4],x1[,5],x1[,6])

z<-c(1,2,3,4,6,7,8)
plot(y1[,1][z]~x1[,1],xlim=c(0,5500000),ylim=c(0.3,3),xlab=expression(paste("Biovolume (", mu, m^{3}, " m", L^{-1},  ")")),ylab=expression(paste("Attenuation coefficient (", m^{-1}, ")")),pch=19,col="black",cex=2,cex.lab=2)
for(i in 2:6){
	lines(y1[,i][z]~x1[,i],pch=pchs[i],col=cols[i],cex=2,type="p")
}
legend("bottomright", legend = type, pch = pchs, col = cols)
panelLabel("(b)");
cor.test(x2,y2,method="spearman")
y4<-c(y1[,1][z],y1[,2][z],y1[,3][z],y1[,4][z],y1[,5][z],y1[,6][z])
#plot(x3,y4)
cor.test(x3,y4,method="spearman")


## Fig. S3c: seston carbon
read.csv("sestonC.csv",header=TRUE)->x
x1<-t(x[,-1])
x2<-c(mean(x1[,1]),mean(x1[,2]),mean(x1[,3]),mean(x1[,4]),mean(x1[,5]),mean(x1[,6]))
x3<-c(x1[,1],x1[,2],x1[,3],x1[,4],x1[,5],x1[,6])

plot(y1[,1]~x1[,1],xlim=c(30,500),ylim=c(0.3,3),xlab=expression(paste("Seston carbon (", mu, "mol ", L^{-1},  ")")),ylab=expression(paste("Attenuation coefficient (", m^{-1}, ")")),pch=19,col="black",cex=2,cex.lab=2)
for(i in 2:6){
	lines(y1[,i]~x1[,i],pch=pchs[i],col=cols[i],cex=2,type="p")
}
legend("bottomright", legend = type, pch = pchs, col = cols)
panelLabel("(c)");
cor.test(x2,y2,method="spearman")
cor.test(x3,y3,method="spearman")