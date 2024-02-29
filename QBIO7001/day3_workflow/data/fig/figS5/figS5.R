### Figure S5 in Yamamichi et al. 
### A shady phytoplankton paradox: When phytoplankton increases under low light

panelLabel=function(text) mtext(text,side=3,adj=0,cex=1.25,line=0.5); 
z<-c(1,2,3,4,6,7,8)
read.csv("chla.csv",header=TRUE)->x
x1<-t(x[,-1])
x2<-c(mean(x1[,1][z]),mean(x1[,2][z]),mean(x1[,3][z]),mean(x1[,4][z]),mean(x1[,5][z]),mean(x1[,6][z]))
x3<-c(x1[,1][z],x1[,2][z],x1[,3][z],x1[,4][z],x1[,5][z],x1[,6][z])
read.csv("biomass.csv",header=TRUE)->y
y1<-t(y[,-1])
y2<-c(mean(y1[,1]),mean(y1[,2]),mean(y1[,3]),mean(y1[,4]),mean(y1[,5]),mean(y1[,6]))
y3<-c(y1[,1],y1[,2],y1[,3],y1[,4],y1[,5],y1[,6])

dev.new(width=16,height=6); 
par(mfrow = c(1,2), mar=c(5, 6, 3, 2) + 0.1, mgp=c(3.5,1,0))
cols <- c("black", "black", "blue", "blue", "red", "red")
pchs <- c(19, 15, 19, 15, 19, 15)
type <- unique(x[, 1])


## Fig. S5a
# Chlorophyll a vs. biovolume
plot(x1[,1][z]~y1[,1],xlim=c(0,5.5*10^6),ylim=c(0,32),xlab=expression(paste("Biovolume (", mu, m^{3}, " ", mL^{-1}, ")")),ylab=expression(paste("Chl ", italic(a), " (", mu, "g ", L^{-1}, ")")),pch=19,col="black",cex=2,cex.lab=2)
for(i in 2:6){
	lines(x1[,i][z]~y1[,i],pch=pchs[i],col=cols[i],cex=2,type="p")
}
legend(45*10^5, 20, legend = type, pch = pchs, col = cols)
panelLabel("(a)");
cor.test(x2,y2,method="spearman")
cor.test(x3,y3,method="spearman")


## Fig. S5b
# Time-series of chlorophyll a/biovolume
date<-c("2015/6/24","2015/7/10","2015/7/22","2015/8/3","2015/8/27","2015/9/8","2015/9/21")
xaxis <- 1:(ncol(x[, -1])-1)

plot(as.POSIXct(date), log10(y[1,-1]), type = "n", ylim = c(-6.5, -4), xlab = "Date in 2015", ylab = expression(paste("log"[10], "(Chl ", italic(a), "/biovolume)")),cex.lab=2,xaxt="n")
axis.POSIXct(1, at=date, format="%b/%d")
for (i in 1:length(type)) {
  points(as.POSIXct(date), log10(x1[,i][z]/y1[,i]), col = cols[i], pch = pchs[i], cex=2)
  lines(as.POSIXct(date), log10(x1[,i][z]/y1[,i]), col = cols[i], lwd=2)
}
legend("topleft", legend = type, pch = pchs, lty = 1, col = cols, ncol=2)
panelLabel("(b)");