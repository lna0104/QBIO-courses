### Figure S2 in Yamamichi et al. 
### A shady phytoplankton paradox: When phytoplankton increases under low light
# Macrophyte biomass vs. macrophyte coverage

cover<-c(0,0,48.4,7.7,0,87.0)
macro<-c(0,0.2,75,31.5,0.2,94.3)
type <- c("202L", "219L", "204M", "218M", "203H", "217H") 
cols <- c("black", "black", "blue", "blue", "red", "red")
pchs <- c(19, 15, 19, 15, 19, 15)

dev.new(width=8,height=6); 
par(mar=c(5, 5.5, 4, 2) + 0.1)
plot(cover, macro, xlim = c(0,100), ylim = c(0, 100),
     xlab = "Macrophyte coverage (%)", ylab =expression(paste("Macrophyte biomass (g DW ", m^{-2}, ")")),pch=pchs,col=cols,cex=2,cex.lab=1.7)
legend("topleft", legend = type, pch = pchs, col = cols)

cor(cover,macro,method="spearman")
cor.test(cover,macro,method="spearman")