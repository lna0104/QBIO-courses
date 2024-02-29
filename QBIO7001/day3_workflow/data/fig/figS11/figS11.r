### Figure S11 in Yamamichi et al. 
### A shady phytoplankton paradox: When phytoplankton increases under low light

read.csv("phytoplankton1.csv")->dat
df <- dat[c(2:9)]
library(ggfortify)
dev.new(width=8,height=6); 
autoplot(prcomp(df), data=dat, colour='Pond', loadings=T, loadings.colour = 'blue',loadings.label = TRUE, loadings.label.size = 3)
