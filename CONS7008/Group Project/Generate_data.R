##Random value
#11.3.1.1
`RE.11.3.1.1` <- rnorm(50, mean = 1.5, sd=0.3)
#11.3.1.2
`RE.11.3.1.2`<- rnorm(50, mean = 1.8, sd=0.5)
#11.9.5.1
`RE.11.9.5.1`<- rnorm(50, mean = 2.8, sd=0.4)
#11.9.5.2
`RE.11.9.5.2`<- rnorm(50, mean = 3.2, sd=0.5)

library(tidyverse)
data_1<-data.frame(cbind(`RE.11.3.1.1`,`RE.11.3.1.2`,`RE.11.9.5.1`,`RE.11.9.5.2`)) 
newData <- pivot_longer(data_1, everything(), names_to = "types")

#Simulate data with known correlation
N <- 500;
rho <- -0.7;
x1 <- rnorm(n = N, mean = 0, sd = 1);
x2 <- (rho * x1) + sqrt(1 - rho*rho) * rnorm(n = N, mean = 0, sd = 1)
data_2<-data.frame(x1,x2)

Control <- rnorm(50, mean = 5, sd = 0.6)
T1 <- rnorm(50, mean = 4.5, sd = 0.4)
T2 <- rnorm(50, mean = 3.8, sd = 0.3)
T3 <- rnorm(50, mean = 3.2, sd = 0.5)
T4 <- rnorm(50, mean = 2.5, sd = 0.5)


rdata_3 <-data.frame(Control,T1, T2, T3, T4) 
rdata_3 <- pivot_longer(rdata_3, everything(), names_to = "Treatments")
rdata_3$season <- "Rainy"

Control <- rnorm(50, mean = 4.5, sd = 0.4)
T1 <- rnorm(50, mean = 3.8, sd = 0.4)
T2 <- rnorm(50, mean = 3, sd = 0.3)
T3 <- rnorm(50, mean = 2.4, sd = 0.2)
T4 <- rnorm(50, mean = 1.5, sd = 0.3)

ddata_3 <-data.frame(Control,T1, T2, T3, T4) 
ddata_3 <- pivot_longer(ddata_3, everything(), names_to = "Treatments")
ddata_3$season <- "Dry" 

finalDat <- rbind(rdata_3, ddata_3)


##Plots
library(ggplot2)
library(hrbrthemes)

newData %>% ggplot(aes(x=types, y=value, fill=types)) + 
  geom_boxplot() +
  theme(legend.position="none", axis.text.y = element_blank(), axis.title = element_text(face="bold", size = 13),
        axis.text.x = element_text(size = 10)) +
  xlab("Areas") +
  ylab("Buffet grass") 

ggplot(data_2, aes(x=x1, y=x2)) +
  geom_point() +
  geom_smooth(method=lm , color="red", se=FALSE) +
  theme_ipsum(axis_title_size = 13, axis_title_face = "bold") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())+
  xlab("Buffet grass")+
  ylab("Solanum species")


finalDat %>% ggplot(aes(x=season, y=value, fill=Treatments)) + 
  geom_boxplot() +
  theme(axis.text.y = element_blank(), axis.title = element_text(face="bold", size = 13),
        axis.text.x = element_text(size = 10)) + 
  xlab("Seasons") +
  ylab("Height of buffet grass after 1/3/6 month(s)") +
  scale_fill_discrete(labels=c("Control", "T1: Light grazing", "T2: Moderate grazing", "T3: Herbicide", "T4: Light grazing + Herbicide"))





