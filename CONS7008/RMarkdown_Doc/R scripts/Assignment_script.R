library(tidyverse)
library(DHARMa) # flexible model diagnostics
library(emmeans) 
library(lme4) # for mixed-effects models
library(lmerTest) # to get p-values
library(ggplot2)

##Import data
NatParkData<-read_csv("CHYTRID_DATA_2023/National_Park_Data.csv")
summary(NatParkData)
ChytridAmpData<-read_csv("CHYTRID_DATA_2023/Chytrid_Amphibian_Data.csv")
summary(ChytridAmpData)

##Data analysis
#National_Park_Data
NatParkData$NP<-factor(NatParkData$NP)

pairs(NatParkData)
hist(NatParkData$frog_spp_richness, breaks=5)
hist(log(NatParkData$frog_spp_richness), breaks=5)

## log-transform and fit a linear model
NatParkData_lm<-lm(frog_spp_richness ~ VPD + NP_area_1000_ha + visit_rate_1000_per_yr + plant_spp_richness, data = NatParkData)

## do we meet the assumptions of linear modelling?
qqnorm(resid(NatParkData_lm))
qqline(resid(NatParkData_lm))

## or check diagnostics using the simulateResiduals() function from the DHARMa package
plot(simulateResiduals(NatParkData_lm))
summary(NatParkData_lm)

#Chytrid_Amphibian_Data
ChytridAmpData$NP<-factor(ChytridAmpData$NP)
ChytridAmpData$species<-factor(ChytridAmpData$species, levels=c("Taudactylus acutirostris","Taudactylus rheophilus","Rhinella marina"))
ChytridAmpData$n_no_infected<-ChytridAmpData$n_frogs_sampled - ChytridAmpData$n_infected
ChytridAmpData$normal_frogs_prop<-ChytridAmpData$n_no_infected/ChytridAmpData$n_frogs_sampled

hist(ChytridAmpData$n_frogs_sampled)
hist(log(ChytridAmpData$n_frogs_sampled))
hist(ChytridAmpData$n_infected)
hist(ChytridAmpData$normal_frogs_prop)
hist(ChytridAmpData$n_no_infected)

p1<-ggplot(ChytridAmpData, aes(x=n_frogs_sampled)) + 
  geom_histogram(color="black", fill="white", bins=10)
p2<-ggplot(ChytridAmpData, aes(x=n_infected)) + 
  geom_histogram(color="black", fill="white", bins=10)
p3<-ggplot(ChytridAmpData, aes(x=normal_frogs_prop)) + 
  geom_histogram(color="black", fill="white", bins=10)

grid.arrange(p1,p2,p3,ncol = 3, nrow = 1)

ggplot(ChytridAmpData, aes(x=species, y=n_frogs_sampled, fill=species)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C8483","#60B2E5","#53F4FF"))+
  xlab("Species")+
  ylab("Number of frogs in each creek")

ggplot(ChytridAmpData, aes(x=species, y=n_infected, fill=species)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#8ACB88","#FFBF46","#575761"))+
  xlab("Species")+
  ylab("Number of sampled frogs/toads that had visible signs of Chytrid infection")

ggplot(ChytridAmpData, aes(x=pop_density, y=normal_frogs_prop, color=species)) + 
  geom_point(size=2) +
  scale_color_manual(values=c("#8ACB88","#FFBF46","#575761"))+
  theme_minimal() +
  theme(legend.background = element_rect(linetype="solid", 
                                         colour ="#423E3B"))

ggplot(ChytridAmpData, aes(x=NP, y=n_frogs_sampled, fill=NP)) + 
  geom_boxplot()+
  xlab("National Parks (NPs)")+
  ylab("Number of frogs in each creek")

ggplot(ChytridAmpData, aes(x=NP, y=n_infected, fill=NP)) + 
  geom_boxplot()+
  xlab("National Parks (NPs)")+
  ylab("Number of sampled frogs/toads that had visible signs of Chytrid infection")

ggplot(ChytridAmpData, aes(x=NP, y=normal_frogs_prop, fill=NP)) + 
  geom_boxplot()+
  xlab("National Parks (NPs)")+
  ylab("normal_frogs_prop")

ggplot(ChytridAmpData, aes(x=species, y=normal_frogs_prop, fill=species)) + 
  geom_boxplot()+
  scale_fill_manual(values=c("#7C8483","#60B2E5","#53F4FF"))+
  xlab("Species")+
  ylab("Number of frogs in each creek")

##Linear model
ChytridAmp_nFrogs_lm<-lm(n_frogs_sampled ~ track_density + pop_density + species + NP, data = ChytridAmpData)
plot(simulateResiduals(ChytridAmp_nFrogs_lm))
summary(ChytridAmp_nFrogs_lm)

ChytridAmp_prop_lm<-lm(normal_frogs_prop ~ track_density + pop_density + species + NP, data = ChytridAmpData)
plot(simulateResiduals(ChytridAmp_prop_lm))

ChytridAmp_nNorFrogs_lm<-lm(n_no_infected ~ track_density + pop_density + species + NP, data = ChytridAmpData)
plot(simulateResiduals(ChytridAmp_nNorFrogs_lm))

#Linear mixed-effect model (LMMs)
ChytridAmp_lmm<-lmer(normal_frogs_prop ~ track_density + pop_density + species + (1|NP), data = ChytridAmpData)
plot(simulateResiduals(ChytridAmp_lmm))
summary(ChytridAmp_lmm)

#Generalized linear model (GLMs)
ChytridAmp_glm_1 <- glm(cbind(n_infected, n_no_infected) ~ track_density + pop_density + species + NP, family="binomial", data = ChytridAmpData)
summary(ChytridAmp_glm_1)
testDispersion(simulateResiduals(ChytridAmp_glm_1))

#Overdispersed
ChytridAmpData$ID = c(1:dim(ChytridAmpData)[1])
ChytridAmp_glmer_1 <- glmer(cbind(n_infected, n_no_infected) ~ track_density + pop_density + species + NP + (1|ID), family="binomial", data = ChytridAmpData)
summary(ChytridAmp_glmer_1)
testDispersion(simulateResiduals(ChytridAmp_glmer_1))

#GLM + mixed-effects model  
ChytridAmp_glmer_2 <- glmer(cbind(n_infected, n_no_infected) ~ track_density + pop_density + species + (1|ID) + (1|NP), family="binomial", data = ChytridAmpData)
summary(ChytridAmp_glmer_2)
testDispersion(simulateResiduals(ChytridAmp_glmer_2))

#Remove pop_density 
ChytridAmp_glmer_3 <- glmer(cbind(n_infected, n_no_infected) ~ track_density + species + (1|ID) + (1|NP), family="binomial", data = ChytridAmpData)
summary(ChytridAmp_glmer_3)
testDispersion(simulateResiduals(ChytridAmp_glmer_3))

#See correlation between different NPs and normal frogs sampled
#Join two datasets
mergedData<-merge(ChytridAmpData,NatParkData, by = "NP",  all = TRUE)
ChytridAmp_glmer_4 <- glmer(cbind(n_infected, n_no_infected) ~ track_density + pop_density + species + VPD + NP_area_1000_ha + visit_rate_1000_per_yr + plant_spp_richness + (1|ID) + (1|NP), family="binomial", data = mergedData)
summary(ChytridAmp_glmer_4)
testDispersion(simulateResiduals(ChytridAmp_glmer_4))

with(mergedData, pairs(log(n_frogs_sampled) ~ VPD + NP_area_1000_ha + visit_rate_1000_per_yr + plant_spp_richness + frog_spp_richness))

library(reshape2)
cormat <- NatParkData %>% select(!NP) %>% cor()
meltedCormat <- melt(cormat)
ggplot(data = meltedCormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  theme(axis.title = element_blank())
