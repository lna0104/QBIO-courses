#This script will import and analyse marsupial data


### Import data -----

# import from csv file
dat <- read.csv("./data/marsupials.csv")

# import from csv file which is tidy already

# dat <- read.csv("./data/marsupials.csv", 
#                 na.strings = "_",
#                 col.names = c("Family", "Genus", "Species", "FemaleMass", "PopDensity", "MaxLifeSpan"))

# import from xlxs file
library(readxl)

excel_sheets("./data/marsupials.xlsx")
dat2 <- read_excel("./data/marsupials.xlsx", sheet = "reduced")

str(dat2)

### Tidying data -----
str(dat)

#convert lifespan column into double and "_" into NAs
dat$Maximum.lifespan <- as.double(dat$Maximum.lifespan)

#simplify the column names
names(dat)<-c("Family", "Genus", "Species", "FemaleMass", "PopDensity", "MaxLifeSpan")
str(dat)

### Basic summary statistics -----

# What is the marsupial with the greatest maximum life-span?
dat[which.max(dat$MaxLifeSpan),]

# Does the dataset also contain genera from the Americas, such as Glironia, Didelphis and Marmosa?
c("Glironia", "Didelphis", "Marmosa") %in% dat$Genus

### Visual data exploration ------
datMacropus <- dat[dat$Genus == "Macropus",]
barplot(datMacropus$FemaleMass, names.arg = datMacropus$Species,
        xlab = "Female body mass [g]", horiz = TRUE, las = 1)

#las = 1 procuces horizontal bar names
#horiz = TRUE tells the function to plot horizontal bars

### Summarising data -----

table(dat$Family)

# What is the mean of population for each family?
aggregate(dat$PopDensity, by = list(dat$Family), FUN = mean)
aggregate(PopDensity ~ Family, data = dat, FUN = "mean")


aggregate(dat[c('FemaleMass', 'PopDensity', 'MaxLifeSpan')], by = list(dat$Family, dat$Genus), FUN = median)



aggregate(cbind(FemaleMass, PopDensity, MaxLifeSpan) ~ Family + Genus, 
          data = dat,
          FUN = median)



### Statistical modeling -----

### t-test 
testResult <- t.test(dat$PopDensity[dat$Family == "Macropodidae"],
                     dat$PopDensity[dat$Family == "Dasyuridae"])
testResult

str(testResult)

#remove htest and print as a list
# attributes(testResult) <- NULL
# testResult

testResult$p.value

### Fitting a linear model 

dat$LogFemaleMass <- log(dat$FemaleMass)
dat$LogMaxLifeSpan <- log(dat$MaxLifeSpan)

lmFit <- lm(LogMaxLifeSpan ~ LogFemaleMass, data = dat)
lmFit

str(lmFit)
summary(lmFit)

lmFit$coefficients["LogFemaleMass"]

plot(x = dat$LogFemaleMass, y = dat$LogMaxLifeSpan, 
     xlab = "log(Female body mass [g])", ylab = "log(Maximum life-span [years])",
     pch = 16, col = rgb(0, 0, 0.8, 0.2))
abline(lmFit, col = "red")


### Exporting results and plots

x <- c(3,7,2,1)
save(x, file = "./results/A_little_vector.RData")

y <- 1:17
save(x, y, file = "./results/Two_little_vectors.RData")

load(file = "./results/Two_little_vectors.RData")

write.csv(dat, file = "./results/extended_data.csv")

pdf(file = "linear_fitting.pdf",
    width = 5, height = 4)
plot(x = dat$LogFemaleMass, y = dat$LogMaxLifeSpan, 
     xlab = "log(Female body mass [g])", ylab = "log(Maximum life-span [years])",
     pch = 16, col = rgb(0, 0, 0.8, 0.2))
abline(lmFit, col = "red")
dev.off()


### Exercises ----

#Import data
#3.1
chla <- read.csv("./data/fig/figS3/chla.csv")
biomass <- read.csv("./data/fig/figS3/biomass.csv")
attenuation <- read.csv("./data/fig/figS3/attenuation.csv")
sestonC <- read.csv("./data/fig/figS3/sestonC.csv")

datPhyto <- list(chla = chla, 
            biomass = biomass, 
            attenuation = attenuation, 
            sestonC = sestonC)

#3.3
mean(reefFishFST$FST)
quantile(reefFishFST$FST)
max(reefFishFST$FST)
min(reefFishFST$FST)
median(reefFishFST$FST)

hist(reefFishFST$FST, breaks = seq(from=min(reefFishFST$FST)-0.1, to=max(reefFishFST$FST)+0.1, by=0.1),
     main = "Histogram of Reef Fish FST", xlab = "FST")
abline(v = mean(reefFishFST$FST), col = "red")

FSTdemersals <- reefFishFST$FST[reefFishFST$Egg == "demersal"]
FSTpelagic <- reefFishFST$FST[reefFishFST$Egg == "pelagic"]

aggregate(reefFishFST$FST, by = list(reefFishFST$Egg), FUN = mean)


hist(dat$FemaleMass, breaks = seq(from=min(dat$FemaleMass)-1500, to=max(dat$FemaleMass)+1500, by=1500),
     main = "Histogram of Reef Fish FST", xlab = "FST")
abline(v = mean(dat$FemaleMass),col="red")

#3.4
par(mar=c(0,0,0,0))
plot(0, xlim=c(-3, 3), ylim=c(-3, 3), type='n', axes=FALSE, ann=FALSE)

n<-1000
x<-rnorm(n,0,1)
y<-rnorm(n,0,1)
size<-seq(10, 0.1, length.out = n)

colours<-hsv(h = seq(0, 1, length.out=n), s= 1, v=1)
points(x, y, cex = size, col = colours)

pdf(file = "dots.pdf", width = 5, height = 4)
plot(0, xlim = c(-3, 3), ylim = c(-3, 3), 
     type = 'p', axes = FALSE, ann = FALSE)
points(x, y, col = colours, pch = 8, cex = size)
dev.off()
