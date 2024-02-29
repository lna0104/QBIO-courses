### Chapter 1: Vector -----
rep(5, 10)

rep(c(5,10), 10)

rep(c(5,10), each = 10)

rep(5:7, 3:1)

runif(10, min = 0, max = 100)

sample(c(1,4,7,8,10),3)
sample(c(1,4,7,8,10),10)
sample(c(1,4,7,8,10),10, replace = TRUE)

x <- 1:5
y <- 6:10
x + y
x*y

x + 7

1:10 * 2

2^(1:10)

2^1:10

1:10 + c(0,1)

# 1 2 3 4 5 6 7 8 9 10
# 0 1 0 1 0 1 0 1 0 1
# 1 3 3 5 5 7 7 9 9 11

#It works
1:10 + 1:2
#It gives error
1:10 + 1:3

# 1 2 3 4 5 6 7 8 9 10
# 1 2 3 1 2 3 1 2 3 1

log(10, base = c(2,3,10))

y<-1:10
log(y, base = c(2,10))

log2(1)
log10(2)
log2(3)
log10(4)

sum(y)    
mean(y)
var(y)
median(y)

x<-1:3
y<-4:6
sum(x*y) #Scalar product of x and y

x<- 4L #Integer

amphibians <- c("frog", "toad", "salamander")
nchar(amphibians)

colours <- c("yellow", "green", "black")
paste(colours, amphibians)

paste("Hello", "How are you?", sep = ', ')
paste(amphibians, colours, sep = ": ")
paste(colours, amphibians, collapse = ", ")

paste0("Hello", "How are you?")

x<-1:100

x == 5 

b1 <- TRUE
b2 <- FALSE

b1 | b2
b1 & b2  

!b1
!b2

b1 & (!b2)

any(1:100 < 0)
all(1:100 > -5)

x <- 1:3
y <- rep(2, 6)
x + y
seq(10, 20, by = 5) < 17


coordinates <- c(x=1, y=4, z=7)
names(coordinates)

coordinates <- c(1, 4, 7)
names(coordinates) <- c("x", "y", "z")

coordinates <- unname(coordinates)

weirdMammalWeights <- c(platypus = 1484, giantArmadillo = 45.4, nakedMoleRat = 55)
attr(weirdMammalWeights, "units") <- c("g", "kg", "g")
attr(weirdMammalWeights, "info") <- "This vector contains the weights of some really weird mammals."
attr(weirdMammalWeights, "source") <- "Encyclopedia of Life, at www.eol.org"

weirdMammalWeights[1]
weirdMammalWeights[[1]]

attributes(weirdMammalWeights)

### Exercises
#1.2
x <- rnorm(1000000, 1, 1)
x <- runif(1000000, 0, 2)
mean(x)
median(x)
var(x)

y <- x^2
mean(y)
median(y)
var(y)
#1.3
b <- c("ostrich", "emu", "cassowary")
paste(b, collapse = " > ")

paste0(1:3, ".", b, collapse = ", ")

n<-nchar(b)

paste(b, "has", n, "letters", collapse = ", ")

cols<- c("blue","red","green")

paste(rep(cols, each = 3), rep(b,3))

#1.4
n <- 12
chars <- c(letters, LETTERS, 0:9)
rndChars <- sample(chars, n, replace = TRUE)
paste0(rndChars, collapse = '')

#1.5
x<-factor(sample(b, 20, replace = TRUE))

typeof(x)
as.character(x)

attributes(x) <- NULL
x

#1.6
x <- NA
typeof(x)
typeof(c(x, NA))
sum(x)
### Chapter 2: Data -----
library(boilrdata)
drosophilaWingG

vars <- diag(drosophilaWingG)
GvarsOnly <- diag(vars) #Covariance
GcorsOnly <- drosophilaWingG - GvarsOnly

drosophilaWingG %*% drosophilaWingG #matrix mutiplication

genera <- unique(marsupials$Genus)[1:7]
names(genera) <- paste0("Genus", LETTERS[1:7])
genera

str(reefFishPhylogeny)

reefFishPhylogeny["tip.label"][1:5]


### Exercises
ls("package:boilrdata")

#2.2

