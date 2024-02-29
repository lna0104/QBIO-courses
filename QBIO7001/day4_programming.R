
### Function -----

divShannon <- function(n) {
  p <- n/sum(n)
  return(-sum(p * log(p)))
}

divShannon(c(100, 500, 300, 100))

divShannon <- function(n, removeNAs = FALSE) {
  p <- n/sum(n, na.rm = removeNAs)
  return(-sum(p * log(p), na.rm = removeNAs))
}

divShannon(c(100, 500, 300, 100, NA), removeNAs = TRUE)

### Flow control ------

### Conditional expressions
ifelse(1:20 < 10, "small", "large")

divSimpson <- function(n, removeNAs = FALSE) {
  p <- n/sum(n, na.rm = removeNAs)
  return(1/sum(p^2, na.rm = removeNAs))
}

div <- function(n, type = "Shannon", removeNAs = FALSE) {
  if (type == "Shannon") {
    return(divShannon(n, removeNAs = removeNAs))
  } else if (type == "Simpson") {
    return(divSimpson(n, removeNAs = removeNAs))
  }
}

# div <- function(n, type = "Shannon") {
#   if (type == "Shannon") return(divShannon(n))
#   else if (type == "Simpson") return(divSimpson(n))
# }

div(c(100, 500, 300, 100, NA), removeNAs = TRUE)

for(i in 1:nrow(marsupials)){
  cat(paste0("Marsupial #", i, ": ", marsupials$Genus[i], " ", marsupials$Species[i], "\n"))
}


### Loop

fibonacci <- function(n) {
  v <- rep(NA, n)
  v[1] <- 0
  v[2] <- 1
  for(i in 3:n) {
    v[i] <- v[i-2] + v[i-1]
  }
  return(v)
}

# example call:
fibonacci(10)

v<-runif(1)
while (sum(v) < 10) {
  v <- c(v, runif(1))
}
v
sum(v)

### Apply and friends
maxValues <- sapply(marsupials[, 4:6], max)
maxValues

quantiles <- sapply(marsupials[, 4:6], quantile, na.rm = TRUE)


### Exercises -----

#4.1

printMess <- function(){
  motivationalMessages <- c(
    "You're doing great!",
    "Believe in yourself and all that you are.",
    "Stay positive. Work hard. Make it happen.",
    "You are capable of amazing things.",
    "Every small step counts. Keep going!"
  )
  selectedMessage <- sample(motivationalMessages,1)
  print(selectedMessage)
}

printMess()
#4.2 

startString <- "lena"
splittedChars <- strsplit(startString, "")[[1]]

reversedChars <- splittedString[length(splittedChars):1]
reversedChars <- rev(splittedChars)

reversedString <- paste0(reversedChars, collapse = "")

if (reversedString == startString){
  print("The string is a palindrome")
} else {
  print("The string is not a palindrome")
}



palindromeTest <- function(s){
  splittedChars <- strsplit(s, "")[[1]]
  reversedChars <- rev(splittedChars)
  reversedString <- paste0(reversedChars, collapse = "")
  
  if (reversedString == s){
    print("The string is a palindrome")
  } else {
    print("The string is not a palindrome")
  }
  
}

palindromeTest("kayak")

#4.3

meanMedianTest <- function(x,y){
  if (((mean(x) < mean(y)) & (median(x) > median(y))) | 
      ((mean(x) > mean(y)) & (median(x) < median(y)))){
      return(TRUE)
  } else {
    return(FALSE)
  }
}

n <- 10000
numberCases <- 0

for (i in 1:n){
  x <- rnorm(100)
  y <- rnorm(100)
  if (meanMedianTest(x,y)) {
    numberCases <- numberCases + 1
  }
  
}

numberCases/n

x <- lapply(rep(100, n), rnorm)
y <-  lapply(rep(100, n), rnorm)

numberCases <- sum(mapply(meanMedianTest, x, y))
numberCases/n


#4.4

n<-1000

{
  t <- c(0, rep(NA, n-1))
  for (i in (2:n)){
    t[i] <- t[i-1] + rnorm(1)
  }
  plot(t, type = "l")
}

{
  n<-1000
  m <- matrix(NA, nrow = n, ncol =2)
  m[1,] <- c(0,0)
  
  for (i in (2:n)){
    m[i,] <- m[i-1,] + rnorm(2)
  }
  plot(x = m[,1] , y= m[,2])
}


t <- c(0, cumsum(rnorm(n-1)))

{
  m <- matrix(rnorm(2*n), nrow = n, ncol = 2)
  m[1,] <- c(0,0)
  m <- apply(m, 2, cumsum)
}

#4.5

sum(c(1, 2, 3), rm.na = TRUE)

 
sum(c(1, 2, 3), na.rm = TRUE)

#4.7
shuffleFirstElements <- function(data, n) {
  c(sample(data[1:n]), data[(n+1):length(data)])
}

data <- 500:550
n<-5

shuffleFirstElements(data, n)
