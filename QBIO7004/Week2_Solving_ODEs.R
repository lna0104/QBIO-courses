install.packages("deSolve")
library(ggplot2)
library(deSolve)
library(tidyverse)

logisticODE <- function(times, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN <- r * N * (1 - N/K)
    return(list(c(dN))) 
  })
}

times <- seq(0, 100, by = 0.1)
parameters <- c(r = 0.2, K = 1000)
iniState <- c(N = 10)

solution <- ode(iniState, times, logisticODE, parameters)
head(solution)
plot(solution, xlab = "Time", ylab = "Population size", type = "l")

# This procedure readily extends from single ODEs to systems of ODEs. 
# For example, consider the Lotka-Volterra model for predator-prey-dynamics:
LotkaVolterraODE <- function(times, state, parameters) {
  with(as.list(c(state, parameters)), {
    dX <- r*X - beta*X*Y
    dY <- -delta*Y + gamma*X*Y
    return(list(c(dX, dY))) 
  })
}

times <- seq(0, 100, by = 0.1)
parameters <- c(r = 0.5, beta = 1, delta = 1, gamma = 1)
iniState <- c(X = 1, Y = 1)

solution <- ode(iniState, times, LotkaVolterraODE, parameters)
plot(x = solution[,"time"], y = solution[,"X"], xlab = "Time", ylab = "Population size", type = "l", ylim = c(0,2))
lines(x = solution[,"time"], y = solution[,"Y"], col = "red")

### Practical tasks ------
### Task 2.1:
### 2.1.a
logisticEq <- function(t, N0, r, K) {
  Nt <- N0 * K * exp(r * t) / (K + N0 * (exp(r * t) - 1))
  return(Nt)
}

# Set parameters
N0 <- 1
r <- 0.1
K <- 1000

# Generate time points from 0 to 100 in increments of 0.1
times<- seq(0, 100, by = 0.1)
# Calculate solution values using the logistic_solution function
solutionValues <- logisticEq(times, N0, r, K)

# Plot the solution
plot(times, solutionValues, type = 'l', col = 'blue', xlab = 'Time', ylab = 'Population Size', main = 'Logistic ODE Solution')


### 2.1.b: Using the code developed in section 2.6
logisticODE <- function(times, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN <- r * N * (1 - N/K)
    return(list(c(dN))) 
  })
}

parameters <- c(r = 0.1, K = 1000)
iniState <- c(N = 1)

logisticSolution <- ode(iniState, times, logisticODE, parameters)

lines(x = logisticSolution[,"time"], y = logisticSolution[,"N"], col = "red", lty = 3)

###2.1.c

EulerFunction <- function(N0, r, K, h, t){
  nSteps <- round(t/h)  
  times <-  seq(0, t, by = h)
  N <- numeric(length = nSteps + 1)
  N[1] <- N0
  for (i in 1:nSteps){
    dN <- r*N[i]*(1-N[i]/K)
    N[i+1] <- N[i] + h*dN
  }
  return(list(times = times, solution = N))
}

# Set parameters

EulerResult_5 <- EulerFunction(N0 = 1, r = 0.1, K = 1000, h = 5, t = 100)
EulerResult_1 <- EulerFunction(N0 = 1, r = 0.1, K = 1000, h = 1, t = 100)
EulerResult_01 <- EulerFunction(N0 = 1, r = 0.1, K = 1000, h = 0.1, t = 100)
EulerResult_001 <- EulerFunction(N0 = 1, r = 0.1, K = 1000, h = 0.01, t = 100)


plot(EulerResult_5$times, EulerResult_5$solution, type = 'l', col = 'red')
lines(EulerResult_1$times, EulerResult_1$solution, type = 'l', col = 'orange')
lines(EulerResult_01$times, EulerResult_01$solution, type = 'l', col = 'green')
lines(EulerResult_001$times, EulerResult_001$solution, type = 'l', col = 'blue')

library(reshape2) 
df <- data.frame(times, solutionValues, logisticSolution[,"N"], EulerResult_1$solution) %>%
  `colnames<-`(c('times', 'a','b','c')) %>%
  melt(., id.vars='times',  
       measure.vars=c('a','b','c')) 
df %>% ggplot(aes(x=times, y=value, fill=variable)) + 
  geom_boxplot()


### Task 2.2.a 
siModel_a <- function(times, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta*S*I
    dI <- beta*S*I - gamma*I
    dR <- gamma*I
    return(list(c(dS, dI, dR))) 
  })
}

# Set time points
times <- seq(0, 300, by = 1)

# Set initial conditions and parameters
parameters <- c(beta = 0.3, gamma = 0.1)
iniState <- c(S = 0.99, I = 0.1, R = 0)

siSolution_a <- ode(iniState, times, siModel_a, parameters)

#par(mfrow = c(3, 1))

plot(x = siSolution_a[,"time"], y = siSolution_a[,"S"], col="blue", type="l", ylab="y", xlab ='Time', main = "Task 2.2.a")
lines(x = siSolution_a[,"time"], y = siSolution_a[,"I"], col="red")
lines(x = siSolution_a[,"time"], y = siSolution_a[,"R"], col="orange")
# Add a legend
legend("topright", legend = c("S", "I", "R"), 
       col = c("red", "blue", "orange"), lty = 1)

### Task 2.2.b: add the disease kills infected people at a certain rate α

siModel_b <- function(times, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta*S*I
    dI <- beta*S*I - gamma*I - alpha*I
    dR <- gamma*I
    return(list(c(dS, dI, dR))) 
  })
}

# Set initial conditions and parameters
parameters <- c(beta = 0.3, gamma = 0.1, alpha = 0.05)
iniState <- c(S = 0.99, I = 0.1, R = 0)

siSolution_b <- ode(iniState, times, siModel_b, parameters)
plot(x = siSolution_b[,"time"], y = siSolution_b[,"S"], col="blue", type="l", ylab="y", xlab ='Time', main = "Task 2.2.b")
lines(x = siSolution_b[,"time"], y = siSolution_b[,"I"], col="red")
lines(x = siSolution_b[,"time"], y = siSolution_b[,"R"], col="orange")
# Add a legend
legend("topright", legend = c("S", "I", "R"), 
       col = c("red", "blue", "orange"), lty = 1)

### Task 2.2.c: add a constant influx of susceptible individuals into the population +
# a natural mortality for all classes of individuals

siModel_c <- function(times, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- influx - beta*S*I - mu*S
    dI <- beta*S*I - gamma*I - alpha*I - mu*I
    dR <- gamma*I -  mu*R
    return(list(c(dS, dI, dR))) 
  })
}

# Set initial conditions and parameters
parameters <- c(beta = 0.3, gamma = 0.1, alpha = 0.05, mu = 0.001, influx = 0.002)
iniState <- c(S = 0.99, I = 0.1, R = 0)

siSolution_c <- ode(iniState, times, siModel_c, parameters)
plot(x = siSolution_c[,"time"], y = siSolution_c[,"S"], col="blue", type="l", ylab="y", xlab ='Time', main = "Task 2.2.c")
lines(x = siSolution_c[,"time"], y = siSolution_c[,"I"], col="red")
lines(x = siSolution_c[,"time"], y = siSolution_c[,"R"], col="orange")
# Add a legend
legend("topright", legend = c("S", "I", "R"), 
       col = c("red", "blue", "orange"), lty = 1)

### Solutions -----

### 2.1.a
solution <- function(t, N0, K, r) {
  N0*K*exp(r*t)/(K+N0*(exp(r*t) - 1))
}

timesAna <- seq(0, 100, by=0.1)
parameters <- c(r=0.2, K=1000)
iniState <- c(N=1)
solutionAna <- solution(timesAna, iniState, parameters["K"], parameters["r"])
plot(x = timesAna, y = solutionAna, xlab = "Time", ylab = "Population size", type = "l", main = "")

### 2.1.b
logisticODE <- function(times, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN<-r*N*(1-N/K)
    return(list(c(dN))) 
  })
}
timesNum <- seq(0, 100, by=0.1)
solutionNum<-ode(iniState, timesNum, logisticODE, parameters)
plot(x = timesAna, y = solutionAna, xlab = "Time", ylab = "Population size", type = "l", main = "")
lines(x = timesNum, y = solutionNum[,"N"], col = "red")

### 2.1.c
stepSize <- c(5, 1, 0.1, 0.001)
solutionEuler <- vector(mode = "list", length = length(stepSize))
plot(x = timesAna, y = solutionAna, xlab = "Time", ylab = "Population size", type = "l", main = "")
for(k in 1:length(solutionEuler)) {
  solutionEuler[[k]] <- data.frame(times = seq(0, 100, by = stepSize[k]), N = NA)
  solutionEuler[[k]]$N[1] <- iniState
  for(i in 1:(nrow(solutionEuler[[k]])-1)) {
    currentN <- c(N = solutionEuler[[k]]$N[i])
    solutionEuler[[k]]$N[i + 1] <- currentN + 
      stepSize[k]*logisticODE(NA, currentN, parameters)[[1]]
  }
  lines(x = solutionEuler[[k]]$times, y = solutionEuler[[k]]$N, col = "blue")
}

### 2.1.d

diffs <- data.frame(method = c("deSolve", paste0("Euler_", stepSize)),
                    difference = NA)
diffs$difference[1] <- abs(solutionNum[nrow(solutionNum), "N"] - 
                             solutionAna[nrow(solutionNum)])
for(k in 1:length(solutionEuler)) {
  diffs$difference[k + 1] <- abs(solutionEuler[[k]]$N[nrow(solutionEuler[[k]])] - 
                                   solutionAna[nrow(solutionNum)])
}

barplot(diffs$difference, names.arg = diffs$method, log = "y")
box()

