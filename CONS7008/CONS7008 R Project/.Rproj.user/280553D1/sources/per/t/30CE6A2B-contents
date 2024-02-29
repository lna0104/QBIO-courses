#Practical 10: Principal Component Analysis
##10.1. Simple correlated data
zebrafish <- read.csv("Data/zebrafish.csv")
head(zebrafish) #have a look at the data - we will initially work with just the first two of the continuous variables ("TotalLength" and "TailDepth")
raw_data<-as.data.frame(zebrafish[,c("TotalLength", "TailDepth")]) #subset the data to two columns, and tell R that it is a dataframe

## Now produce a "scatter plot"
with(raw_data, plot(TotalLength, TailDepth, xlab="Y1", ylab= "Y2")) ## Scatter plot the two variables

std_function<-function(x) ((x-mean(x, na.rm=T)) / sd(x, na.rm=T))
## here "x" is just a place holding name for whatever variable we feed to this new function
## we could have used "ros" or "barb" - its just a name

## make a copy of raw data before we standardise the variables
std_data<-raw_data

## now standardise the two variables using our flash new function!
std_data$TotalLength<-std_function(x=std_data$TotalLength)
std_data$TailDepth<-std_function(x=std_data$TailDepth)

## Now plot the standardised variables
with(std_data, plot(TotalLength, TailDepth, xlab="Y1", ylab= "Y2")) ## Scatter plot the two variables

cov_mat <- round(var(std_data), 3)  ## Estimate the sample covariance matrix 
cov_mat         ## Look at the sample variances (diagonals) and the covariance (off diagonal) 

var(raw_data)
##10.2. Turning our correlated data into uncorrelated data

PCA <- eigen(cov_mat)           ## Calculate eigenvalues and eigenvectors  
round(PCA$values, 3)            ## Look at the eigenvalues  
round(PCA$vectors, 2)           ## Look at the eigenvectors 

##10.3 Principal Components (Eigenvectors)
## Make the plot dimensions square ("s") to help us to visualise the new axes
par(pty = "s")

## Set the range for the two axes
AxisRange <- range(std_data)

## Plot our standardised variables again
plot(x = std_data$TotalLength,  y = std_data$TailDepth, ## Plot the two variables
     xlim = AxisRange,        ## Set the range of Y1 values
     ylim = AxisRange,        ## Set the range of Y2 values to be identical to the range of Y2 values
     xlab = "TotalLength (std)",    #label the axes to so you remember what we are plotting
     ylab = "TailDepth (std)")        

## Now plot straight lines that go through the origin (0, 0)
## and the coordinates of the eigenvectors (one at a time)
## Recall that the slope of a straight line is equal to the rise/run:
abline(0, PCA$vectors[2, 1]/PCA$vectors[1, 1], col = "red")   ## Plot a line the for the first PC in red
abline(0, PCA$vectors[2, 2]/PCA$vectors[1, 2], col = "blue")  ## Plot a line the for the second PC in blue

## Reset the plot dimensions
par(pty = "m")

test<-princomp(std_data)
loadings(test)
summary(test)

biplot(test, col=c("grey","red"))

##10.4. PC scores

## Step 1: multiply fish 1's value for Y1 by PC1's loading for Y1
std_data$TotalLength[1] * PCA$vectors[1, 1] 

## Step 1: multiply fish 1's value for Y2 by PC1's loading for Y2
std_data$TailDepth[1] * PCA$vectors[2, 1] 

## Step 3, sum the two products to calculate the PC1 score for the 1st fish in the dataset
std_data$TotalLength[1] * PCA$vectors[1, 1]  + std_data$TailDepth[1] * PCA$vectors[2, 1]

PC_scores <- as.matrix(std_data) %*% PCA$vectors ## calculate PC scores for all objects and PCs
colnames(PC_scores)=c("PC1", "PC2")
head(PC_scores)   ## Look at the first six objects

## Note: we needed to tell R to treat std_data as a matrix for the "%*%" to work

cov_matPCA <- var(PC_scores)  ## Calculate the covariance matrix and store it
round(cov_matPCA, 3)  ## Look at the variance covariance matrix of PC_scores 

Range <- range(std_data, PC_scores) ## Prepare the plotting range

par(mfcol = c(1, 2))                ## Set up space for two graphs, side by side 
plot(x = std_data$TotalLength,  y = std_data$TailDepth,  ## Plot the original data 
     xlim = Range,          ## Set the axes range
     ylim = Range,              ## Set the axes range 
     xlab = 'TotalLength',      ## Label the vertical axis 
     ylab = 'TailDepth')           ## Label the horizontal axis

plot(PC_scores[, 1], PC_scores[, 2],    ## Plot the new data 
     xlim = Range,            ## Set the axes range
     ylim = Range,              ## Set the axes range
     xlab = 'First principal component',            ## Label the vertical axis 
     ylab = 'Second principal component')       ## Label the horizontal axis 

par(mfcol = c(1, 1))                    ## Reset to just one graph per page 

## Sum of the variances (i.e. total variance)
## We can grab the variances from the "diagonal" of the covariance matrix!
sum(diag(cov_mat)) 

## Sum the eigenvalues
PCA$values[1] + PCA$values[2] 

##10.5. Eigenvector loadings and understanding PC scores

PCA$values

##10.6 Zero eigenvalue
## Make sure R treats this as a dataframe
std_data <- as.data.frame(std_data)  

## Generate a new, third variable, that is totally determined by the first variable 
std_data$Y3 <- std_data$TotalLength ## Copy the data from the first column into a new column
pairs(std_data) ## Scatterplot each raw data variable against every other 
cor(std_data)   ## Look at the correlation matrix 

cov_mat2 <-  var(std_data)      ## Estimate the sample covariance matrix 
PCA2 <- eigen(cov_mat2)   ## Extract the eigenvalues and eigenvectors 
round(PCA2$values, 4)       ## Look at the eigenvalues 

PC_scores <- as.matrix(std_data) %*% PCA2$vectors  ## Convert the data to PC_scores 
round(var(PC_scores), 4)   

rm(list = ls()) 
