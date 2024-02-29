##Create a vector of "stuff" with numbers and characters
stuff <- c(6, "elephant", 4.5, 1, "cat")    ## Create a vector of "stuff" with numbers and characters
stuff                ## Show stuff in the R console
View(stuff)          ## Show stuff as a new tab in the upper left script panel
stuff[2]             ## This calls the second element of the vector, "stuff"
stuff[1]             ## And the first. Note the "" around the 6 - R has not recognised 6 as a number
class(stuff)         ## What type of class is "stuff"? Everything in "stuff" is recognised as a character
stuff[1] + stuff[4]  ## Error message because "6" and "1" are not recognised as numbers
stuff[2:4]           ## The 2nd, 3rd, 4th elements of "stuff"

##Changing between classes 
stuffNumeric <- as.numeric(stuff)         ## There were warnings because "cat" and "elephant" can't be converted to numbers
class(stuffNumeric)                       ## Now it's a numeric vector!
stuffNumeric                              ## See the NAs where "cat" and "elephant" were? R has removed them because it couldn't convert them

stuffNumeric[1] + stuffNumeric[4]         ## Now this works as desired! 1 + 6 = 7

stuffFactor <- as.factor(stuff)           ## Change the type to "factor"
stuffFactor                               ## This is similar to "stuff", but there are now levels. This is IMPORTANT. Notice that the values are now ordered - numerically then alphabetically.
stuffNumeric2 <- as.numeric(stuffFactor)  ## No warnings! Why?
stuffNumeric2                             ## This is now a vector of integers; everything has changed - these numbers are the ORDER that each of our original "stuff" appeared in when we converted to a factor.

levels(stuffFactor)  ## What are the levels of "stuffFactor"
stuffNumeric[3]      ## is 4.5, but
stuffNumeric2[3]     ## is 2! AND,
stuffNumeric2[1] + stuffNumeric2[4]  ## no longer equals 7

##Matrices vs data.frames

Owners <- c("Sally", "Harry", "Beth", "Roger") ## Vector of people's names
Pets <- c("dog", "lizard", "bird", "cat")      ## Vector of the type of pet each person has

OwnersPets <- cbind(Owners, Pets)  ## cbind concatenates these two vectors as columns
OwnersPets        ## Have a look at it
str(OwnersPets)   ## Check the class - here we've used "str" which is useful when you have multiple variables, or columns in your data
dim(OwnersPets)   ## the command "dim" returns the dimensions (rows and columns) of our object         

OwnersPets.df<- data.frame(Owners, Pets)    ## add the vectors to a dataframe
OwnersPets.df         ## Have a look at it
str(OwnersPets.df)    ## Check the structure

##Load data from csv file
zebrafish<-read.csv("Data/zebrafish.csv", header = T)
zebrafish               ## Look at the entire data set
names(zebrafish)        ## List the variables (the column names)
head(zebrafish, n = 4)  ## Look at the first 4 rows of the data
tail(zebrafish, n = 4)  ## Look at the last 4 rows of the data
dim(zebrafish)          ## How many rows and columns are there?

##Plot 
plot(zebrafish$TotalLength, zebrafish$SwimSpeed, col = "tomato", cex=3)

##Looping
words<- c("cuddles", "Barb", "hates", "Ros", "treats", "beards", "bathtime", "loves")
for(i in c(2,8,5)){
  print(words[i])
}

