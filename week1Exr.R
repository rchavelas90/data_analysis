# Week 1 Notes

## Getting Help https://dl.dropboxusercontent.com/u/7710864/002gettingHelp/index.html#1
?rnorm
help.search("rnorm")
args("rnorm")
rnorm

## advanced data analysis from and elemntry point of view

# Representing data in R

## Classes
###character
firstName  <- "Jeff"
class(firstName)
firstName
###numeric
heightCM <- 188.2
class(heightCM)
heightCM
###Integer
numberSons <- 1L
class(numberSons)
numberSons
###Logical
teachingCoursera <- TRUE
class(teachingCoursera)
teachingCoursera

##Objects
###Vectors
heights <- c(188.2,181.3,193.4)
heights

firstNames <- c("jeff","roger","andrew","brian")
firstNames

##Lists
vector1 <- c(188.2,181.3,193.4)
vector2 <- c("jeff","roger","andrew","brian")
myList <- list(heights = vector1,firstNames = vector2)
myList

##matrices
myMatrix <- matrix(c(1,2,3,4),byrow=T,nrow=2)
myMatrix

## Data frames
vector1 <- c(188.2,181.3,193.4,192.3)
vector2 <- c("jeff","roger","andrew","brian")
myDataFrame <- data.frame(heights = vector1,firstNames = vector2)
myDataFrame

##factors
smoker <- c("yes","no","yes","yes")
smoker
smokerFactor <- as.factor(smoker)
smokerFactor

##Missing values (NA) 
vector1 <- c(188.2,181.3,193.4,NA)
vector1
is.na(vector1) ## which of the values is missing

## Subsetting
vector1 <- c(188.2,181.3,193.4,192.3)
vector2 <- c("jeff","roger","andrew","brian")
myDataFrame <- data.frame(heights = vector1,firstNames = vector2)
vector1[1]
vector1[c(1,2,4)]
myDataFrame[1,1:2]
myDataFrame$firstNames
myDataFrame[vector2=="jeff",]
myDataFrame[vector1<190,]


# Simulation (Idealized datasets for data analysis)
## Genearte random variables from a particular distribution
### r... distribution

## Denisties
### d... distribution

## sampling...
# rfoo foo= distribution
args(rnorm)
heights <- rnorm(10,mean=188,sd=3)
heights

### binomial
args(rbinom)
coinFlips <- rbinom(10,size=10,prob=0.5)
coinFlips

### values of a density
args(dnorm)
x <- seq(from=-5,to=5,length=10)
normalDensity <- dnorm(x,mean=0,sd=1)
round(normalDensity,2)
plot(normalDensity)

### discrete
args(dbinom)
x <- seq(from=0,to=10,by=1)
binomialDensity <- dbinom(x,size=10,
                          prob=0.5)
round(binomialDensity,2)
plot(binomialDensity)

## Sample from variables
## already generated

args(sample)
heights <- rnorm(10,mean=188,
                sd=3)
heights
a <- sample(heights,size=10,
       replace=T)
b <- sample(heights,size=10,
       replace=F)
heights[order(heights)]==
 b[order(b)]

## Sample according to be probs

heights
probs <- c(0.4,
           0.3,
           0.2,
           0.1,
           0,
           0,
           0,
           0,
           0,
           0)
sum(probs)
sample(heights,size=10,
       replace=T,
       prob=probs)

## set a sp eed
set.seed(12345)
rnorm(5,mean=0,sd=1)

set.seed(5)
sample(1:8,size=4,replace=F)

##Convenience sample
probs <- c(5,5,5,5,1,1,1,1)/16
sum(probs)
sample(1:8,size=4,replace=F,prob=probs)

## Randomized trail
### causal analysis
treat1 <- sample(1:8,size=2,replace=FALSE,
                 )
treat2 <- sample(2:7,size=2,replace=FALSE,
)
c(treat1,treat2)

## Quiz
set.seed(31);
heightsCM = rnorm(30,mean=188, sd=5);
weightsK = rnorm(30,mean=84,sd=3); 
hasDaughter = sample(c(TRUE,FALSE),size=30,replace=T); 
dataFrame = data.frame(heightsCM,weightsK,hasDaughter);
dataFrameSubset <- dataFrame[dataFrame$heightsCM>188,]
mean(dataFrameSubset$weightsK)

set.seed(41)
cauchyValues <- rcauchy(100)
set.seed(415)
sampCauchy <- sample(cauchyValues,10,replace=T)
head(sampCauchy,3)



