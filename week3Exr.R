# Exploratory Analysis
## Graphing (just for me)
## easy ways and efficient to communicate information

## Housing data
pData <- populationData
head(pData)

## Boxplot (distribution, cuantitative1)
boxplot(pData$AGEP,col="blue")
head(pData$AGEP)
### Median - center and soooooo on[simetry, concentrated, variablity...]

## Boxplots for camparisons (common scale)
boxplot(pData$AGEP~as.factor(pData$DDRS),col="blue")
### Missing?  numbers

## Boxplots and number in each category
### How many individuals we have in each DDRS factor
boxplot(pData$AGEP~as.factor(pData$DDRS),col=c("blue","orange"),names=c("yes","no"),
        varwidth=T) ## widht of the plot is proportional to the number of observations that
                    # we have in a group

##Barplots (postion{better} vs length)
table(pData$CIT)
barplot(table(pData$CIT),col="blue") ## table makes a frequency table(or count of each level)
## see the number of observations in each class (more calitaitative)

## HIstograms
hist(pData$AGEP,col="blue")
# Sort of boxplots
## Goal univariate data
## Distribution and SHAPE  <-  IMPORTANT

hist(pData$AGEP,col="blue",breaks=100,main="Age")
## set the breaks

## Density plots
### like a histogram but smoothed out
dens <- density(pData$AGEP)
plot(dens,lwd=3,col="blue") ## lwd = thick 
## Density = Shape similar but instead of the exact number, it looks the porcentagep
## carefull with the boundaries
## Dens vs Hist: Overlay the distributions

## Multiple distributions
dens <- density(pData$AGEP)
densMales <- density(pData$AGEP[which(pData$SEX==1)])
plot(dens,lwd=3,col="blue") ## lwd = thick 
lines(densMales,lwd=3,col="orange") ## add another line

### Exploratory graphs part 2
pData <- populationData
head(pData)

## Scatterplot: Visualize relationship between variables
plot(pData$JWMNP,pData$WAGP,pch=19,col="blue")
### See some weird patterns

## Size matters, size of the axis matter
plot(pData$JWMNP,pData$WAGP,pch=19,col="blue",cex=0.5)

## Using color
plot(pData$JWMNP,pData$WAGP,pch=19,col=pData$SEX,cex=0.5)
### NOTE THAT COLOR VECTOR MUST BE INTEGER VALUES
### DIFERENCE BETWEEN A VARIABLE (third variable of the relationship)

## use size for third variable (better with colors)
percentMaxAge <- pData$AGEP/max(pData$AGEP)
plot(pData$JWMNP,pData$WAGP,pch=19,col="blue",cex=percentMaxAge*2)

## Overlay lines or points
plot(pData$JWMNP,pData$WAGP,pch=19,col="blue",cex=0.5)
lines(rep(100,dim(pData)[1]),pData$WAGP,col="grey",lwd=5)
points(seq(0,200,length=100),seq(0,20e5,length=100),col="red",pch=19)


## Visualize numeric vriables as factors
## break down a numeric vector to a factor
library(Hmisc)
ageGroups <- cut2(pData$AGEP,g=5)
plot(pData$JWMNP,pData$WAGP,pch=19,col=ageGroups,cex=1)

## have a lot of points
x <- rnorm(1e5)
y <- rnorm(1e5)
plot(x,y,pch=19)

## Try different things
### sampling
x <- rnorm(1e5)
y <- rnorm(1e5)
sampledValues <- sample(1:1e5,size=500,replace=F) ## use an index to subset
plot(x[sampledValues],y[sampledValues],pch=19)

### SmoothScatter
x <- rnorm(1e5)
y <- rnorm(1e5)
smoothScatter(x,y) ## Creates a smooth density plot (with individual outlines)
## See the relationship wo looking at all the info

## Hexbin
library(hexbin)
x <- rnorm(1e5)
y <- rnorm(1e5)
hbo <- hexbin(x,y)  ## hecbin counts the number of points in the hexagon 
plot(hbo)
### visualize lots of points

##QQ-plots
x <- rnorm(20)
y <- rnorm(20)
qqplot(x,y) 
abline(c(0,1))## two parameters: Intercept and slope
## Purpose (2 quantitative variables) plot quantiles of X vs quantiles of Y
## (up to the 100% percental)
## the smallest quantile(%) of x to the same of Y

## Matplots and spaghetti plots
## overtime or longitudinaly
X <- matrix(rnorm(20*5),nrow=20)
X
matplot(X,type="b") ## reads a column and plot it as a specific line
## compare treds or trajectories over time 
## longitudinal or time series (spaguetti plots)

## Heatmaps
## 2D histogram
pData[1:10,161:236]
image(1:10,161:236,as.matrix(pData[1:10,161:236])) 
#### The brighter de color, the larger de value
### color represents the insentity or the size

## Match intuiton (Rows to rows and cols to cols)
## It is a bit confusing the other war
newMatrix <- as.matrix(pData[1:10,161:236])
newMatrix <- t(newMatrix)[,nrow(newMatrix):1] ## reordered
image(161:236,1:10,newMatrix) 

## Maps 
library(maps)
map("world")## plots greographic with countries
lat <- runif(40,-180,180); lon <- runif(40,-90,90)
points(lat,lon,col="blue",pch=19)

## missing values
## one pattern is missing values
x <- c(NA,NA,NA,4,5,6,7,8,9,10)
y <- 1:10
plot(x,y,pch=19,xlim=c(0,11),ylim=c(0,11))
### does not plot missing values (it skips it, take care)

## See the distribution of NA's
x <- rnorm(100)
y <- rnorm(100)
y[x<0] <- NA
boxplot(x~is.na(y)) ## is there a relationship between the missing values in y and the values
## of x, relationship between misiing values and other vairables
plot(x,y)