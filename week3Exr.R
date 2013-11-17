populationData <- read.csv("./data/americanCommunityPop.csv")
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

# Expository Graphs
pData <- populationData

## Axes
plot(pData$JWMNP, pData$WAGP,pch=19,col="blue",cex=0.5,
xlab="Travel time (min)", ylab="Last 12 month wages (dollars)")
## Never R oputut as axis (and UNITS!)

## Axes
plot(pData$JWMNP, pData$WAGP,pch=19,col="blue",cex=0.5,cex.lab=2,cex.axis=1.5,
     xlab="Travel time (min)", ylab="Last 12 month wages (dollars)")
## be careful with presentation

## Legends
plot(pData$JWMNP, pData$WAGP,pch=19,col="blue",cex=0.5,xlab="TT (min)", ylab="Wages (Dlls)")
legend(100,200000,legend="All surveyed",col="blue",pch=19,cex=0.5)
## Upper left hand corner, same parameters of point as before

## more than one point
plot(pData$JWMNP, pData$WAGP,pch=19,cex=0.5,xlab="TT (min)", ylab="Wages (Dlls)",
     col=pData$SEX)
legend(100,200000,legend=c("men","women"),col=c("black","red"),
       pch=c(19,18),cex=c(0p.5,0.5))

## Titles
## MUST INCLUDE! EITHER type of plot or conclusion OF PLOT
plot(pData$JWMNP, pData$WAGP,pch=19,cex=0.5,xlab="TT (min)", ylab="Wages (Dlls)",
     col=pData$SEX, main="Wages earned versus commute time")
legend(100,200000,legend=c("men","women"),col=c("black","red"),
       pch=c(19,19),cex=c(0.5,0.5))

## multiple panels (done to communicate several components)
resetPar <- function() {
 dev.new()
 op <- par(no.readonly = TRUE)
 dev.off()
 op
}
resetPar()
## use pannels for data related in some ways to see some diferent aspects
par(mfrow=c(1,2))
hist(pData$JWMNP,xlab="CT (min)",col="blue",breaks=100,main="")
plot(pData$JWMNP, pData$WAGP,pch=19,cex=0.5,xlab="TT (min)", ylab="Wages (Dlls)",
     col=pData$SEX)
legend(100,200000,legend=c("men","women"),col=c("black","red"),
       pch=c(19,18),cex=c(0.5,0.5))

## Adding text to the margins or to the plots (mtext() command adds text to the
## margins of a figure)
par(mfrow=c(1,2))
hist(pData$JWMNP,xlab="CT (min)",col="blue",breaks=100,main="")
mtext(text="(a)",side=3,line=1) ## 3 to the top and 1 far from from the edge (in lines)
plot(pData$JWMNP, pData$WAGP,pch=19,cex=0.5,xlab="TT (min)", ylab="Wages (Dlls)",
     col=pData$SEX)
legend(100,200000,legend=c("men","women"),col=c("black","red"),
       pch=c(19,18),cex=c(0.5,0.5))
mtext(text="(b)",side=3,line=1)

## Figure captions
### Include: NUmber, bolded text with description of the plot and for each subcomponent
### describe what we see. Captions should be self-containing so anybody that sees the 
## graph can look at the caption alone and undesrtand what is being tried to cummunicate

## Colorblindess (Visible for them)

##Now saving the file
### PDF
pdf(file="./plots/twoPanel.pdf",height=4,width=8) ## as it is 2 panel it must be the double (inches)
par(mfrow=c(1,2))
hist(pData$JWMNP,xlab="CT (min)",col="blue",breaks=100,main="")
mtext(text="(a)",side=3,line=1) ## 3 to the top and 1 far from from the edge (in lines)
plot(pData$JWMNP, pData$WAGP,pch=19,cex=0.5,xlab="TT (min)", ylab="Wages (Dlls)",
     col=pData$SEX)
legend(100,200000,legend=c("men","women"),col=c("black","red"),
       pch=c(19,18),cex=c(0.5,0.5))
mtext(text="(b)",side=3,line=1)
dev.off() ## done with the device

### PNG
png(file="./plots/twoPanel.png",height=480,width=(480*2)) ## pixels
par(mfrow=c(1,2))
hist(pData$JWMNP,xlab="CT (min)",col="blue",breaks=100,main="")
mtext(text="(a)",side=3,line=1) ## 3 to the top and 1 far from from the edge (in lines)
plot(pData$JWMNP, pData$WAGP,pch=19,cex=0.5,xlab="TT (min)", ylab="Wages (Dlls)",
     col=pData$SEX)
legend(100,200000,legend=c("men","women"),col=c("black","red"),
       pch=c(19,18),cex=c(0.5,0.5))
mtext(text="(b)",side=3,line=1)
dev.off() ## done with the device

## sometimes the plot is made different because of the height
## tweak to make exactly as I look it
par(mfrow=c(1,2))
hist(pData$JWMNP,xlab="CT (min)",col="blue",breaks=100,main="")
mtext(text="(a)",side=3,line=1) ## 3 to the top and 1 far from from the edge (in lines)
plot(pData$JWMNP, pData$WAGP,pch=19,cex=0.5,xlab="TT (min)", ylab="Wages (Dlls)",
     col=pData$SEX)
legend(100,200000,legend=c("men","women"),col=c("black","red"),
       pch=c(19,18),cex=c(0.5,0.5))
dev.copy2pdf(file="./plots/twoPanelv2.pdf")

# Herarchical clustering
set.seed(1234)
par(mar=c(0,0,0,0))
x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=2)
text(x+0.05,y+0.05,labels=as.character(1:12))

## Distance
dataFrame <- data.frame(x=x,y=y)
a <- dist(dataFrame) ## calculate the distance of the points
min(a)

## Dendogram
dataFrame <- data.frame(x=x,y=y)
distxy <- dist(dataFrame)
hclustering <- hclust(distxy) ## apply this to a distance object
unclass(hclustering)
plot(hclustering)
## the tree must be cutted up at some point to define the clustersp

myplclust <- function( hclust, lab=hclust$labels, lab.col=rep(1,length(hclust$labels)), hang=0.1,...){
 ## modifiction of plclust for plotting hclust objects *in colour*!
 ## Copyright Eva KF Chan 2009
 ## Arguments:
 ##    hclust:    hclust object
 ##    lab:        a character vector of labels of the leaves of the tree
 ##    lab.col:    colour for the labels; NA=default device foreground colour
 ##    hang:     as in hclust & plclust
 ## Side effect:
 ##    A display of hierarchical cluster with coloured leaf labels.
 y <- rep(hclust$height,2); x <- as.numeric(hclust$merge)
 y <- y[which(x<0)]; x <- x[which(x<0)]; x <- abs(x)
 y <- y[order(x)]; x <- x[order(x)]
 plot( hclust, labels=FALSE, hang=hang, ... )
 text( x=x, y=y[hclust$order]-(max(hclust$height)*hang),
       labels=lab[hclust$order], col=lab.col[hclust$order], 
       srt=90, adj=c(1,0.5), xpd=NA, ... )
}

dataFrame <- data.frame(x=x,y=y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
myplclust(hClustering,lab=rep(1:3,each=4),lab.col=rep(1:3,each=4))
mds(hClustering)
## Merge points together (points located farthest appart=Complete linkage)
### method= Complete
## average distance = average of x values with the average of y values for both clusters

## Visualizing the cluster (visualize the quantitative data after the cluster with heatmap)
dataFrame <- data.frame(x=x,y=y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
heatmap(dataMatrix) ## clusters the rows and the colums

# K-means clustering
set.seed(1234)
par(mar=c(0,0,0,0))
x <- rnorm(12,mean=rep(1:3,each=4),sd=0.2)
y <- rnorm(12,mean=rep(c(1,2,1),each=4),sd=0.2)
plot(x,y,col="blue",pch=19,cex=1)
text(x+0.05,y+0.05,labels=as.character(1:12))

##starting centroids
## I know there are 3 clusters
## Assign all points to closes centroid (according to the eucledian centroid dist)

## careful with iterations, centers and number of starts (it is not determinsitic)
## average centroids for diferent start
dataFrame <- data.frame(x,y)
kmeansObj <- kmeans(dataFrame,centers=3)
names(kmeansObj)
kmeansObj$cluster

## plot
plot(x,y,col=kmeansObj$cluster,pch=19,cex=2)
points(kmeansObj$centers,col=1:3,pch=3,cex=3,lwd=3)

##heatmaps
set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12),]
kmeansObj2 <- kmeans(dataMatrix,centers=3)
par(mfrow=c(1,2),mar=rep(0.2,4))
image(t(dataMatrix)[,nrow(dataMatrix):1],yaxt="n")
image(t(dataMatrix)[,order(kmeansObj$cluster)],yaxt="n")
resetPar()


# Dimension reduction
## Matrix data (random generated, observations in rows and variables in colsp)
set.seed(12345);par(mar=rep(0.2,4))
dataMatrix <- matrix(rnorm(400),nrow=40) ## see no cluster
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)

## add a pattern to the data set
set.seed(678910)
for(i in 1:40){
 #flip a coin
 coinFlip <- rbinom(1,size=1,prob=0.5)
 # if coin is heads add a common pattern to that row
 if(coinFlip){
  dataMatrix[i,] <- dataMatrix[i,]+rep(c(0,3),each=5)
 }
}

par(mar=rep(0.2,4))
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)

## what if we want to see the pattern
## get the means of the rows and the means of the colums
hh <- hclust(dist(dataMatrix)); dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered),40:1,,xlab="Row",ylab="Row Mean",pch=19)
plot(colMeans(dataMatrixOrdered),xlab="Column",ylab="Column Mean",pch=19)

## what happens if there is more than one pattern
## compression of DataMatrix
## SVD matrix decomposition

### Singular value decomposition ( u:left singular vectors and v:Right singular vectors)
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1],40:1,,xlab="Row",ylab="First left singular vector",pch=19)
## The first left singular vector= first column of the U component
plot(svd1$v[,1],xlab="Column",ylab="First rigth singular vector",pch=19)
## The first right singular vector= first column of the V component
#### sclae is different

## D component
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,2))
plot(svd1$d,xlab="column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)
## d value = decreasing set of values
## 1rst right/left singular vector 1st d value

## Relationship between principal component and SVD
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered,scale=T)
plot(pca1$rotation[,1],svd1$v[,1],pch=19,xlab="Pirncipal Component 1",
ylab="Right Singular Vector 1")
## First principal component (rotation) = first right singular vecotr 
abline(c(0,1))
resetPar()

## Variance explained
constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)

## Adding another pattern
set.seed(12345)
dataMatrix <- matrix(rnorm(400),nrow=40) ## see no cluster
set.seed(678910)
for(i in 1:40){
 # flip a coin
 coinFlip1 <- rbinom(1,size=1,prob=0.5)
 coinFlip2 <- rbinom(1,size=1,prob=0.5)
 # if coin is heads add a common pattern to that row
 if(coinFlip1){
  dataMatrix[i,] <- dataMatrix[i,]+rep(c(0,5),each=5)
 }
 if(coinFlip2){
  dataMatrix[i,] <- dataMatrix[i,]+rep(c(0,5),5)
 } 
}
resetPar()
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order,]

# me playing with clusters
plot(hh)
par(mfrow=c(1,2))
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
#

## SVD True patterns
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rep(c(0,1),each=5),pch=19,xlab="Column",ylab="Pattern 1")
plot(rep(c(0,1),5),pch=19,xlab="Column",ylab="Pattern 2")

## Singluar value decomposition
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd2$v[,1],pch=19,xlab="Column",ylab="First right singular vector")
plot(svd2$v[,2],pch=19,xlab="Column",ylab="Second right singular vector")
# The two singular vector represent a combination of the pattern that went into 

#creating the matix

## d and variance explained
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,2))
plot(svd1$d,xlab="column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)

## using fast.svd function
##  # rows much smaller that  # columns or viceversa
install.packages("corpcor")
library(corpcor)
bigMatrix <- matrix(rnorm(1e4*40),nrow=1e4)
system.time(svd(scale(bigMatrix)))
system.time(fast.svd(scale(bigMatrix),tol=0))

## does not work on datasets with missing values
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=F)] <- NA
svd1 <- svd(scale(dataMatrix2))

## if missing values we can impute (Using bioconductor)
source("http://bioconductor.org/biocLite.R")
biocLite("impute")
library(impute)

dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=F)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered)); svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2));
plot(svd1$v[,1],pch=19);
plot(svd2$v[,1],pch=19)
resetPar()

## one quick example of SVD is cool
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/face.rda",
              destfile="./data/face.rda",method="curl")
load("./data/face.rda")
image(t(faceData)[,nrow(faceData):1]) ## pixels intensity saved 
heatmap(faceData)
dim(faceData)

##SVD on faceData
svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2),pch=19,xlab="Singular vector",ylab="Variance explained")
## 1:5 variables explain a large percentage of the variance

## Create aproxximations
svd1 <- svd(scale(faceData))
# %*% is a matrix multiplication

# Here svd1$d is a constant
approx1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]

# In these examples we need to make the diagonal matrix out of d
approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[,1:5])
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[,1:10])
resetPar()
### Plot aproximations
par(mfrow=c(1,4))
image(t(faceData)[,nrow(faceData):1])
image(t(approx10)[,nrow(approx10):1])
image(t(approx5)[,nrow(approx5):1])
image(t(approx1)[,nrow(approx1):1])

### SVD example (https://class.coursera.org/dataanalysis-002/forum/thread?thread_id=571#post-3179)
rho <- 0.95
nn <- 1000
set.seed(2013)
xx <- rnorm(nn)
yy <- rho*xx+sqrt(1-rho^2)*rnorm(nn)

plot(xx,yy,pch=19,xlab="",ylab=""); abline(v=0,col="gray"); abline(h=0, col="gray")

SVD <- svd(cbind(xx,yy))
SVD$d^2/sum(SVD$d^2)

arrows(x0=0,y0=0,x1=SVD$v[1,1],y1=SVD$v[2,1],lwd=3,col="red")
arrows(x0=0,y0=0,x1=SVD$v[1,2],y1=SVD$v[2,2],lwd=3,col="red")

phi <- atan(SVD$v[1,1]/SVD$v[2,1])
tt <- seq(0,2*pi,by=.01)
eps <- .1
lines(eps*(SVD$d[1]*cos(tt)*cos(phi)-SVD$d[2]*sin(tt)*sin(phi)),
      eps*(SVD$d[1]*cos(tt)*sin(phi)+SVD$d[2]*sin(tt)*cos(phi)))

foo <- c(xx[1],yy[1])
foo
bar <- SVD$u[1,,drop=FALSE]%*%diag(SVD$d)%*%t(SVD$v)
bar

## SVD practice 2 (https://class.coursera.org/dataanalysis-002/forum/thread?thread_id=571#post-3637)
library(ggplot2)
data(diamonds)
str(diamonds)

diamonds$cut <- as.numeric(diamonds$cut)
diamonds$clarity <- as.numeric(diamonds$clarity)
diamonds$color <- as.numeric(factor(diamonds$color, levels=rev(levels(diamonds$color))))
str(diamonds)

s <- svd(scale(diamonds))
p <- prcomp(diamonds, scale.=TRUE)

str(s)
str(p)

par(mfrow=c(1,2))
plot(s$d^2 / sum(s$d^2))
plot(p$sdev^2 / sum(p$sdev^2))

# Percentage that each layer contributes to overall image
round((p$sdev^2 / sum(p$sdev^2)), 3)
# What percentage do the first 5 layers contribute?
sum((p$sdev^2 / sum(p$sdev^2))[1:5])

str(s$v)
str(p$rotation)

plot(s$v[,1], pch=19)
plot(p$rotation[,1], pch=19)
## The y-axis tells us how much that particular variable
## contributes to the resolution of this particular layer.

round(s$v[,1], 2)
round(p$rotation[,1], 2)

round(p$rotation[,1:5],2)
resetPar()
## Weekly quiz 3
### Q1
install.packages("ElemStatLearn")
library(ElemStatLearn)
data(bone)
plot(bone$age,bone$spnbmd,pch=19,col=((bone$gender=="male")+1))

### Q2
library(ElemStatLearn)
data(marketing)
plot(bone$age,bone$spnbmd,pch=19,col=((bone$gender=="male")+1))
boxplot(marketing$Income ~ marketing$Marital,col="grey",xaxt="n",ylab="Income",xlab="",
        varwidth=T)
axis(side=1,at=1:5,labels=c("Married","Living together/not married",
                            "Divorced or separated","Widowed","Nevermarried"),las=2)

### Q3
library(datasets)
data(iris)
str(iris)
irisSubset <- iris[,1:4]
str(irisSubset)
distIrisSubset <- dist(irisSubset)
hclusteringIris <- hclust(distIrisSubset)
plot(hclusteringIris)
plclust(hclusteringIris,hmin=3)

### Q4
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/quiz3question4.rda ",
              destfile="./data/quiz3question4.rda",method="curl")
a <- load("./data/quiz3question4.rda")
## name of the dataset = dataSet
dataSet
plot(dataSet$x,dataSet$y,col="blue",pch=19,cex=1)
kmeansObjQ3 <- kmeans(dataSet,centers=2)
kmeansObjQ3$cluster
plot(dataSet$x,dataSet$y,col=kmeansObjQ3$cluster,pch=19,cex=1)
points(kmeansObjQ3$centers,col=1:2,pch=3,cex=3,lwd=3)

### Q5
library(ElemStatLearn)
data(zip.train)
str(zip.train)
dim(zip.train)
?zip.train
im  <-  zip2image(zip.train,1)
dim(im)
image(im)

#### 8th row
zip.train[8,1]
im8  <-  zip2image(zip.train,8)
image(im8)
svd8 <- svd(im8)
plot(svd8$d^2/sum(svd8$d^2),pch=19,xlab="Singluar vector",ylab="Variance explained")
round(svd8$d^2/sum(svd8$d^2),5)
#### 18th row
zip.train[18,1]
im18  <-  zip2image(zip.train,18)
image(im18)
svd18 <- svd(im18)
plot(svd18$d^2/sum(svd18$d^2),pch=19,xlab="Singluar vector",ylab="Variance explained")
round(svd18$d^2/sum(svd18$d^2),5)