## Clustering Samsung galaxy s3

download.file("https://spark-public.s3.amazonaws.com/dataanalysis/samsungData.rda",
              destfile="./data/samsungData.rda",method="c")
a <- load("./data/samsungData.rda")
a
names(samsungData)[1:12]
table(samsungDatap$activity)

##Goal Use accelerometer meassurements to identify / predict which activity the person was doing

### 1* see the variables themselves
par(mfrow=c(1,2))
table(samsungData$subject)
numericActivity <- as.numeric(as.factor(samsungData$activity))[samsungData$subject==1]
#### numeric vector for activities p
plot(samsungData[samsungData$subject==1,1],pch=19,col=numericActivity,ylab=names(samsungData)[1])
plot(samsungData[samsungData$subject==1,2],pch=19,col=numericActivity,ylab=names(samsungData)[2])
legend(150,-0.1,legend=unique(samsungData$activity),col=unique(numericActivity),pch=19)

### Clustering based on average accelaration (X,Y,Z)
resetPar()
distanceMatrix <- dist(samsungData[samsungData$subject==1,1:3])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=numericActivity)
#### Look that it doesnt cluster by activities very nicely
#### The scatterplot shows why, the pattern variation between one another is very similar 
## didnt seem to be so good

### try to see if other variable (like the max acceleration on each axis)
par(mfrow=c(1,2))
plot(samsungData[samsungData$subject==1,10],pch=19,col=numericActivity,ylab=names(samsungData)[10])
plot(samsungData[samsungData$subject==1,11],pch=19,col=numericActivity,ylab=names(samsungData)[11])
legend(150,0.9,legend=unique(samsungData$activity),col=unique(numericActivity),pch=19)

resetPar()
distanceMatrix <- dist(samsungData[samsungData$subject==1,10:12])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=numericActivity)
## imporves the clustering (try to look at the SVD  )

## identify the dimensions 
dim(samsungData[samsungData$subject==1,])
dim(samsungData)

## SVD
svd1 <- svd(scale(samsungData[samsungData$subject==1,-c(562,563)])) ## all variables w/o 2 last 
### (subjet and variable) to see if the pattern on those data
### Remember U=left singlar vector 
par(mfrow=c(1,2))
plot(svd1$u[,1],col=numericActivity,pch=19) ## first singular vector
## This vectors show across all different activities, what are the most common patterns
## This distinguish doing something vs dont (maybe the same as the max acceleration - maybe no useful )
plot(svd1$u[,2],col=numericActivity,pch=19) ## across observations
## second singular vector (orothonal - uncorrelated) wakking up and down are well distinguish
## a pattern in the data that explains a lot of variation (separates categories that werent before)

## Discover what variables contribute to the pattern (observed in U)
resetPar()
## rignth singular vector that corresponds to the pattern (second one)
plot(svd1$v[,2],pch=19) ### across variables
## weight of variables that contribute to that specific pattern
## pick the one maximum and inculde it in the clustering


## New clustering with maximum contributer
maxContrib <- which.max(svd1$v[,2])
resetPar()
distanceMatrix <- dist(samsungData[samsungData$subject==1,c(10:12,maxContrib)])
hclustering <- hclust(distanceMatrix)
myplclust(hclustering,lab.col=numericActivity)
legend(150,0.9,legend=unique(samsungData$activity),col=unique(numericActivity),pch=19)
names(samsungData)[maxContrib]


############ Using K-means
kClust <- kmeans(samsungData[samsungData$subject==1,-c(562,563)],centers=6,nstart=2)
table(kClust$cluster,samsungData$activity[samsungData$subject==1])
## cluster assigned vs activity performed
## there are 6 clusters, but it doesnt identify the clusters that we think in advice

kClust <- kmeans(samsungData[samsungData$subject==1,-c(562,563)],centers=6,nstart=100)
table(kClust$cluster,samsungData$activity[samsungData$subject==1])
## cluster assigned vs activity performed
## there are 6 clusters, but it doesnt identify the clusters that we think in advice
## generate random 100 initialization

## What are the variables that are contributing to the clusters
## Look at the cluster variable centers (laying)
## Cluster 1 variable centers (varies with replication, but look at which cluster and the variable)
plot(kClust$center[1,1:10],pch=19,ylab="Cluster Center",xlab="")
p## The first 10 variables
plot(kClust$center[4,1:10],pch=19,ylab="Cluster Center",xlab="")



#### Session 2: Basic least sqares
library(UsingR);data(galton)
par(mfrow=c(1,2))
hist(galton$child,col="blue",breaks=100)
hist(galton$parent,col="blue",breaks=100)

resetPar()
hist(galton$child,col="blue",breaks=100) ## how to summarize this distribution
meanChild <- mean(galton$child) ## if distribution = simetric
lines(rep(meanChild,100),seq(1,120.1636,length=100),col="red",lwd=5)
dim(galton)

plot(galton$parent,galton$child,pch=19,col="blue")

set.seed(1234)
plot(jitter(galton$parent,factor=2),jitter(galton$child,factor=2),pch=19,col="Blue")
## ad random noise

## predicting child height with parent height 
plot(galton$parent,galton$child,pch=19,col="blue")
near65 <- galton[abs(galton$parent-65)<1,]
points(near65$parent,near65$child,pch=19,col="red")
lines(seq(64,66,length=100),rep(mean(near65$child),100),col="red",lwd=4)

## different part of the distribution
plot(galton$parent,galton$child,pch=19,col="blue")
near71 <- galton[abs(galton$parent-71)<1,]
points(near71$parent,near71$child,pch=19,col="red")
lines(seq(70,72,length=100),rep(mean(near71$child),100),col="red",lwd=4)

## anther way fitting a line
plot(galton$parent,galton$child,pch=19,col="blue")
lm1 <- lm(galton$child~galton$parent) ## Linear model minimizes the distance from the mean
## or the sums of squares problem
lines(galton$parent,lm1$fitted,col="red",lwd=3)

lines(galton$parent,26+0.646*galton$parent)

par(mfrow=c(1,2))
plot(galton$parent,galton$child,pch=19,col="blue")
lines(galton$parent,lm1$fitted,col="red",lwd=3)
plot(galton$parent,lm1$residuals,col="blue",pch=19)
abline(c(0,0),col="red",lwd=3)


## Inference basics
resetPar()
plot(galton$parent,galton$child,pch=19,col="blue")
lm1 <- lm(galton$child~galton$parent)
lines(galton$parent,lm1$fitted,col="red",lwd=3)
## just one line for a subset of 900 families

## million families 
newGalton <- data.frame(parent=rep(NA,1e6),child=rep(NA,1e6))
newGalton$parent <- rnorm(1e6,mean=mean(galton$parent),sd=sd(galton$parent))
newGalton$child <- lm1$coeff[1]+lm1$coeff[2]*newGalton$parent+
 rnorm(1e6,sd=sd(lm1$residuals)) ### SOME NOISE 
## from the model fitted with the galton data
smoothScatter(newGalton$parent,newGalton$child)
abline(lm1,col="red",lwd=3)

## imageine hipoteticaly taking a sample
set.seed(134325)
sampleGalton1 <- newGalton[sample(1:1e6,size=50,replace=F),]
sampleLm1 <- lm(sampleGalton1$child~sampleGalton1$parent)
plot(sampleGalton1$parent,sampleGalton1$child,pch=19,col="blue")
lines(sampleGalton1$parent,sampleLm1$fitted,lwd=3,lty=2)
abline(lm1,col="red",lwd=3)

## SIMULATE
sampleLm <- vector(100,mode="list")
 for(i in 1:100){
 sampleGalton <- newGalton[sample(1:1e6,size=50,replace=F),]
 sampleLm[[i]] <- lm(sampleGalton$child~sampleGalton$parent) ## save it to one element of the list
}

## loop through the list
smoothScatter(newGalton$parent,newGalton$child)
for(i in 1:100){abline(sampleLm[[i]],lwd=3,lty=2)}
abline(lm1,col="red",lwd=3)

## HIstogram of estimates
par(mfrow=c(1,2))
hist(sapply(sampleLm,function(x){coef(x)[1]}),col="blue",xlab="Intercept",main="")
hist(sapply(sampleLm,function(x){coef(x)[2]}),col="blue",xlab="Slope",main="")

## estimate the values of the coefficients
sampleGalton4 <- newGalton[sample(1:1e6,size=50,replace=F),]
sampleLm4 <- lm(sampleGalton4$child~sampleGalton4$parent)
summary(sampleLm4p)

resetPar()
hist(sapply(sampleLm,function(x){coef(x)[2]}),col="blue",xlab="Slope",main="",
     freq=F)
## distribution for one subsample DS
lines(seq(0,5,length=100),dnorm(seq(0,5,length=100),
                                mean=coef(sampleLm4)[2],
                                sd=summary(sampleLm4)$coeff[2,2]),
      lwd=3,col="red")

unclass(summary(sampleLm4))

## IF we want to standarize
## DF how much variation has been leftover after estimating the parameters
resetPar()
## t distribution
x <- seq(-5,5,length=100)
plot(x,dnorm(x),type="l",lwd=3)
lines(x,dt(x,df=3),lwd=3,col="red") ## five data points and two parameters
## more spread out
lines(x,dt(x,df=10),lwd=3,col="blue") ##

## Posible values (real Btsand its possible values)
## level alpha confidence
summary(sampleLm4)$coeff
confint(sampleLm4,level=0.95)
## note the frequency in which a confidence interval convers the actual value

par(mar=c(4,4,0,2))
plot(1:10,type="n",xlim=c(0,1.5),ylim=c(0,100),
     xlab="Coefficient Values",ylab="Replication")

for(i in 1:100){
 ci <- confint(sampleLm[[i]]);
 color <- "red"
 if((ci[2,1]<lm1$coeff[2])&(lm1$coeff[2]<ci[2,2])){
  color="grey"
 }
 segments(ci[2,1],i,ci[2,2],i,col=color,lwd=3)
}
lines(rep(lm1$coeff[2],100),seq(0,100,length=100),lwd=3)

 #Reporting
sampleLm4$coeff
confint(sampleLm4,level=0.95)
