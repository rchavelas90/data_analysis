install.packages("scatterplot3d")
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

## p-values
plot(galton$parent,galton$child,pch=19,col="blue")
lm1 <- lm(galton$child~galton$parent) ## Linear model minimizes the distance from the mean
abline(lm1,col="red",lwd=3)


## Null distribution
x <- seq(-20,20,length=100)
plot(x,dt(x,df=(928-2)),col="blue",lwd=3,type="l")
arrows(summary(lm1)$coeff[2,3],0.25,summary(lm1)$coeff[2,3],0,col="red",lwd=4)

## 
summary(lm1)

## Simulated example
set.seed(9898324)
yValues <- rnorm(10)
xValues <- rnorm(10)
plot(xValues,yValues)
lm2 <- lm(yValues~xValues)
summary(lm2)

# Null hypotesis distributions
x <- seq(-5,5,length=100)
plot(x,dt(x,df=(10-2)),col="blue",lwd=3,type="l")
arrows(summary(lm2)$coeff[2,3],0.25,
       summary(lm2)$coeff[2,3],0,col="red",lwd=4)

## Probabilities (look at the area under the curve)
xCoords <- seq(-5,5,length=100)
plot(xCoords,dt(xCoords,df=(10-2)),col="blue",lwd=3,type="l")
xSequence <- c(seq(summary(lm2)$coeff[2,3],5,length=10),
               summary(lm2)$coeff[2,3])
ySequence <- c(dt(seq(summary(lm2)$coeff[2,3],5,length=10),df=8),0)
polygon(xSequence,ySequence,col="red")
polygon(-xSequence,ySequence,col="red")

## properties of pvalues
set.seed(8323)
ppValues <- rep(NA,1000)
for(i in 1:1000){
 xValues <- rnorm(20);yValues <- rnorm(20)
 pValues[i] <- summary(lm(yValues~xValues))$coeff[2,4]
}
hist(pValues,col="blue",main="",freq=F)
abline(h=1,col="red",lwd=3)

## with signal
set.seed(8323)
pValues <- rep(NA,1000)
for(i in 1:1000){
 xValues <- rnorm(100);yValues <- 0.2*xValues+rnorm(100) 
##### depends on the sample size and the noise
 pValues[i] <- summary(lm(yValues~xValues))$coeff[2,4]
}
hist(pValues,col="blue",main="",freq=F)
abline(h=1,col="red",lwd=3)

### if we have two data sets with very different size, then the p-values wont
## be neccesarly comparable

summary(lm(galton$child~galton$parent))$coeff

## Regression with factor variables
## factor covaariate but still numeric output (Cuantitative)
download.file(
 "https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt",
 destfile="./data/movies.txt",method="curl")

movies <- read.table("./data/movies.txt",sep="\t",header=T,quote="")
head(movies)

## Score vs rating
plot(movies$score~jitter(as.numeric(movies$rating)),col="blue",xaxt="n",pch=19)
axis(side=1,at=unique(as.numeric(movies$rating)),labels=unique(movies$rating))

## calculate the mean ratings
meanRatings <- tapply(movies$score,movies$rating,mean)
points(1:4,meanRatings,col="red",pch="-",cex=5)

lm1 <- lm(movies$score~as.factor(movies$rating))
summary(lm1)

## plot fitted values 
plot(movies$score~jitter(as.numeric(movies$rating)),col="blue",xaxt="n",pch=19)
axis(side=1,at=unique(as.numeric(movies$rating)),labels=unique(movies$rating))
points(1:4,lm1$coeff[1]+c(0,lm1$coeff[2:4]),col="red",pch="-",cex=5)
## it plots the means within gropus 

summary(lm1)
confint(lm1) ## Gives the confidence interval of the diff between the levels

## reorder the model
## bo for R movies
## so the "intercept" term must be the one associated with R movies
lm2 <- lm(movies$score~relevel(movies$rating,ref="R")) ## the reference category
summary(lm2)
confint(lm2)
## See the confint includes 0! problems there

## any difference betw all (FULL MODEL)
## not just one variable, or individual coefficients
## use ANOVA
lm1 <- lm(movies$score~as.factor(movies$rating))
anova(lm1)
 ### DF hom many parameters had to be estimated
### residuals = variables - observations

## SUm of squares
gMovies <- movies[movies$rating=="G",]
xVals <- seq(0.2,0.8,length=4)
plot(xVals,gMovies$score,ylab="Score",xaxt="n",xlim=c(0,1),pch=19)
abline(h=mean(gMovies$score),col="blue",lwd=3)
abline(h=mean(movies$score),col="red",lwd=3)
segments(xVals+0.01,rep(mean(gMovies$score),length(xVals)),xVals+0.01,
         rep(mean(movies$score),length(xVals)),col="red",lwd=2)
segments(xVals-0.01,gMovies$score,xVals-0.01,rep(mean(gMovies$score),length(xVals)),
         col="blue",lwd=2)

## Tukeys honestly significat difference test
#### Less frequently than ANOVA
#### use AOV and TukeyHSD
lm1 <- aov(movies$score~as.factor(movies$rating))
TukeyHSD(lm1)
#### Calculate the diff between all posible par of levels (lwr and upr at 95%)


## Multiple variable regression
# Multiple covariate
# carefull with interaction terms

download.file("https://spark-public.s3.amazonaws.com/dataanalysis/hunger.csv",
              destfile="./data/hunger.csv",method="curl")

hunger <- read.csv("./data/hunger.csv")
head(hunger,10)
table(hunger$Sex)
hunger <- hunger[hunger$Sex!="Both sexes",]
head(hunger)
## Numeric % of children that are underweight

lm1 <- lm(hunger$Numeric~hunger$Year)
plot(hunger$Year,hunger$Numeric,pch=19,col="blue")
lines(hunger$Year,lm1$fitted,lwd=3,col="darkgrey")
abline(lm1)

plot(hunger$Year,hunger$Numeric,pch=19,col="blue")
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+1))

## two lines
### subset by factor vaiable in both variables
lmM <- lm(hunger$Numeric[hunger$Sex=="Male"]~hunger$Year[hunger$Sex=="Male"])
lmF <- lm(hunger$Numeric[hunger$Sex=="Female"]~hunger$Year[hunger$Sex=="Female"])
plot(hunger$Year,hunger$Numeric,pch=19,col="blue")
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+1))
lines(hunger$Year[hunger$Sex=="Male"],lmM$fitted,lwd=3,col="black")
lines(hunger$Year[hunger$Sex=="Female"],lmF$fitted,lwd=3,col="red")

## two lines one slope (one slope for both line) with dummy variables
## B2= Decrease in % hunger by year
lmBoth <- lm(hunger$Numeric~hunger$Year + hunger$Sex)
summary(lmBoth)
plot(hunger$Year,hunger$Numeric,pch=19)
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+1))
abline(c(lmBoth$coeff[1],lmBoth$coeff[2]),col="red",lwd=3)
abline(c(lmBoth$coeff[1]+lmBoth$coeff[3],lmBoth$coeff[2]),col="black",lwd=3)
## slopes the same

## Two lines With different slopes
lmBoth <- lm(hunger$Numeric~hunger$Year + hunger$Sex + hunger$Sex*hunger$Year)
plot(hunger$Year,hunger$Numeric,pch=19)
points(hunger$Year,hunger$Numeric,pch=19,col=((hunger$Sex=="Male")*1+1))
abline(c(lmBoth$coeff[1],lmBoth$coeff[2]),col="red",lwd=3)
abline(c(lmBoth$coeff[1]+lmBoth$coeff[3],
         lmBoth$coeff[2]+lmBoth$coeff[4]),col="black",lwd=3)

summary(lmBoth)
## Interaction with colon
## Interactio term tooo low (smaller) b3 difference in slopes

## Cuantitative terms wit interactions
### interaction btw income and year

# Regression in the real world
## Vibariate normal with normal shape
hunger <- read.csv("./data/hunger.csv")
hunger <- hunger[hunger$Sex!="Both sexes",]
head(hunger)
resetPar()

## Confounders
par(mfrow=c(1,2))
plot(hunger$Year,hunger$Numeric,col=as.numeric(hunger$WHO.region),pch=19)
plot(1:10,type="n",xaxt="n",yaxt="n",xlab="",ylab="")
legend(1,10,col=unique(as.numeric(hunger$WHO.region)),
       legend=unique(hunger$WHO.region),pch=19)p
## several variation by region (Not uniform if I performed a regression by
## region )
## Conclusion: Region is correlated with year

## More observations for certain regions in certain years
anova(lm(hunger$Year~hunger$WHO.region))
pplot(jitter(hunger$Year),jitter(as.numeric(hunger$WHO.region)))

## Region is also correlated with hunger
### Much more than for year
anova(lm(hunger$Numeric~hunger$WHO.region))

## so! including a region is quite a complicated interaction
plot(hunger$Year,hunger$Numeric,col=as.numeric(hunger$WHO.region),pch=19)
lmRegion <- lm(hunger$Numeric~hunger$Year+hunger$WHO.region+
                hunger$Year*hunger$WHO.region)
summary(lmRegion)
abline(c(lmRegion$coeff[1]+lmRegion$coeff[6],
         lmRegion$coeff[2]+lmRegion$coeff[12]),
       col=5,lwd=3)

## difficult to decide which variables to include in the model

download.file("https://spark-public.s3.amazonaws.com/dataanalysis/income.csv",
              destfile="./data/income.csv",method="curl")
incomeData <- read.csv("./data/income.csv",header=F)
income <- incomeData[,3]
age <- incomeData[,1]
head(incomeData)

## Log to adress right skew
par(mfrow=c(1,4))
smoothScatter(age,income)
hist(income,col="blue",breaks=100)
hist(log(income+1),col="blue",breaks=100) ## always add 1
smoothScatter(age,log(income+1))
### Rigth skew data (long tail to the right)
## Violates one of the rules of a regression model and can lead to difficulties

## Outliers: o not follow the pattern of other data points
## Consider: Magnitude, orientation and location
resetPar()
set.seed(1235)
xVals <- rcauchy(50)
hist(xVals,col="blue")

## Outliers might be real
age <- c(age,52)
income <- c(income,378e6)
smoothScatter(age,income)

## Exam 
### Q1
download.file(
 "https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt",
 destfile="./data/movies.txt",method="curl")
movies <- read.table("./data/movies.txt",sep="\t",header=T,quote="")
head(movies)
lmMov <- lm(movies$score~movies$box.office)
summary(lmMov)
plot(movies$box.office,movies$score)
abline(lmMov)

## Q2
lmMov <- lm(movies$score~movies$box.office)
summary(lmMov)
plot(movies$box.office,movies$score)
abline(lmMov)
confint(lmMov,level=0.90)

## Q3
head(movies)
lmMov2 <- lm(movies$score~movies$box.office+movies$running.time)
summary(lmMov2)
library(scatterplot3d)
par(mfrow=c(1,3))
s3d <- scatterplot3d(z=movies$score,x=movies$box.office,y=movies$running.time, 
              main="3D Scatterplot",pch=16, highlight.3d=TRUE,type="h",box=F)
s3d$plane3d(lmMov2)
plot(movies$box.office,movies$score)
plot(movies$running.time,movies$score)
resetPar()

# Q4
lmMov2 <- lm(movies$score~movies$box.office+movies$running.time)
summary(lmMov2)
anova(lm(movies$box.office~movies$running.time))
anova(lm(movies$score~movies$running.time))

# Q5
lmMov2 <- lm(movies$score~movies$box.office+movies$running.time)
summary(lmMov2)
plot(movies$running.time,movies$score)
outli <- which(movies$running.time>175)
moviesWO <- movies[-outli,]

lmMov3 <- lm(moviesWO$score~moviesWO$box.office+moviesWO$running.time)
summary(lmMov3)
plot(moviesWO$running.time,moviesWO$score)

# Q6
lmMov2 <- lm(movies$score~movies$box.office+movies$running.time)
summary(lmMov2)

# Q7
head(movies)
lmMov3 <- lm(movies$score~movies$rating+movies$running.time+
              movies$running.time*movies$rating)
lmMov3 <- lm(movies$score~movies$rating+movies$running.time+
              movies$rating*movies$running.time)
summary(lmMov3)

# Q8
head(movies)
lmMov3 <- lm(movies$score~movies$rating+movies$running.time+
              movies$running.time*movies$rating)
lmMov3 <- lm(movies$score~movies$rating+movies$running.time+
              movies$rating*movies$running.time)
summary(lmMov3)
lmMov3$coeff[5]+lmMov3$coeff[6]

## Q9 
data(warpbreaks)
head(warpbreaks)
?warpbreaks
lmWar <- lm(warpbreaks$breaks~relevel(warpbreaks$tension,ref="H"))
plot(warpbreaks$tension,warpbreaks$breaks)
summary(lmWar)
confint(lmWar,level=0.95)

y1 <- function(x){
 x+2
}
y2 <- function(x){
 x*x
}

x <- seq(-1,2,by=0.001)
values <- data.frame(x=x,y=NA)
head(values)
length(values$x)
for(i in 1:length(values$x)){
 x <- values$x[i]
 diff <- y1(x)-y2(x)
 values$y[i] <- diff
}

index <- which(values$y==max(values$y))
values[index,]
1/2+2-1/4