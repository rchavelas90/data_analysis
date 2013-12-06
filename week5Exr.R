## ANOVA for multiple variables
download.file(
 "https://spark-public.s3.amazonaws.com/dataanalysis/movies.txt",
 destfile="./data/movies.txt",method="curl")
movies <- read.table("./data/movies.txt",sep="\t",header=T,quote="")
head(movies)

## ANova for one factor with multiple levels
aovObject <- aov(movies$score~movies$rating)
summary(aovObject)
aovObject$coeff
unique(movies$genre)

## two variables with multple levels
aovObject2 <- aov(movies$score~movies$rating+movies$genre)
aovObject2
summary(aovObject2)
#### !!!Very careful with results of summary¡¡¡
### In this case the p value (and F) is the additional variation due to genre
### After taking into account the variation of rating (The variation already)
### explained (see next examplep)

## Unless a balanced desing... order matters
aovObject3 <- aov(movies$score~movies$genre+movies$rating)
summary(aovObject3)
### unbalanced: Levels of the genre variable arent exacltly even distributed
### between the levels of the movies rating variable

## Now quantitative (3 variables)
aovObject4 <- aov(movies$score~movies$rating+
                   movies$genre+
                   movies$box.office)
aovObject4
summary(aovObject4)
## interpretation is different , just one parameter

# Binary outcomes
a <- load("./data/ravensData.rda")
a
head(ravensData)
lmRavens <- lm(as.numeric(ravensData$ravenWinNum)~ravensData$ravenScore)
psummary(lmRavens)
### Problems with linear regression and binary outcomes

## Ploting fitted values
plot(ravensData$ravenScore,lmRavens$fitted,pch=19,col="blue",
     pylab="Prob Win",
     xlab="Raven Score")

## Log of the odds as a linear function
### GLM Command
logRegRavens <- glm(ravensData$ravenWinNum~ravensData$ravenScore,
                    family="binomial")
summary(logRegRavens)
### Intercept = log odds of result:1 when variable:0) 
### negative because is "likely" - Kinda obvious
### Estimate for de covariate: log odds ratio for 1 unit increase in covariate
### each point, the log odds ratio of wins increases .106
### Positive: as they score more points they are likely to win
### z and p same as linear

plot(ravensData$ravenScore,logRegRavens$fitted,pch=19,col="blue",
      ylab="Prob Win",
      xlab="Raven Score")

## confidence intervals for the odds ratio
exp(logRegRavens$coeff) ## get the odds ration (not the log)
## log less than 0 is more likely to loose
## once expo is less than one
exp(-1.68001)

exp(confint(logRegRavens))
## as both includes 1 (equal probability) no difference in odds of wining
## no clear of significant relationship

## Anova for logistics regresion
anova(logRegRavens,test="Chisq")


## Count outcomes (another GLM)
set.seed(3433); par(mfrow=c(1,2))
poisData1 <- rpois(100,lambda=50)
poisData2 <- rpois(100,lambda=100)
hist(poisData1,col="blue",xlim=c(0,150))
hist(poisData2,col="blue",xlim=c(0,150))

## Pparameter controls all things (mean and variance or center and spread
## var=mean  <- important assumption
c(mean(poisData1),var(poisData1))
c(mean(poisData2),var(poisData2))

a <- load("./data/gaData.rda")
gaData$julian <- julian(gaData$date) ## Number of days from 1970-1-1
tail(gaData)

resetPar()
plot(gaData$julian,gaData$visits,pch=19,col="darkgrey",xlab="Julian",ylab="Visits")
lm1 <- lm(gaData$visits~gaData$julian)
abline(lm1,col="red",lwd=3)

## Multiplicative version
glm1 <- glm(gaData$visits~gaData$julian,family="poisson")
lines(gaData$julian,glm1$fitted,col="blue",lwd=3)

## If poisson random variable, then Mean=~Variance
### AS MEAN INCREASES, VARIANCE INCREASES AS WELL!
plot(glm1$fitted,glm1$residuals,pch=19,col="grey",ylab="Residuals",xlab="Fitted")
## If assumption is correct: The more increases the fit(x), the greater the variance


## If we dont want to follow that assumption
##  model agnostic or rubust std error
install.packages("sandwich")
library(sandwich)
confint.agnostic <- function (object, parm, level = 0.95, ...) {
 cf <- coef(object); pnames <- names(cf) 
 if (missing(parm))
  parm <- pnames
 else if (is.numeric(parm))
  parm <- pnames[parm]
 a<-(1-level)/2;a<-c(a,1-a)
 pct <- stats:::format.perc(a, 3)
 fac <- qnorm(a)
 ci <- 
  array(NA, dim = c(length(parm), 2L), dimnames = list(parm,pct))
ses <- sqrt(diag(sandwich::vcovHC(object)))[parm] 
ci[] <- cf[parm] + ses %o% fac
ci
}

confint(glm1) ### carefull with interpretation
confint.agnostic(glm1) ## bit wider

## Model rates (percentage of something) (add an offset= the total)
glm2 <- glm(gaData$simplystats~gaData$julian,offset=log(visits+1),
            family="poisson",data=gaData)
## offset must be log()
plot(gaData$julian,glm2$fitted,col="blue",pch=19,xlab="Date",ylab="Fitted counts")
points(gaData$julian,glm1$fitted,col="red",pch=19)

## another way
glm2 <- glm(gaData$simplystats~gaData$julian,offset=log(visits+1),
            family="poisson",data=gaData)
plot(gaData$julian,gaData$simplystats/(gaData$visits+1),
     col="grey",
     xlab="Date",
     ylab="Fitted Rates",
     pch=19)
lines(gaData$julian,glm2$fitted/(gaData$visits+1),col="blue",lwd=3)

## Model checking and model selection
## LM  - constant variance
set.seed(3433)
par(mfrow=c(1,2))
data <- rnorm(100,mean=seq(0,3,length=100),sd=seq(0.1,3,length=100))
x <- seq(0,3,length=100)
lm1 <- lm(data ~ x)
plot(x,data,pch=19,col="grey")
abline(lm1,col="red",lwd=3)
plot(x,lm1$residuals,pch=19,col="grey")
abline(c(0,0),col="red",lwd=3)

## Using sandwich estimates for increasing variance
library(sandwich)
summary(lm1)
summary(lm1)$cov.unscaled ## Variance is smaller
vcovHC(lm1) 
## Practically with the regular LM they will underestimate the amount of
## variablity, measure assosiations less "common" 

## Linear trend?
set.seed(3433)
par(mfrow=c(1,2))
data <- rnorm(100,mean=seq(0,3,length=100)^3,sd=2)
lm1 <- lm(data~x)
plot(x,data,pch=19,col="grey")
abline(lm1,col="red",lwd=3)
plot(x,lm1$residuals,pch=19,col="grey")
abline(c(0,0),col="red",lwd=3)

## MIssing covaraite
set.seed(3433)
par(mfrow=c(1,3))
z <- rep(c(-0.5,0.5),50)
data <- rnorm(100,mean=(seq(0,3,length=100)+z),sd=seq(0.1,3,length=100))
x <- seq(0,3,length=100)
lm1 <- lm(data ~ x)
plot(x,data,pch=19,col=((z>0)+3))
abline(lm1,col="red",lwd=3)
plot(x,lm1$residuals,pch=19,col=((z>0)+3))
abline(c(0,0),col="red",lwd=3)
boxplot(lm1$residuals~z,col=((z>0)+3))

## Outliers
set.seed(343)
par(mfrow=c(1,2))
betahat <- rep(NA,100)
x <- seq(0,3,length=100)
y <- rcauchy(100)
lm1 <- lm(y~x)
plot(x,y,pch=19,col="blue")
abline(lm1,col="red",lwd=3)

for(i in 1:length(betahat)){
 betahat[i] <- lm(y[-i] ~ x[-i])$coeff[2]
}
plot(betahat - lm1$coeff[2],col="blue",pch=19)
abline(c(0,0),col="red",lwd=3)
## points near the end have a lot of influence

resetPar()
index <- which(abs(betahat-lm1$coeff[2])>0.18)
lm1 <- lm(y~x)
lm2 <- lm(y[-index]~x[-index])
plot(x,y,pch=19,col="red")
points(x[-index],y[-index],col="black",pch=19)
abline(lm1,col="red",lwd=3)
abline(lm2,col="green",lwd=3)

## Robust linear modeling
library(MASS)
set.seed(343)
x <- seq(0,3,length=100)
y <- rcapuchy(100)
lm1 <- lm(y~x)
rlm1 <- rlm(y~x)
lm1$coeff
rlm1$coeff

## Plot
par(mfrow=c(1,2))
plot(x,y,pch=19,col="grey")
lines(x,lm1$fitted,col="blue",lwd=3)
lines(x,rlm1$fitted,col="green",lwd=3)
plot(x,y,pch=19,col="grey",ylim=c(-5,5),main="Zoomed In")
lines(x,lm1$fitted,col="blue",lwd=3)
lines(x,rlm1$fitted,col="green",lwd=3)
summary(lm1)
summary(rlm1)
rlm1

## automated plots
set.seed(343)
par(mfrow=c(2,2))
x <- seq(0,3,length=100)
y <- rnorm(100)
lm1 <- lm(y~x)
plot(lm1)
## 1) differences in variation / Points outliers
## 2) QQ plot (quantile quantile) x-quantiles(norm0-1) y-Standarized residuals

## Show several plots
## Exploratory analysis (Residuals~colors)

## Model selection
head(movies)
movies <- movies[,-1]
lm1 <- lm(score~.,data=movies)
aicFormyla <- step(lm1)


## regsubsets
install.packages("leaps")
library(leaps)
regSub <- regsubsets(score~.,data=movies)
plot(regSub)
resetPar()
## Goal minimice BIC

install.packages("BMA")
library(BMA)
bicglm1 <- bic.glm(score~.,data=movies,glm.family="gaussian")
print(bicglm1)


#### Weekly Quiz 5
#Q1
data(warpbreaks)
head(warpbreaks)
summary(warpbreaks)
str(warpbreaks)
aovWarpbreaks <- aov(warpbreaks$breaks~
                      warpbreaks$wool+
                      warpbreaks$tension)
summary(aovWarpbreaks)

#Q2
p <- 0.2
log(p/(1-p))

#Q3
install.packages("glm2")
library(glm2)
data(crabs)
head(crabs)
?crabs


plot(crabs$Width,crabs$Satellites,
     pch=19,col="darkgrey",xlab="Width",ylab="Satellites")
glm1 <- glm(Satellites~Width,family="poisson",data=crabs)
points(crabs$Width,glm1$fitted,col="blue",lwd=1,pch=19)
summary(glm1)
exp(glm1$coefficients[[2]])

#Q4
index <- which(crabs$Width==22)
crabs[index,]
glm1$fitted[index][[1]]
glm1$residuals[index]
exp(-1)

#Q5
data(quine)
head(quine)
lm1 = lm(log(Days + 2.5) ~.,data=quine)
aicFormula <- step(lm1)
aicFormula

(40*.7)+50

## A bit of practice
cuse <- read.table("http://data.princeton.edu/wws509/datasets/cuse.dat",
                   header=TRUE)
head(cuse)
attach(cuse)

lrfit <- glm(cbind(using, notUsing) ~ 
               age * noMore + hiEduc , family = binomial,data=cuse)
lrfit
noMore <- wantsMore=="no"
hiEduc <- cuse$education=="high"