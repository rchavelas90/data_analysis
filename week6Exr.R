# Prediction models
## Cross validation

### Overfitting as a problem
set.seed(12345)
x <- rnorm(10)
y <- rnorm(10)
z <- rbinom(10,size=1,prob=0.5)
plot(x,y,pch=19,col=(z+3))
### Using z as the predicted variable and x,y as the predictors, look that it kind
### of clusters

## clasiffier
# if(-0.2,0.6) is z=blue
par(mfrow=c(1,2))
zhat <- (-0.2<y)&(y<0.6)
plot(x,y,pch=19,col=(z+3))
plot(x,y,pch=19,col=(zhat+3))

## new data, same clasiffier
set.seed(1233)
xnew <- rnorm(10)
ynew <- rnorm(10)
znew <- rbinom(10,size=1,prob=0.5)
zhatnew <- (-0.2<ynew)&(ynew<0.6)
plot(xnew,ynew,pch=19,col=(z+3))
plot(xnew,ynew,pch=19,col=(zhatnew+3))

## Example leave half out
y1 <- y[1:5]
x1 <- x[1:5]
z1 <- z[1:5]
y2 <- y[6:10]
x2 <- x[6:10]
z2 <- z[6:10]
zhat <- (y2<1) & (y2>-0.5)
par(mfrow=c(1,3))
plot(x1,y1,col=(z1+3),pch=19)
## not test on the 2nd data set
plot(x2,y2,col=(z2+3),pch=19)
plot(x2,y2,col=(zhat+3),pch=19)

## Prediction with regression
data(faithful)
dim(faithful)
set.seed(333)
trainSamples <- sample(1:272,size=(272/2),replace=F)
trainFaith <- faithful[trainSamples,]
testFaith <- faithful[-trainSamples,]
head(trainFaith)

resetPar()
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",
     xlab="Waiting",
     ylab="Duration")

## Fit a linear model
lm1 <- lm(eruptions~waiting,data=trainFaith)
summary(lm1)

## plot the model fit
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",
     xlab="Waiting",
     ylab="Duration")
lines(trainFaith$waiting,lm1$fitted,lwd=3)

##  getting the coefficients
coef(lm1)[1]+coef(lm1)[2]*80
newdata <- data.frame(waiting=80) ## dataframe for the values
## of the variables we want to predict
## names must match
predict(lm1,newdata)
points(80,predict(lm1,newdata),col="red",pch=19)

## plot on training and test
par(mfrow=c(1,2))
plot(trainFaith$waiting,trainFaith$eruptions,pch=19,col="blue",
     xlab="Waiting",
     ylab="Duration")
lines(trainFaith$waiting,predict(lm1),lwd=3)

plot(testFaith$wariting,testFaith$eruptions,pch=19,col="blue",
     xlab="Waiting",
     ylab="Duration")
lines(testFaith$waiting,predict(lm1,newdata=testFaith),lwd=3)
lm1 <- lm(eruptions~waiting,data=testFaith)


resetPar()
# getting errors
## calculate RMSE on training
summary(lm1)
sqrt(sum((lm1$fitted-trainFaith$eruptions)^2))
## calculate RMSE on test
sqrt(sum((predict(lm1,newdata=testFaith)-testFaith$eruptions)^2))

## prediction intervals
pred1 <- predict(lm1,newdata=testFaith,interval="prediction")
ord <- order(testFaith$waiting)
plot(testFaith$waiting,testFaith$eruptions,pch=19,col="blue",
     xlab="Waiting",
     ylab="Duration")
matlines(testFaith$waiting[ord],pred1[ord,],type="l",,
         col=c(1,2,2),
         lty=c(1,1,1),
         lwd=3)

## Binary data
a <- load("./data/ravensData.rda")
a
head(ravensData)

## ravenWin as quantitative
## better choise is a logistics model
glm1 <- glm(ravenWinNum~ravensData$ravenScore,family="binomial",
            data=ravensData)
## model the logit (probability of wins)
coef(glm1)
summary(glm1)
par(mfrow=c(1,2))
boxplot(predict(glm1)~ravensData$ravenWinNum,col="blue")
## remember this is on the linear scale

## see them of probabilities themseflt, (the predicted probability)
boxplot(predict(glm1,type="response")~ravensData$ravenWinNum,
        col="blue")

## the output of this regression is the probability of the ravens wins
## goal predict of win or not (binary) need to convert the probabilities to wins (1 or 0)
## choose a cutoff (if the probability is high enougth then say a win)
## could use 0.5 but is not always goods
## cutoff
xx <- seq(0,1,length=10) ## calculate a series of cutoffs
err <- rep(NA,10)
for(i in 1:length(xx)){ ## loop over all posible values of the cutof
 err[i] <- sum((predict(glm1,type="response")>xx[i])!=ravensData$ravenWinNum)
## calculate the prediction (probability of win) and look if it is bigger than the cuttof
## calculate the number of times that that is not equal to the value of the raven wins
## so this is the number of errors (the sum of the total number of errors for each value of
## the cuttof 
}
plot(xx,err,pch=19,xlab="Cutoff",ylab="Error")
## cutof is 0.7 about with less errors
## in practice use cross validation

## cross validation to compare models
library(boot)
cost <- function(win,pred=0) mean(abs(win-pred)>0.5)
## error measure (if the prediction is grater of .5)
glm1 <- glm(ravenWinNum~ravenScore,family="binomial",data=ravensData) ## logistics regression
glm2 <- glm(ravenWinNum~ravenScore,family="gaussian",data=ravensData) ## linear model traditional
cv1 <- cv.glm(ravensData,glm1,cost,K=3)
cv2 <- cv.glm(ravensData,glm2,cost,K=3)
cv1$delta ## cost for each different model
cv2$delta

glm1 <- glm(ravenWinNum~ravenScore,family="binomial",data=ravensData) ## logistics regression
glm2 <- glm(ravenWinNum~ravenScore,family="gaussian",data=ravensData) ## linear model traditional
cv1 <- cv.glm(ravensData,glm1,K=5)
cv2 <- cv.glm(ravensData,glm2,K=5)
cv1$delta ## cost for each different model
cv2$delta

## prediction with trees
## capture non-linearity
## use lots of variables
## split variables one at a time and assign the prediction most likely prediction at the end nodes
data(iris)
names(iris)
table(iris$Species)
## goal classify the species by its variables
plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
legend(1,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)

## try to predict according to those variables
install.packages("tree")
library(tree)
tree1 <- tree(Species~Sepal.Width+Petal.Width,data=iris)
summary(tree1)
## missclassification error = how oftten were they missclassified (very low)
plot(tree1)
text(tree1)

##CART model
## 2 continuos predictors in the model (how they look in 2d)
plot(iris$Petal.Width,iris$Sepal.Width,pch=19,col=as.numeric(iris$Species))
partition.tree(tree1,label="Species",add=T)
legend(1.75,4.5,legend=unique(iris$Species),col=unique(as.numeric(iris$Species)),pch=19)


## predicting new variables
set.seed(32313)
newdata <- data.frame(Petal.Width=runif(20,0,2.5),Sepal.Width=runif(20,2,4.5))
pred1 <- predict(tree1,newdata)
pred1
## reports the probability of being on one of the particular species

## calculate a prediction value for each point
pred1 <- predict(tree1,newdata,type="class")
## returns the max probability and assigns it to the classification
plot(newdata$Petal.Width,newdata$Sepal.Width,col=as.numeric(pred1),pch=19)
partition.tree(tree1,label="Species",add=T)

## pruning trees example: Cars
data(Cars93,package="MASS")
head(Cars93)
## the trees must be cutted off
## predict the drive train (4w rw fr2 based on some other variables)
resetPar()
treeCars <- tree(DriveTrain ~ MPG.city + MPG.highway + AirBags +
 EngineSize + Width + Length + Weight + Price + Cylinders +
 Horsepower + Wheelbase,data=Cars93)
plot(treeCars) 
text(treeCars)

## very dense! 
par(mfrow=c(1,2))
plot(cv.tree(treeCars,FUN=prune.tree,method="misclass"))
plot(cv.tree(treeCars))
## cross validation = new data sets

##pruning the tree
pruneTree <- prune.tree(treeCars,best=4)
plot(pruneTree)
text(pruneTree)
## best tree with just 4 terminal trees (has best error rate)
resetPar()
## Resubsituton accuracy of the trees
table(Cars93$DriveTrain,predict(pruneTree,type="class"))
## works less better than the other but the CValidation shows it would work better
table(Cars93$DriveTrain,predict(treeCars,type="class"))

download.file(url="http://www.cbcb.umd.edu/~hcorrada/PracticalML/src/trees.R",
              destfile="trees.R",method="curl")
