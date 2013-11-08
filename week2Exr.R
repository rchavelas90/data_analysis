## Load data set
install.packages("kernlab")
library(kernlab)
data(spam)
dim(spam)

## Prediction analysis: Train and test dataset
set.seed(3435)
trainIndicator=rbinom(4601,size=1,prob=0.5)
table(trainIndicator)

### 1 training set
### 0 test set
trainSpam <- spam[trainIndicator==1,]
testSpam <- spam[trainIndicator==0,]
dim(trainSpam)


## Exploratory data analysis
names(trainSpam) 
head(trainSpam) 
table(trainSpam$type) ## summary
plot(trainSpam$capitalAve~trainSpam$type)
plot(log10(trainSpam$capitalAve+1)~trainSpam$type)
plot(log10(trainSpam[,1:4]+1)) ## Difference between predictors
## exploratory
hCluster <- hclust(dist(t(trainSpam[1:57])))
plot(hCluster)

hClusterUpdated <- hclust(dist(t(log10(trainSpam[1:55]+1))))
plot(hClusterUpdated)
## Data analysis: Make plots, transformations and identify potential problems


## Prediction or modelling
trainSpam$numType  <- as.numeric(trainSpam$type)-1
costFunction <- function(x,y){sum(x!=(y>0.5))}
cvError <- rep(NA,55)
install.packages("boot")
library(boot)
for(i in 1:55){
 lmFormula <- as.formula(paste("numType~",names(trainSpam)[i],sep=""))
 glmFit <- glm(lmFormula,family="binomial",data=trainSpam)
 cvError[i] <- cv.glm(trainSpam,glmFit,costFunction,2)$delta[2]
}
which.min(cvError)
### identify name of the variable with the minimum error rate
names(trainSpam)[which.min(cvError)]

## measure of uncertainty
predictionModel <- glm(numType ~ charDollar,family="binomial",data=trainSpam)
predictionTest <- predict(predictionModel,testSpam)
predictedSpam <- rep("nonspam",dim(testSpam)[1])
predictedSpam[predictionModel$fitted>0.5] <- "spam"

table(predictedSpam,testSpam$type) ## results 22.43% error rate
(61+458)/(1346+61+458+449)

## Get/set your working directory
getwd()
setwd("/Users/RCHM/Rwork1/GitLocal/data_analysis/data")


## relative paths
### Moving down
setwd("./data")
getwd()
### Moving one up
setwd("../")
getwd()

## from internet (a bit reproducible)
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/cameras.csv",method="curl") ## curl for secure conetion
list.files("./data")
dateDownloaded <- date()

## loading saved data
getwd()
cameraData <- read.table("./data/cameras.csv",sep=",",header=T)
head(cameraData)

cameraData <- read.csv("./data/cameras.csv")
head(cameraData)

## excel files
install.packages("xlsx")
library(xlsx)
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/camera.xlsx",method="curl")
dateDownloaded <- date()
cameraData <- read.xlsx2("./data/camera.xlsx",sheetIndex=1)
head(cameraData)

### manually
cameraData <- read.csv(file.choose())

## Getting data pt 2
?connections
##read lines
con <- file("./data/cameras.csv","r")
cameraData <- read.csv(con)
close(con)
head(cameraData)

## web
con <- url("http://simplystatistics.org/","r")
simplyStats <- readLines(con)
close(con)
head(simplyStats)

##JSON files
library(RJSONIO)
fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.json?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/camera.json",method="curl")
con <- file("./data/camera.json")
jsonCamera <- fromJSON(con,depth=1)
close(con)
head(jsonCamera)[[2]][[1]]

## Write data
cameraData <- read.csv("./data/cameras.csv")
tmpData <- cameraData[,-1]
write.table(tmpData,file="./data/camerasModified.csv",sep=",")
cameraData2 <- read.csv("./data/camerasModified.csv")
head(cameraData2)

## save and save.image (use .rda)
cameraData <- read.csv("./data/cameras.csv")
tmpData <- cameraData[,-1]
save(tmpData,cameraData,file="./data/cameras.rda")

## load .rda
rm(list=ls())
ls()
load("./data/cameras.rda")
ls()

## command for common patern loading
for(i in 1:5){
 fileName <- paste0("./data",i,".csv")
 print(fileName)
}

##data of webpages
con <- url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode <- readLines(con)
close(con)
htmlCode

##data of webpages2
library(XML)
html3 <- htmlTreeParse("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en",
                       useInternalNodes=T)
html3
xpathSApply(html3,"//title",xmlValue)
xpathSApply(html3,"//td[@id='col-citedby']",xmlValue)
xpathSApply(html3,"//tr",xmlValue)