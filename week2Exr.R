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

## Summarizing data 
fileUrl <- "http://earthquake.usgs.gov/earthquakes/catalogs/eqs7day-M1.txt" 
download.file(fileUrl,destfile="./data/earthquakeData.csv",method="curl") 
dateDownloaded <- date()
dateDownloaded
eData <- read.csv("./data/earthquakeData.csv")
head(eData)
eData <- eData[-1,]
str(eData)
eData <- droplevels(eData)

head(eData)
dim(eData)
names(eData)
nrow(eData)

quantile(eData$Lat) ## See maximunm and minimum
summary(eData) 
class(eData)
sapply(peData[1,], class) ## just the first row, small trick
str(eData)

## Cualitative variables 
unique(eData$Src)
length(unique(eData$Src))
table(eData$Src)

## Table flexibility
#### Relationshipo of variables
table(eData$Src,eData$Version)

### build comparison: ANY and ALL looking and missing data o look for special characteristics
eData$Lat[1:10]
eData$Lat[1:10]>40 # Logical comparison
any(eData$Lat[1:10]>40)
any(eData$Lon < -100)


## Subsetting
eData[eData$Lat>0 & eData$Lon< -100 ,c("Lat","Lon")]
eData[eData$Lat>0 | eData$Lon< -100 ,c("Lat","Lon")] ## or operator


reviews <- read.csv("./data/reviews.csv")
solutions <- read.csv("./data/solutions.csv") 
head(reviews,2)
head(solutions,2)

## other method 1
fileUrl1 <- "https://dl.dropbox.com/u/7710864/data/reviews-apr29.csv"
download.file(fileUrl1,destfile="./data/reviews2.csv",method="curl",extra=c("-L")) 
reviews2 <- read.csv("./data/reviews2.csv")
head(reviews2,2)
## other method 2
fileUrl1 <- "https://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
download.file(fileUrl1,destfile="./data/reviews3.csv",method="curl") 
reviews3 <- read.csv("./data/reviews3.csv")
head(reviews3,5)

## Missing values
head(reviews,10)
is.na(reviews$time_left[1:10])
sum(is.na(reviews$time_left))
table(is.na(reviews$time_left))

## issue
table(c(0,1,2,3,NA,3,3,2,2,3)) ## does not shows NA
table(c(0,1,2,3,NA,3,3,2,2,3),useNA="ifany") ## shows NA

## sumarize by rows and columns
colSums(reviews)
colMeans(reviews,na.rm=T)
rowMeans(reviews,na.rm=T)

## Fixing character vectors
cameraData <- read.csv("./data/cameras.csv")
names(cameraData)
names(cameraData) <- tolower(names(cameraData))

##Splitting character vectos
names(cameraData)
splitNames <- strsplit(names(cameraData),"\\.")
splitNames[[5]]
splitNames[[6]][1]
firstElement <- function(x){x[[1]]}
sapply(splitNames,firstElement)

##
reviews <- read.csv("./data/reviews.csv")
head(reviews,2)
solutions <- read.csv("./data/solutions.csv") 
head(solutions,2)

names(reviews)
sub("_",".",names(reviews))## subsitute a pattern/character with another (only the first instance)

testName <- "this_is_a_test"
testName
sub("_","",testName)
gsub("_","",testName)

## turn variables into ranges
reviews$time_left[1:10]
timeRanges <- cut(reviews$time_left,seq(0,3600,by=600),dig.lab=4)
timeRanges[1:10]
class(timeRanges)
table(timeRanges,useNA="ifany")

## Cut 2 by quantiles
library(Hmisc)
timeRanges <- cut2(reviews$time_left,g=6)
table(timeRanges,useNA="ifany")

##Adding any extra variable
timeRanges <- cut2(reviews$time_left,g=6)
reviews$timeRanges <- timeRanges
head(reviews,2)

## merge by common variable names
names(reviews)
names(solutions)
mergedData <- merge(reviews,solutions,all=T)
head(mergedData)

mergedData2 <- merge(reviews,solutions,by.x="solution_id",by.y="id",all=T)
head(mergedData2[,1:6],3)
head(mergedData2,3)
reviews[1,1:6]
head(reviews)

##sort values
mergedData2$reviewer_id[1:10]
sort(mergedData2$reviewer_id)[1:10]

## sort data frames by a particular variable
mergedData2$reviewer_id[1:10]
order(mergedData2$reviewer_id)[1:10]
mergedData2$reviewer_id[order(mergedData2$reviewer_id)]

## Reordering a DataFrame
head(mergedData2[,1:6],3)
sortedData <- mergedData2[order(mergedData2$reviewer_id),]
head(sortedData[,1:6],3)

##by multiple variables
head(mergedData2[,1:6],3)
sortedData <- mergedData2[order(mergedData2$reviewer_id,mergedData2$id),]
head(sortedData[,1:6],3)

## change the format of a DataSet
misShaped <- as.data.frame(matrix(c(NA,5,1,4,2,3),byrow=T,nrow=3))
names(misShaped) <- c("treatmentA","treatmentB")
misShaped$people <- c("John","Jane","Mary")
misShaped

library(reshape2)
melt(misShaped,id.vars="people",variable.name="treatment",value.name="value")

## Quiz
con <- url("http://simplystatistics.tumblr.com/")
simplyStats <- readLines(con)
close(con)
head(simplyStats)
nchar(simplyStats)[c(2,45,122)]

## American Community Survey
fileUrl <- "https://dl.dropboxusercontent.com/u/7710864/data/csv_hid/ss06hid.csv"
download.file(fileUrl,destfile="./data/americanCommunity.csv",method="curl")
americanCommunity <- read.csv("./data/americanCommunity.csv")
dateDownloaded <- date()

###Question 3 (worth more than 1,000,000- code VAL=24)
head(americanCommunity)
str(americanCommunity)
table(americanCommunity$VAL)

### QUestion 5
#### 1- How many households have 3 bedrooms and and 4 total rooms? 
#### 2- How many households have 2 bedrooms and 5 total rooms? 
#### 3- How many households have 2 bedrooms and 7 total rooms?
#code bedrooms = BDS
#code rooms = RMS
sub1 <- as.logical(!is.na(americanCommunity$BDS) & !is.na(americanCommunity$RMS))
americanCommunity <- americanCommunity[sub1,]
str(americanCommunity)
table(americanCommunity$BDS == 3 & americanCommunity$RMS == 4,useNA="ifany")
americanCommunity[americanCommunity$BDS == 3 & americanCommunity$RMS == 4,c("BDS","RMS")]
dim(americanCommunity[americanCommunity$BDS == 3 & americanCommunity$RMS == 4,c("BDS","RMS")])

table(americanCommunity$BDS == 2 & americanCommunity$RMS == 5,useNA="ifany")

table(americanCommunity$BDS == 2 & americanCommunity$RMS == 7,useNA="ifany")

### QUestion 6
#### households on greater than 10 acres (ACR=3)
#### sold more than $10,000 worth of agriculture products (AGS=6)
americanCommunity <- read.csv("./data/americanCommunity.csv")
agricultureLogical <- americanCommunity$ACR == 3 & americanCommunity$AGS == 6
which(agricultureLogical) 

### Question 7
#### How many households in the subsetDataFrame have a missing value for the mortgage status (MRGX)
#### variable?
indexes  <-   which(agricultureLogical) 
dataFrame <- americanCommunity
subsetDataFrame   <-  dataFrame[indexes,] 
names(subsetDataFrame)
table(subsetDataFrame$MRGX,useNA="ifany")

### QUestion 8 
americanCommunity <- read.csv("./data/americanCommunity.csv")
names(americanCommunity)
strsplit
index <- grep("wgtp",names(americanCommunity))
wgtpnames <- names(americanCommunity)[index]
strsplit(names(americanCommunity),"wgtp",fixed=T)[123]

### Question 9 
americanCommunity <- read.csv("./data/americanCommunity.csv")
quantile(americanCommunity$YBL,na.rm=T)
summary(americanCommunity$YBL)

### Question 10 
fileUrl <- "https://dl.dropboxusercontent.com/u/7710864/data/csv_hid/ss06pid.csv"
download.file(fileUrl,destfile="./data/americanCommunityPop.csv",method="curl")
populationData <- read.csv("./data/americanCommunityPop.csv")
housingData <- read.csv("./data/americanCommunity.csv")
dateDownloaded <- date()
names(populationData)
names(housingData)
DF <- merge(populationData,housingData,by="SERIALNO",all=T)
DF <- merge(housingData,populationData,by="SERIALNO",all=T)
dim(DF)