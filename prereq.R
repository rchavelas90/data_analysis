## 1
dataset <- read.csv("data/dataanalysis-selfquiz-data.csv")
head(dataset)

## 2
colnames(dataset)

## 3
rownames(dataset)

##4
dataset[1:6,]

##5 
dim(dataset)[1]

## 6
len <- dim(dataset)[1]
dataset[(len-5):len,]

## 7
sum(is.na(dataset$Ozone))

## 8
mean(dataset$Ozone,na.rm=T)

## 9
dataset2 <- dataset[!is.na(dataset$Ozone) & !is.na(dataset$Temp),]
dataset2[dataset2$Ozone>31 & dataset2$Temp>90,]

## 10
vec <- c()
for (i in 1:length(dataset)){
 m <- mean(dataset[,i],na.rm=T)
 vec <- c(vec,m)
}
vec

# or

m <- numeric(6)
for (i in 1:6) {
 m[i] <- mean(dataset[, i], na.rm = TRUE)
}
print(m)


## 11
apply(dataset,2,sd,na.rm=T)

## 12 http://stat.ethz.ch/R-manual/R-devel/library/base/html/by.html
#by(dataset$Ozone,dataset$Month,mean,na.rm=T,simplify=F)
tapply(dataset$Ozone,dataset$Month,mean,na.rm=T,simplify=T)

## 13
set.seed(1)
len <- dim(dataset)[1]
dataset[sample(len,5),]
