download.file("https://spark-public.s3.amazonaws.com/dataanalysis/loansData.rda",
              destfile="./data/loansData.rda",method="curl")
a <- load("./data/loansData.rda")
a

## Object loaded loansData
table(loansData$Employment.Length)
head(loansData)
summary(loansData$Employment.Length)
unique(loansData$Loan.Purpose)
unique(loansData$Home.Ownership)
credLines <- unique(loansData$Open.CREDIT.Lines)
sort(credLines,na.last=T)
plot(table(loansData$Open.CREDIT.Lines))

## analyse object
head(loansData)
str(loansData)
summary(loansData)

## Amount funded strage
loansData[which(loansData$Amount.Funded<1),]
### No problem with this

## Interest rate as numeric
str(loansData$Interest.Rate)
testInterestRate <- loansData$Interest.Rate
testInterestRate <- as.character(testInterestRate)
testInterestRate <- strsplit(testInterestRate,"%")
testInterestRate <- unlist(testInterestRate)
testInterestRate <- as.numeric(testInterestRate)
loansData$Interest.Rate <- testInterestRate

##  Debt.To.Income.Ratio as numeric
str(loansData$Debt.To.Income.Ratio)
testDTI <- loansData$Debt.To.Income.Ratio
testDTI <- as.character(testDTI)
testDTI <- strsplit(testDTI,"%")
testDTI <- unlist(testDTI)
testDTI <- as.numeric(testDTI)
loansData$Debt.To.Income.Ratio <- testDTI

## Check for names in the dataset
names(loansData)
head(loansData)
summary(loansData)

## Check for missing values
any(is.na(loansData$Amount.Requested))
apply(loansData,2,function(x){any(is.na(x))})
apply(loansData,2,function(x){which(is.na(x))})
### missing values in Inquires.in.the.Last.6.Months, Open.CREDIT.Lines, Monthly.Income and 
### Revolving.CREDIT.Balance

## Remove missing data
loansData[which(is.na(loansData$Revolving.CREDIT.Balance)),]
naIndex <- which(is.na(loansData$Revolving.CREDIT.Balance))
loansData <- loansData[-naIndex,]
apply(loansData,2,function(x){any(is.na(x))})
summary(loansData)

##plot Interest rate by other variables
plot(loansData$Interest.Rate~loansData$FICO.Range)

## Change FICO rage to number
head(loansData)
FICO.number <- loansData$FICO.Range
loansData$FICO.Range <- as.numeric(lapply(strsplit(as.character(FICO.number),split="-"),"[",1))


plot(loansData$Interest.Rate~loansData$FICO.Range) ## This one!
smoothScatter(loansData$FICO.Range,loansData$Interest.Rate)



plot(loansData$Interest.Rate~loansData$Amount.Funded.By.Investors)
smoothScatter(loansData$Amount.Funded.By.Investors,loansData$Interest.Rate)
summary(lm(loansData$Interest.Rate~loansData$Amount.Funded.By.Investors))

plot(loansData$Interest.Rate~loansData$Loan.Length) ## This one!

plot(loansData$Interest.Rate~loansData$Loan.Purpose) ## Not

plot(loansData$Interest.Rate~loansData$Debt.To.Income.Ratio,pch=19)
smoothScatter(loansData$Debt.To.Income.Ratio,loansData$Interest.Rate) ## NOt
summary(lm(loansData$Interest.Rate~loansData$Debt.To.Income.Ratio))

## Plots by state
any(is.na(loansData$State))

### Remove nos used states ###
loansData$State <- droplevels(loansData$State)
table(loansData$State)
plot(loansData$Interest.Rate~loansData$State,pch=19)

### Get means by states
stateData <- by(loansData,loansData$State,function(x){mean(x$Interest.Rate)},simplify=T)
statesMeanInterest <- data.frame(sapply(stateData,"[",1)) ### Some factors not used
statesMeanInterest$states <- row.names(statesMeanInterest)
row.names(statesMeanInterest) <- NULL
names(statesMeanInterest) <- c("MeanInterest","State")
statesMeanInterest$State
statesMeanInterest$State2 <- paste("US-",statesMeanInterest$State,sep="")
plot(statesMeanInterest$MeanInterest~factor(statesMeanInterest$State,ordered=T))
index <- order(statesMeanInterest$MeanInterest)
statesMeanInterest <- statesMeanInterest[index,]

### Plot by state
library(googleVis)
stateMeanInterest <- gvisGeoChart(statesMeanInterest,locationvar="State2",sizevar="MeanInterest",
                                  options=list(
                                   region="US",
                                   displayMode="regions", 
                                   resolution="provinces"
                                   ))
plot(stateMeanInterest)

head(loansData)

plot(loansData$Interest.Rate~loansData$Home.Ownership) ### Here kind of interesting (NOT)
## remove Monthly.Income value
plot(loansData$Interest.Rate~loansData$Monthly.Income,pch=19)
smoothScatter(loansData$Monthly.Income,loansData$Interest.Rate)
hist(loansData$Monthly.Income)

superIncome <- which(!loansData$Monthly.Income<39000)
loansData <- loansData[-superIncome,]
hist(log10(loansData$Monthly.Income)) ## doesnt seem to be anything interesting so far in this
interestByIncome <- lm(loansData$Interest.Rate~log10(loansData$Monthly.Income))
summary(interestByIncome)
head(loansData)
abline(interestByIncome)

### Interesting!!!
plot(loansData$Debt.To.Income.Ratio~loansData$Monthly.Income,pch=19)
smoothScatter(loansData$Monthly.Income,loansData$Debt.To.Income.Ratio)
debtIncRatioByIncome <- lm(loansData$Debt.To.Income.Ratio~loansData$Monthly.Income)
summary(debtIncRatioByIncome)
abline(debtIncRatioByIncome)

plot(loansData$Interest.Rate~loansData$Open.CREDIT.Lines,pch=19)
smoothScatter(loansData$Open.CREDIT.Lines,loansData$Interest.Rate)
interestByCredit <- lm(loansData$Interest.Rate~loansData$Open.CREDIT.Lines)
summary(interestByCredit)
abline(interestByCredit) ## ALSO interesting

## Reomve Revolving Credit Balance Outliers
loansData <- loansData[which(loansData$Revolving.CREDIT.Balance<150000),]
plot(loansData$Interest.Rate~loansData$Revolving.CREDIT.Balance,pch=19)
smoothScatter(loansData$Revolving.CREDIT.Balance,loansData$Interest.Rate)
interestByRevolving <- lm(loansData$Interest.Rate~loansData$Revolving.CREDIT.Balance)
summary(interestByRevolving)


plot(loansData$Interest.Rate~jitter(loansData$Inquiries.in.the.Last.6.Months),pch=19)
smoothScatter(loansData$Inquiries.in.the.Last.6.Months,loansData$Interest.Rate)
interestByInquiries <- lm(loansData$Interest.Rate~loansData$Inquiries.in.the.Last.6.Months)
summary(interestByInquiries)

### NOB BY FICO
plot(loansData$Interest.Rate~loansData$FICO.Range,pch=19)
smoothScatter(loansData$FICO.Range,loansData$Interest.Rate)
head(loansData)
interestByFico <- lm(loansData$Interest.Rate~loansData$FICO.Range)
summary(interestByFico)
abline(interestByFico)

## SVD of all numeric variables
str(loansData)
prcomp()
p <- prcomp(loansData[,c(1,2,3,6,9,10,11,12,13)])
plot(p$sdev^2 / sum(p$sdev^2))
round((p$sdev^2 / sum(p$sdev^2)), 3)
plot(p$rotation[,1], pch=19)
round(p$rotation[,1], 2)
round(p$rotation[,1:5], 2)


############################ Model
## Model
head(loansData)
lmFinal0 <- lm(loansData$Interest.Rate~
               loansData$FICO.Range)
summary(lmFinal0)
confint(lmFinal0)

lmFinal <- lm(loansData$Interest.Rate~
               loansData$FICO.Range+
               loansData$Amount.Requested+
               loansData$Loan.Length)
summary(lmFinal)
confint(lmFinal)
############################ Final Plots
cx <- 1.2
pdf(file="./plots/InterestFinal.pdf",height=4,width=12)
par(mfrow=c(1,3))
smoothScatter(loansData$FICO.Range,loansData$Interest.Rate,
              xlab="FICO Score",ylab="Interest Rate (%)",cex.axis = cx,cex.lab = cx)
abline(lm(loansData$Interest.Rate~loansData$FICO.Range))

smoothScatter(loansData$Amount.Requested,loansData$Interest.Rate,
              xlab="Amount Requested ($)",ylab="Interest Rate (%)",cex.axis = cx,cex.lab = cx)
abline((lm(loansData$Interest.Rate~loansData$Amount.Requested)))

plot(loansData$Interest.Rate~loansData$Loan.Length,
     xlab="Loan Length (months) ",ylab="Interest Rate (%)",cex.axis = cx,cex.lab = cx) 
dev.off() ## done with the device


loansData$Loan.Length <- droplevels(loansData$Loan.Length)
smoothScatter(loansData$Amount.Requested,loansData$FICO.Range)
resetPar()

head(loansData)