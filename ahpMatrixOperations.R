options(digits=6)

###### has shown some issues with the examples of the book! must check it again

## Define pairwise matrix
a <- matrix(c(1,1/9,1/3,1/4,
              9,1,3,2,
              3,1/3,1,1/2,
              4,1/2,2,1),nrow=4,byrow=T)
a0 <- a
a
## Normalize matrix
normA <- matrix(,nrow=dim(a)[1],ncol=dim(a)[2])
for(i in 1:dim(a)[2]){
 normA[,i] <- a[,i]/sum(a[,i]) 
}
normA

## if estimates of the weights for each column of normA differer, then we must
## use the algorithm (pg 33, Golden, Wasil and Harker)
e <- matrix(1,nrow=4)
den <- a %*% e 
num <- t(e) %*% a %*% e
as.vector(den)/num ## First iteration

## seek for convergence
a <- a %*% a
e <- matrix(1,nrow=4)
den <- a %*% e 
num <- t(e) %*% a %*% e
as.vector(den)/num

## Get parameters of concistency
a <- a0
Wf <- (as.vector(den)/num)
Wf
lambMax <- sum(a[1,]*Wf)/Wf[1]
lambMax
cI <- (lambMax-dim(a)[1])/(dim(a)[1]-1)
cI
rIIndex <- which(rITable$n==dim(a)[1])
cR <- cI/rITable$RI[rIIndex]
cR

## Define the Random Inconsistency Index
rITable <- data.frame(n=c(1:15),
                      RI=c(0,0,.58,.9,1.12,1.24,1.32,1.41,1.45,1.49,1.51,1.48,1.56,
                           1.57,1.59))
rITable

################## Example #################

## Define pairwise matrix (Distance to philadelphia)
a <- matrix(c(1,1/9,1/3,1/4,
              9,1,3,2,
              3,1/3,1,1/2,
              4,1/2,2,1),nrow=4,byrow=T)
a0 <- a
a

## Define pairwise matrix (cimate)
a <- matrix(c(1,1/3,2,5,
              3,1,4,5,
              1/2,1/4,1,2,
              1/5,1/5,1/2,1),nrow=4,byrow=T)
a0 <- a
a

## Define pairwise matrix (Elementary/Highschool)
a <- matrix(c(1,5,1,4,
              1/5,1,2,2,
              1,1/2,1,2,
              1/4,1/2,1/2,1),nrow=4,byrow=T)
a0 <- a
a

## Define pairwise matrix (Colleges and Universities)
a <- matrix(c(1,2,5,6,
              1/2,1,2,3,
              1/5,1/2,1,2,
              1/6,1/3,1/2,1),nrow=4,byrow=T)
a0 <- a
a

## Define pairwise matrix (Criteria respect to goal)
a <- matrix(c(1,2,1/2,1,1/3,
              1/2,1,1/4,2,1/3,
              2,4,1,2,1,
              1/2,1/2,1/2,1,1/4,
              3,3,1,4,1),nrow=5,byrow=T)
a0 <- a
a

## Define pairwise matrix (Criteria respect to goal)
a <- matrix(c(1,1/9,1,9,1,9,1,1/9,1),nrow=3,byrow=T)
a0 <- a
a

## Normalize matrix
normA <- matrix(,nrow=dim(a)[1],ncol=dim(a)[2])
for(i in 1:dim(a)[2]){
 normA[,i] <- a[,i]/sum(a[,i]) 
}
normA

## if estimates of the weights for each column of normA differer, then we must
## use the algorithm (pg 33, Golden, Wasil and Harker)
e <- matrix(1,nrow=dim(a)[1])
den <- a %*% e 
num <- t(e) %*% a %*% e
as.vector(den)/num ## First iteration

## seek for convergence
a <- a %*% a
e <- matrix(1,nrow=dim(a)[1])
den <- a %*% e 
num <- t(e) %*% a %*% e
as.vector(den)/num

## Get parameters of concistency
a <- a0
Wf <- (as.vector(den)/num)
Wf
lambMax <- sum(a[1,]*Wf)/Wf[1]
lambMax
cI <- (lambMax-dim(a)[1])/(dim(a)[1]-1)
cI
rIIndex <- which(rITable$n==dim(a)[1])
cR <- cI/rITable$RI[rIIndex]
cR

## Define the Random Inconsistency Index
rITable <- data.frame(n=c(1:15),
                      RI=c(0,0,.58,.9,1.12,1.24,1.32,1.41,1.45,1.49,1.51,1.48,1.56,
                           1.57,1.59))
rITable
