lista <- read.csv("data/Lista_equipos.csv",skip=1,header=F)
head(lista)
tail(lista)
duplicated(lista$V3)

tabla <- data.frame(producto=c(1,2,3,4,5,6,1,7,8,9,2),precio=rnorm(11,mean=1000,sd=200))
tabla
 tabla$producto[duplicated(tabla$producto)]
 tabla2<- tabla[tabla$producto==1,]
write.csv(file="tabla2.csv",x=tabla2)

gasolData <- read.csv("./data/gasoline.csv",header=F,nrow=15)
plot(gasolData$V2,gasolData$V3,type="l",xlim=c(0,15), ylim=c(0,15))
lm(gasolData$V2~poly(gasolData$V3,))
polinom <- poly(gasolData$V3,1)
plot(polinom)