datosProv <- read.csv("~/Desktop/Proveedores/DatosCaptura.csv")
summary(datosProv)
head(datosProv)
dim(datosProv)
# Obtener el tiempo de servicio y ordena el DF
tiempo <- datosProv[,c(2,3)]
tiempo$llegadaMin <- as.POSIXct(tiempo$Llegada,format="%H:%M") 
tiempo$salidaMin <- as.POSIXct(tiempo$Salida,format="%H:%M") 
tiempo$difrenciaMin <- tiempo$salidaMin-tiempo$llegadaMin
datosProv$tiempoServicio <- as.numeric(tiempo$difrenciaMin)
cols <- ncol(datosProv)
datosProv2 <- datosProv[,c(1:3,cols,4:(cols-2))]
# str(datosProv2)
dim(datosProv2)

## quitar tiempo atipicos
# boxplot(datosProv2$tiempoServicio)
index <- which(datosProv2$tiempoServicio>30)
datosProv3 <- datosProv2[-index,]

hist(datosProv3$tiempoServicio) ## ver tiempos de servicio
boxplot(datosProv3$tiempoServicio~datosProv3$Negocio)

## Graficar tiempo de servicio en función de la distancia
regresion <- lm(tiempoServicio~Distancia,datosProv3)
summary(regresion)
plot(datosProv3$tiempoServicio,datosProv3$Distancia)
abline(regresion)

#ver tiempos de servicio por distnitos factores
str(datosProv3)
library(ggplot2)
#hist(datosProv3$tiempoServicio) ## ver tiempos de servicio
#ggplot(datosProv3,aes(tiempoServicio))+
# geom_histogram(binwidth=2.2)+
# facet_grid(.~Negocio)

ggplot(datosProv3,aes(Negocio,tiempoServicio))+
 geom_boxplot()

## Juntar tiempos: Convenience&FoodDring y Grocery&Kiosk
head(datosProv3)
table(datosProv3$Negocio,useNA="ifany")

idexGroceryKiosk <- datosProv3$Negocio == "Grocery" | datosProv3$Negocio=="Kiosk"
datosProvGroseryKiosk <- datosProv3[idexGroceryKiosk,]
datosProvGroseryKiosk

idexFoodConvenience <- datosProv3$Negocio == "FoodDrink" | datosProv3$Negocio=="Convenience"
datosFoodConvenience <- datosProv3[idexFoodConvenience,]
by(datosFoodConvenience,datosFoodConvenience$Entrega,function(x) sd(x$tiempoServicio))

## porcentaje que se van a uno u a otro
## Grocery y kiosk
100*sum(idexGroceryKiosk)/
 (sum(idexGroceryKiosk)+sum(idexFoodConvenience))
## food y convenience
100*sum(idexFoodConvenience)/
 (sum(idexGroceryKiosk)+sum(idexFoodConvenience))

## evaluar por manual o diablito (para Grocery y Kiosk)
ggplot(datosProvGroseryKiosk,aes(Entrega,tiempoServicio))+
 geom_boxplot()
contingGroceryKioskEntrega <- as.data.frame(table(datosProvGroseryKiosk$Entrega))
contingGroceryKioskEntrega$percent <- contingGroceryKioskEntrega$Freq/
 sum(contingGroceryKioskEntrega$Freq)
contingGroceryKioskEntrega

## evaluar por manual o diablito (para Food and Convenience)
ggplot(datosFoodConvenience,aes(Entrega,tiempoServicio))+
 geom_boxplot()
contingFoodConvenienceEntrega <- as.data.frame(table(datosFoodConvenience$Entrega))
contingFoodConvenienceEntrega$percent <- contingFoodConvenienceEntrega$Freq/
 sum(contingFoodConvenienceEntrega$Freq)
contingFoodConvenienceEntrega

## Evaluar cuantos regresan dos o más veces que entran manual
indiceVariasEntradas <- which(datosProv3$Entrega=="Manual" & datosProv3$Veces>1) 
nrow(datosProv3[indiceVariasEntradas,])/
 nrow(datosProv3[-indiceVariasEntradas,])/
 nrow(datosProv3)
nrow(datosProv3[-indiceVariasEntradas,])
as.data.frame(table(datosProv3$Entrega,datosProv3$Veces))

## Tiempos promedio para tipo de entrega por clasificación de tiendas (GroseryKiosk)
indiceManual <- which(datosProvGroseryKiosk$Entrega=="Manual")
datosProvGroseryKiosk$tipoEntregaSummary <- c("")
datosProvGroseryKiosk$tipoEntregaSummary[indiceManual] <-c("Manual")
datosProvGroseryKiosk$tipoEntregaSummary[-indiceManual] <-c("Diablito")
datosProvGroseryKiosk[-indiceManual,]
datosProvGroseryKiosk[c("Entrega","tipoEntregaSummary")]

by(datosProvGroseryKiosk,
   datosProvGroseryKiosk$tipoEntregaSummary,
   function(x) mean(x$tiempoServicio))

by(datosProvGroseryKiosk,
   datosProvGroseryKiosk$tipoEntregaSummary,
   function(x) sd(x$tiempoServicio))

ggplot(datosProvGroseryKiosk,aes(tipoEntregaSummary,tiempoServicio))+
 geom_boxplot()

## Tiempos promedio para tipo de entrega por clasificación de tiendas (GroseryKiosk)
indiceManual <- which(datosFoodConvenience$Entrega=="Manual")
datosFoodConvenience$tipoEntregaSummary <- c("")
datosFoodConvenience$tipoEntregaSummary[indiceManual] <-c("Manual")
datosFoodConvenience$tipoEntregaSummary[-indiceManual] <-c("Diablito")
datosFoodConvenience[-indiceManual,]
datosFoodConvenience[c("Entrega","tipoEntregaSummary")]

by(datosFoodConvenience,
   datosFoodConvenience$tipoEntregaSummary,
   function(x) mean(x$tiempoServicio))

by(datosFoodConvenience,
   datosFoodConvenience$tipoEntregaSummary,
   function(x) sd(x$tiempoServicio))

ggplot(datosFoodConvenience,aes(tipoEntregaSummary,tiempoServicio))+
 geom_boxplot()


