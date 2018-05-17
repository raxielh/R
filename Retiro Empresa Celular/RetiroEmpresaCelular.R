#install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

datos <- datos[,c(3,6,7,15,18,16,1)]
names(datos)<-c("Usuario Plan Internacional","Minutos/dia","Llamadas/dia","Minutos Internacionales","Reclamos","Llamadas Internacionales","Cancelacion")

ind<-sample(2,nrow(datos),replace = TRUE,prob = c(0.7,0.3))

datosEmtrenamientos<-datos[ind==1,]
datosPrueba<-datos[ind==2,]

#creacion arbol
Arbolpart<-rpart(Cancelacion ~., method = "class",data=datosEmtrenamientos)

print(Arbolpart)
rpart.plot(Arbolpart,extra=100)

printcp(Arbolpart)
plotcp(Arbolpart)

#Podar
podaArbolpart<-prune(Arbolpart,cp=0.017172)
printcp(podaArbolpart)

#prediccion
prediccion<-predict(Arbolpart,newdata = datosPrueba, type="class")

#matriz de confucion
table(prediccion,datosPrueba$Cancelacion)

#Calculamos el % de aciertos
sum(prediccion==datosPrueba$Cancelacion)/length(datosPrueba$Cancelacion)*100

datosoriginales<-datosPrueba$Cancelacion
tabla_mostrar<-data.frame(prediccion,datosoriginales)
tabla_mostrar

