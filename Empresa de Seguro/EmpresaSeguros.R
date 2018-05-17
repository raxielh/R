datos.scale <- as.data.frame(scale(datos[, 5:9]))

set.seed(80)

datos.km <-kmeans(datos.scale,centers = 4)

names(datos.km)

datos.km$cluster #asignacion observaciones a cluster
datos.km$totss #incercia total
datos.km$betweenss #inercia entre grupos
datos.km$withinss #inercia intra grupos
datos.km$tot.withinss #inercia intra grupos total



plot(datos$antiguedad_comp,datos$antiguedad_licencia,col=datos.km$cluster,xlab="Fidelidad Compañia",ylab="Experiancia conductor")

aggregate(datos[,5:9],by=list(datos.km$cluster),mean)



sumbt<-kmeans(datos.scale,centers = 1)$betweenss
for(i in 2:10) sumbt[i] <- kmeans(datos.scale,centers = i)$betweenss
plot(1:10,sumbt,type="b",xlab="Numero de cluster",ylab="suma de cuadros intergrupos")