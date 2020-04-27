library(parallel)

repetir <- 30 #Repeticiones
duracion <- 2^6 #Reemplazar valores del exponente uno a uno desde 6 hasta 12
eucl <- FALSE #Calcular distancia Euclidiana
dim  <- 8 #No de dimensiones

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "duracion")
clusterExport(cluster, "repetir")
clusterExport(cluster, "eucl")
pdatos <- data.frame()

for (dimension in 1:dim) {
  pOrigen <- 0
  clusterExport(cluster, "dimension")
  resultadoP <- parSapply(cluster, 1:repetir,
                         function(r) {
                           pos <- rep(0, dimension)
                           mayor <- 0
                           pOrigen <- 0
                           for (t in 1:duracion) { #mod
                             cambiar <- sample(1:dimension, 1)
                             cambio <- 1
                             if (runif(1) < 0.5) {
                               cambio <- -1
                             }
                             pos[cambiar] <- pos[cambiar] + cambio
                             
                             if(all(pos==0)) #Comprobar que haya regresado a los puntos de origen
                             {
                               pOrigen <- pOrigen + 1  
                             }
                           }
                           return((pOrigen/duracion)*100)
                         })
  
  pdatos <- rbind(pdatos,resultadoP)
  
}

boxplot(data.matrix(pdatos), use.cols=FALSE,main="2^7",xlab = "Dimensión", ylab = "P. de regreso al origen en %")

#par(mfrow=c(2,3))

#png("C://Users//raulc//Desktop//Practica 1//2e12")



#graphics.off()


