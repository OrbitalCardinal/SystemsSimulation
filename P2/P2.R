library(sna)
library(parallel)

dim <- 30 #Matriz 30x30
num <-  dim^2
data <- data.frame()

for(p in seq(0.1,0.9,0.1)) { #Variacion de la probabilidad 
numitvector <- c()
for(rep in 1:20) { #Numero de repeticiones del experimento
actual <- matrix(1 * (runif(num) < p), nrow=dim, ncol=dim)
suppressMessages(library("sna"))
paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}

cluster <- makeCluster(detectCores() - 3)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

for (iteracion in 1:30) { #Rango de iteraciones
  clusterExport(cluster, "actual")
  siguiente <- parSapply(cluster, 1:num, paso)
  if (sum(siguiente) == 0) { # todos murieron
    print("Ya no queda nadie vivo.")
    numit <- iteracion #Almacena la iteracion donde ninguna celula vive
    break;
  }
  actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
}

numitvector <- c(numitvector,numit) #Multiples valores por replica
}

data <- rbind(numitvector,data) #Agrupa los datos de todas las replicas
stopCluster(cluster)

}

row.names(data) <- c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)

boxplot(data.matrix(data), use.cols=FALSE, xlab = "Probabilidades", ylab = "N\u{FA}mero de iteraciones", col="orange",border="brown")





