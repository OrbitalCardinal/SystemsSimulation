library(testit)

suppressMessages(library(doParallel))
cls <- makeCluster(detectCores() - 3)
registerDoParallel(cls)

knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) 
  assert(n == length(valor))
  vt <- sum(valor) 
  if (pt < cap) { 
    return(vt)
  } else {
    filas <- cap + 1 
    cols <- n + 1 
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) 
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 
    }
    rownames(tabla) <- 0:cap 
    colnames(tabla) <- c(0, valor) 
    for (objeto in 1:n) { 
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        tabla[acum, objeto + 1] <- tabla[acum, objeto]                
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

p.mutar <- function() { 
  if (runif(1) < pm) {
    return(mutacion(p[i,], n))
  }
}

repro <- function() {
  padres <- sample(1:tam, 2, replace=FALSE)
  hijos_t <- reproduccion(p[padres[1],], p[padres[2],], n)
  return(hijos_t)
}

obj.f <- function() {
  obj_t <- double()
  obj_t <- c(obj_t, objetivo(p[i,], valores))
  return(obj_t)
}

fact.f <- function() {
  fact_f <- integer()
  fact_f <- c(fact_f, factible(p[i,], pesos, capacidad))
  return(fact_f)
  
}

generador.valores.correlacionados <- function(pesos,min,max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, pesos[i] + rnorm(1))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

n <- 20
pesos <- generador.pesos(n, 15, 80)
capacidad <- round(sum(pesos) * 0.65)

valores <- generador.valores(pesos, 10, 500)
valores_correlacionados <- generador.valores.correlacionados(pesos,10,500)
valores_inversos <- generador.valores.correlacionados(rev(pesos),10,500)

tiempos <- c()

for(ins in 1:3) {
  
  if(ins == 1) {
    valores <- generador.valores(pesos, 10, 500)
  } else if(ins == 2) {
    valores <- generador.valores.correlacionados(pesos,10,500)
  } else if(ins == 3) {
    valores <- generador.valores.correlacionados(rev(pesos),10,500)
  }
  
  optimo <- system.time(knapsack(capacidad, pesos, valores))
  init <- 200
  p <- poblacion.inicial(n, init)
  tam <- dim(p)[1]
  assert(tam == init)
  pm <- 0.05
  rep <- 50
  tmax <- 50
  mejores <- double()
  clusterExport(cls, c("n", "pm", "valores", "pesos", "capacidad", "objetivo"))
  
  for (iter in 1:tmax) {
    tmp1 <- Sys.time()
    print(iter)
    p$obj <- NULL
    p$fact <- NULL
    clusterExport(cls, "p")
    mut <- foreach(i=1:tam, .combine = rbind) %dopar% p.mutar() #mutacion paralela
    p <- rbind(p, mut)
    clusterExport(cls, c("tam", "p"))
    hijos <- foreach(i=1:rep, .combine = rbind) %dopar% repro() #reproduccion paralela
    p <- rbind(p, hijos[1:n]) # primer hijo
    p <- rbind(p, hijos[(n+1):(2*n)]) # segundo hijo
    tam <- dim(p)[1]
    obj <- double()
    fact <- integer()
    obj <- foreach(i=1:tam, .combine = rbind) %dopar% obj.f() #objetivos paralela
    fact <- foreach(i=1:tam, .combine = rbind) %dopar% fact.f() #factibles paralela
    p <- cbind(p, obj)
    p <- cbind(p, fact)
    mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]
    p <- p[mantener,]
    tam <- dim(p)[1]
    assert(tam == init)
    factibles <- p[p$fact == TRUE,]
    mejor <- max(factibles$obj)
    mejores <- c(mejores, mejor)
    tmp2 <- Sys.time()
    tiempos <- c(tiempos,(tmp2-tmp1))
  }
  
  
}

td <- data.frame(pasos = seq(1:50), No_correlacionado = tiempos[1:50], Correlacionado = tiempos[51:100], Inverso_correlacionado = tiempos[101:150])

plot(pesos,valores)
plot(pesos,valores_inversos)
plot(pesos,valores_correlacionados)

library(ggplot2)
gg <- ggplot(td, aes(x=pasos,color=d))
gg + geom_line(aes(y=No_correlacionado), color="red", size=1.3) + 
  geom_line(aes(y=Correlacionado), color="green", size=1.3) + 
  geom_line(aes(y=Inverso_correlacionado), color="blue", size=1.3) + 
  scale_color_discrete(name = "Fuerzas", labels = c("red", "green", "blue")) +
  ylab("Tiempo") + 
  xlab("Pasos") +
  theme_light()



plot(1:tmax, mejores, xlab="Paso", ylab="Mayor valor", type='l', ylim=c(0.95*min(mejores), 1.05*optimo))
points(1:tmax, mejores, pch=15)
abline(h=optimo, col="green", lwd=3)
print(paste(mejor, (optimo - mejor) / optimo))