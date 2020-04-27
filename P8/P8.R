simulacion <- function(kvar, nvar) {  #Encapsular el bloque de codigo
  
  library(testit) # para pruebas, recuerda instalar antes de usar
  k <- kvar
  n <- nvar
  originales <- rnorm(k)
  cumulos <- originales - min(originales) + 1
  cumulos <- round(n * cumulos / sum(cumulos))
  assert(min(cumulos) > 0)
  diferencia <- n - sum(cumulos)
  if (diferencia > 0) {
    for (i in 1:diferencia) {
      p <- sample(1:k, 1)
      cumulos[p] <- cumulos[p] + 1
    }
  } else if (diferencia < 0) {
    for (i in 1:-diferencia) {
      p <- sample(1:k, 1)
      if (cumulos[p] > 1) {
        cumulos[p] <- cumulos[p] - 1
      }
    }
  }
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  assert(sum(cumulos) == n)
  c <- median(cumulos) # tamanio critico de cumulos
  d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva
  rotura <- function(x) {
    return (1 / (1 + exp((c - x) / d)))
  }
  union <- function(x) {
    return (exp(-x / c))
  }
  romperse <- function(tam, cuantos) {
    romper <- round(rotura(tam) * cuantos) # independientes
    resultado <- rep(tam, cuantos - romper) # los demas
    if (romper > 0) {
      for (cumulo in 1:romper) { # agregar las rotas
        t <- 1
        if (tam > 2) { # sample no jala con un solo valor
          t <- sample(1:(tam-1), 1)
        }
        resultado <- c(resultado, t, tam - t)
      }
    }
    assert(sum(resultado) == tam * cuantos) # no hubo perdidas
    return(resultado)
  }
  unirse <- function(tam, cuantos) {
    unir <- round(union(tam) * cuantos) # independientes
    if (unir > 0) {
      division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
      assert(sum(abs(division)) == tam * cuantos)
      return(division)
    } else {
      return(rep(tam, cuantos))
    }
  }
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  
  duracion <- 50
  digitos <- floor(log(duracion, 10)) + 1
  
  for (paso in 1:duracion) {
    assert(sum(cumulos) == n)
    cumulos <- integer()
    for (i in 1:dim(freq)[1]) { # fase de rotura
      urna <- freq[i,]
      if (urna$tam > 1) { # no tiene caso romper si no se puede
        cumulos <- c(cumulos, romperse(urna$tam, urna$num))
      } else {
        cumulos <- c(cumulos, rep(1, urna$num))
      }
    }
    assert(sum(cumulos) == n)
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
    freq <- as.data.frame(table(cumulos)) # actualizar urnas
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    assert(sum(freq$num * freq$tam) == n)
    cumulos <- integer()
    for (i in 1:dim(freq)[1]) { # fase de union
      urna <- freq[i,]
      cumulos <- c(cumulos, unirse(urna$tam, urna$num))
    }
    assert(sum(abs(cumulos)) == n)
    assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
    juntarse <- -cumulos[cumulos < 0]
    cumulos <- cumulos[cumulos > 0]
    assert(sum(cumulos) + sum(juntarse) == n)
    nt <- length(juntarse)
    if (nt > 0) {
      if (nt > 1) {
        juntarse <- sample(juntarse)
        for (i in 1:floor(nt / 2) ) {
          cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
        }
      }
      if (nt %% 2 == 1) {
        cumulos <- c(cumulos, juntarse[nt])
      }
    }
    assert(sum(cumulos) == n)
    freq <- as.data.frame(table(cumulos))
    names(freq) <- c("tam", "num")
    freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
    assert(sum(freq$num * freq$tam) == n)
    
  }
}

#Implementacion sin paralelismo
replicas <- 10 #numero de replicas
tiemposSP <- c()
nseq <- seq(1000000,2500000,500000) #variacion del numero total de particulas
kseq <- seq(10000,25000,5000) #variacion del numero de cumulos existentes
for(nvar in nseq) { #ciclo de n
  for(kvar in kseq) { #ciclo de k
    for(rep in 1:replicas) { #ciclo de replicas
      tsp <- data.matrix(system.time(simulacion(kvar,nvar))[1])[1] #Obtener tiempo
      tiemposSP <- c(tiemposSP, tsp) #se guarda tiempo
    }
  }
}

#Implementacion paralela
suppressMessages(library(doParallel))
cls <- makeCluster(detectCores() - 3)
registerDoParallel(cls)
tiemposCP <- c()
for(nvar in nseq) { #ciclo de n
  for(kvar in kseq) { #ciclo de k
    tcp <- foreach(r= 1:replicas, .combine = c) %dopar% data.matrix(system.time(simulacion(kvar, nvar))[1])[1] #Paralelismo
    tiemposCP <- c(tiemposCP,tcp) #se guarda tiempo
    stopImplicitCluster()
  }
}
  

#Prueba estadistica
wilcox.test(tiemposSP, tiemposCP, alternative = "greater")


ndatos <- ((length(kseq) * length(nseq)) * replicas)
krep <- c(rep("10000",40),rep("15000",40),rep("20000",40),rep("250000",40))
nrep <- c(rep("1000000",10), rep("1500000",10),rep("2000000",10),rep("2500000",10))

datosS <- data.frame(Implementacion = rep(c("No paralela", "Paralela"),ndatos),k = krep, n = nrep, Tiempos = tiemposS)
datosC <- data.frame(Implementacion = rep(c("No paralela", "Paralela"),ndatos),k = krep, n = nrep, Tiempos = tiemposC)
library(ggplot2)
g1 <- ggplot(datosS, aes(y = Tiempos, x = k, fill = k))
g1 + geom_boxplot() + facet_grid(.~ n) + theme_light()
g2 <- ggplot(datosC, aes(y = Tiempos, x = k, fill = k))
g2 + geom_boxplot() + facet_grid(.~ n) + theme_light()

