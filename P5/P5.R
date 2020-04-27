library(distr)
library(ggplot2)
library(gridExtra)
library(grid)
library(lattice)

inicio <- -6
final <- -inicio
paso <- 0.25
x <- seq(inicio, final, paso)
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
plot(x,  (2/pi) * (1/(exp(x)+exp(-x))))
lines(x,  (2/pi) * (1/(exp(x)+exp(-x))), type="l")
graphics.off()
suppressMessages(library(distr))
g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador

muestra <- generador(50000) # sacamos una muestra
hist(muestra, freq=F, breaks=50,
     main="Histograma de g(x) comparado con g(x)",
     xlim=c(inicio, final), ylim=c(0, 0.4))
lines(x, g(x), col="red") # dibujamos g(x) encima del histograma
graphics.off()

desde <- 3
hasta <- 7
pedazo <- 50000
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 3))
parte <- function() {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}

resultadoEsp <- (pi/2) * 0.031089 #Se establece el resultado esperado
resultadoEsp_str <- strsplit(paste(resultadoEsp),split = "") #String del reusltado esperado
datos <- c()
datos_str <- c()

replicas <- 10 #Replicas del experimento

for(i in 1:replicas) { 
  for(j in c(1000,2500,5000,10000)){ #Variacion de las muestras
    montecarlo <- foreach(i = 1:j, .combine=c) %dopar% parte()
    stopImplicitCluster()
    integral <- sum(montecarlo) / (j * pedazo)
    resultado <- (pi / 2) * integral
    datos <- c(datos,resultado)
    
  }
  
}

resultados <- c()
resultados_str <- c()
res_contadores <- c()

for(i in 1:length(data.matrix(datos))) {
  resultados <- c(resultados,data.matrix(datos)[i])
  resultados_str <- c(resultados_str, strsplit(paste(resultados[i]), split=""))
}

for(i in 1:length(resultados)) {
  contador <- 0
  for(j in 1:7) {
    b <- (unlist(resultados_str[i])[j]) == (unlist(resultadoEsp_str)[j])
    if(b) {
      contador <- contador + 1
    } else {
      break;
    }
    
  }
  
  res_contadores <- c(res_contadores,contador-2)
}

Decimales_Esperadas = 7
graf <- data.frame(T.Muestra = c(rep("500",10),rep("1000",10),rep("2000",10),rep("5000",10)), Decimales = res_contadores)
graf2 <- data.frame(T.Muestra = c(rep("500",10),rep("1000",10),rep("2000",10),rep("5000",10)), Resultado = resultados)

g <- ggplot(graf, aes(T.Muestra,Decimales,fill=T.Muestra)) + geom_hline(aes(yintercept=Decimales_Esperadas,size=Decimales_Esperadas),colour="blue")  + geom_violin(alpha=0.5, draw_quantiles = c(0.25, 0.5, 0.75), trim = FALSE,adjust=1.5) + geom_jitter() 


p <- ggplot(graf2, aes(T.Muestra,Resultado,fill=T.Muestra))+ geom_hline(aes(yintercept=resultadoEsp,size=resultadoEsp),colour="purple")+ geom_violin(alpha=0.5, draw_quantiles = c(0.25, 0.5, 0.75), trim = FALSE,adjust=1.5) + geom_jitter() 

grid.arrange(g, p, nrow = 1)















