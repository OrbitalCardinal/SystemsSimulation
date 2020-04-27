l <- 1.5
n <- 50
pi <- 0.2 #Se aumenta para mayor probabilidad de infeccion
pr <- 0.02

pvvar <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1) #Variacion de la probablidad de 0 a 1 
infectadosMax <- c()
for(pv in pvvar) { #Incremento en la probabilidad de vacunados
  for(rep in 1:30) { #Replicas del experimento 
    agentes <- data.frame(x = double(), y = double(), pmx = double(), pmy = double(), dx = double(), dy = double(),  estado  = character(), rest = integer())
    
    for (i in 1:n) {
      e <- "S"
      if(runif(1) < pv) { #Vacunar 
        e <- "R"
      } else { #Si no estan vacunados
        
        if (runif(1) < pi) {
          e <- "I"
        }
        
      }
      
      pasos <- sample(5:50,1) #Numero de pasos por recorrer
      xc <- runif(1,0,l) #Punto inicial en X
      yc <- runif(1,0,l) #Punto inicial en Y
      px = runif(1,0,l) #Punto meta en X
      py = runif(1,0,l) #Punto meta en Y
      
      vx = ((px - xc)/pasos) #Velocidad a la meta en X
      vy = ((py - yc)/pasos) #Velocidad a la meta en Y
      agentes <- rbind(agentes, data.frame(x = xc, y = yc, pmx = px, pmy = py,
                                           dx = vx, dy = vy,
                                           estado = e,rest = pasos))
      
      levels(agentes$estado) <- c("S", "I", "R")
      
    }
    
    epidemia <- integer()
    r <- 0.1
    tmax <- 100
    digitos <- floor(log(tmax, 10)) + 1
    
    
    for (tiempo in 1:tmax) {
      infectados <- dim(agentes[agentes$estado == "I",])[1]
      epidemia <- c(epidemia, infectados)
      if (infectados == 0) {
        break
      }
      contagios <- rep(FALSE, n)
      for (i in 1:n) { # posibles contagios
        a1 <- agentes[i, ]
        if (a1$estado == "I") { # desde los infectados
          for (j in 1:n) {
            if (!contagios[j]) { # aun sin contagio
              a2 <- agentes[j, ]
              if (a2$estado == "S") { # hacia los susceptibles
                dx <- a1$x - a2$x
                dy <- a1$y - a2$y
                d <- sqrt(dx^2 + dy^2)
                if (d < r) { # umbral
                  p <- (r - d) / r
                  if (runif(1) < p) {
                    contagios[j] <- TRUE
                  }
                }
              }
            }
          }
        }
      }
      
      
      
      
      
      
      for (i in 1:n) { # movimientos y actualizaciones
        a <- agentes[i, ]
        if (contagios[i]) {
          a$estado <- "I"
        } else if (a$estado == "I") { # ya estaba infectado
          if (runif(1) < pr) {
            a$estado <- "R" # recupera
          }
        }
        a$x <- a$x + a$dx #Avanza con la velocidad en x 
        a$y <- a$y + a$dy #Avanza con la velocidad en y
        if(a$rest != 1){ 
          a$rest <- a$rest - 1 #Resta uno mientras no haya alcanzado el punto meta
          
        } else {
          #Nuevos puntos meta y velocidades
          pasos <- sample(5:30,1)
          a$rest <- pasos
          a$x <- a$pmx
          a$y <- a$pmy
          a$pmx <- runif(1,0,l)
          a$pmy <- runif(1,0,l)
          vx = (a$pmx - a$x)/pasos
          vy = (a$pmy - a$y)/pasos
          a$dx <- vx
          a$dy <- vy
          
        }
        agentes[i, ] <- a
      }
      
      
      
      
    }
    
    infectadosMax <- c(infectadosMax, max(epidemia)) #Se guardan los infectados maximos por cada replica
    
  }
  
}


datos <- data.frame(Probabilidad_de_vacuna = c(rep("0",20),rep("0.1",20),rep("0.2",20),rep("0.3",20),rep("0.4",20),rep("0.5",20),rep("0.6",20),rep("0.7",20),rep("0.8",20),rep("0.9",20),rep("1",20)), Infectados_maximos = (infectadosMax/n)*100)

library(ggplot2)
tiff("C:\\Users\\OrbitalCardinal\\Desktop\\Practica 6\\graficas\\Grafica2p6.tiff", units="in", width=12, height=6.8, res=300, compression = 'lzw')
g <- ggplot(datos, aes(Probabilidad_de_vacuna,Infectados_maximos))
g + geom_boxplot(alpha = 0.5,aes(fill= Probabilidad_de_vacuna)) + theme_bw() + labs(x= "Probabilidad de vacuna",y = "Porcentaje m\u{E1}ximo de infectados", fill ="Probabilidad de vacuna" )
graphics.off()