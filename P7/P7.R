g <- function(x, y) {
  return((((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100))
}
low <- -3 #Limite inferior invertido
high <- -low #Limite superior invertido
digitos <- floor(log(tmax, 10)) + 1
tmax = 30 #Numero de pasos 
step <- 0.3 #Magnitud de los pasos
for(rep in 1:15) {
  curr <- data.frame(x= runif(1, low, high), y = runif(1, low, high)) #Nueva posicion sobre la funcion
  best <- curr
  for (tiempo in 1:tmax) {
    delta <- runif(2,0,step) #Delta x, Delta Y
    vecinos <- data.frame(x = numeric(), y = numeric())
    for(cx in c(-delta[1], 0 , delta[1])) { #Cambio en X
      for(cy in c(-delta[2], 0 , delta[2])) { #Cambio en Y
        nx = curr[1] + cx #Nuevo x
        if(nx <= 3 && nx >= -3) { #Condiciones X
          ny = curr[2] + cy #Nuevo y
          if(ny <= 3 && ny >= -3) { #Condiciones Y
            vecinos <- rbind(vecinos, data.frame(x = nx, y = ny))
          }
        } 
      }
    }
    vecinos$g = g(vecinos$x, vecinos$y)     
    curr = vecinos[which.max(vecinos$g),1:2] #Maximo
    if (g(curr[1], curr[2]) > g(best[1], best[2])) { # Reemplazar mejor maximo
      best <- curr
    } 
    tl <- paste(tiempo, "", sep="")
    while (nchar(tl) < digitos) {
      tl <- paste("0", tl, sep="")
    }
    salida <- paste("C:\\Users\\raulc\\Desktop\\Practica7\\gifs\\p7_t_R",rep,"_P", tl, ".png", sep="")
    tiempo <- paste("Paso", tiempo)
    png(salida, width=500, height=400)
    x <- seq(-6, 5, 0.10)
    y <-  x
    z <- outer(x, y, g)
    image(z, xaxt = "n", yaxt = "n", main=paste("Replica: ", rep))
    b = (best + 3)/6
    points(b, col="blue", pch = 19, cex=2.4) #best
    a = (curr + 3)/6
    points(a, col="white", pch = 19, cex=1.2) # current
    graphics.off()
  }
}
