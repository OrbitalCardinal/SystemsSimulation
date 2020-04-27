n <- 50
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m=runif(n)) #m agrega masa
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
png("p9i.png")
library(lattice)
xyplot(y ~ x, group=g, data=p, auto.key=list(space="right"),
       xlab="X", ylab="Y", main="Part\u{00ed}culas generadas",
       par.settings = list(superpose.symbol = list(pch = 15, cex = 1.5,
                                                   col = colores)))
graphics.off()
eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  mi <- p[i,]$m
  fx <- 0
  fy <- 0
  fxm <- 0
  fym <- 0
  fxt <- 0
  fyt <-0
  for (j in 1:n) {
    cj <- p[j,]$c
    mj <- p[j,]$m
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dirm <- (-1)^(1 + 1 * (mi < mj)) # Direccion de la atraccion gravitatoria
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    factorm <- dirm * mi*mj / ((dx^2 + dy^2) + eps)  #regla de atraccion gravitatoria
    fx <- (fx - dx * factor) 
    fy <- (fy - dy * factor) 
    fxm <- (fxm - dx * factorm) 
    fym <- (fym - dy * factorm)
    fxt <- fx + fxm #Suma de fuerza calculadas en x
    fyt <- fy + fym #suma de fuerza calculadas en y
  }
  return(c(fxt, fyt))
}

fuerzam <- function(i) { #Solo regresa fuerzas calculadas con masa
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  mi <- p[i,]$m
  fx <- 0
  fy <- 0
  fxm <- 0
  fym <- 0
  fxt <- 0
  fyt <-0
  for (j in 1:n) {
    cj <- p[j,]$c
    mj <- p[j,]$m
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dirm <- (-1)^(1 + 1 * (mi < mj))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factorm <- dirm * mi*mj / ((dx^2 + dy^2) + eps)
    fxm <- (fxm - dx * factorm) 
    fym <- (fym - dy * factorm)
    fxt <- fx + fxm
    fyt <- fy + fym
  }
  return(c(fxt, fyt))
}

fuerzac <- function(i) { #Solo regresa fuerzas calculadas con carga
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  mi <- p[i,]$m
  fx <- 0
  fy <- 0
  fxm <- 0
  fym <- 0
  fxt <- 0
  fyt <-0
  for (j in 1:n) {
    cj <- p[j,]$c
    mj <- p[j,]$m
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dirm <- (-1)^(1 + 1 * (mi < mj))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    fx <- (fx - dx * factor) 
    fy <- (fy - dy * factor) 
    fxt <- fx + fxm
    fyt <- fy + fym
  }
  return(c(fxt, fyt))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
system("rm -f p9_t*.png") # borramos anteriores en el caso que lo hayamos corrido
tmax <- 100
deltadata <- c()
deltamdata <- c()
deltacdata <- c()
for (iter in 1:tmax) {
  print(iter)
  fm <- foreach(i = 1:n, .combine=c) %dopar% fuerzam(i) #Todas las fuerzas de las particulas con solo masa
  fc <- foreach(i = 1:n, .combine=c) %dopar% fuerzac(i) #Todas las fuerzas de las particulas con solo carga
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i) #Todas las fuerzas de las particulas con ambas cantidades
  delta <- 0.02 / max(abs(f)) #Pasos con ambas
  deltam <- 0.02 / max(abs(fm)) #Pasos solo con masa
  deltac <- 0.02 / max(abs(fc)) #Pasos solo con carga
  deltadata <- c(deltadata,delta) #Fuerzas resultantes con ambas magnitudes
  deltamdata <- c(deltamdata,deltam) #Se guardan los valores de solo masa
  deltacdata <- c(deltacdata,deltac) #Se guardan los valores de solo carga
 
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
  
}
stopImplicitCluster()

library(ggplot2)
a <- data.frame(iter=seq(1,100,1), d=deltadata, dm=deltamdata, dc=deltacdata)
gg <- ggplot(a, aes(x=iter,color=d))
gg + geom_line(aes(y=d), color="red", size=1.3) + 
geom_line(aes(y=dm), color="green", size=1.3) + 
geom_line(aes(y=dc), color="blue", size=1.3) + 
scale_color_discrete(name = "Fuerzas", labels = c("red", "green", "blue")) +
ylab("Velocidad") + 
xlab("Paso") +
theme_light()


