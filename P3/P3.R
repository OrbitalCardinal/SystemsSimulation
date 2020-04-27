library(foreach)

#Leer archivo de numeros primos
lista <- read.csv("C:\\Users\\OrbitalCardinal\\Desktop\\Practica 3\\Primos 10k - 20k.txt", sep="", header=FALSE)

#Extraer los numeros primos del archivo en un vector
nums <- c()
for(i in 1:129)
{ 
  for(j in 1:8) {
    nums <- c(nums,lista[i,j])
  }
}

primo <- function(n) { #Calcula primos y no primos
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}


#Crear vectores de primos y no primos
primos <- c()
noprimos <- c()
desde <- nums[1]
hasta <-  nums[length(nums)]
for(i in desde:hasta) {
  if(primo(i) == TRUE) {
    primos <- c(primos,i)
  }
  if(primo(i) == FALSE) {
    noprimos <- c(noprimos,i)
  }
}

#Igualamos la cantidad de valores dentro del vector "noprimos" a la de "primos"
noprimos <- sample(noprimos[1]:noprimos[length(noprimos)], length(primos))

#Mezclamos los vectores variando las proporciones de primos y no primos dentro de el
#25/75
mvectores1 <- c()
mvectoresTEMP1 <- c()
mvectoresTEMP1 <- c(mvectoresTEMP1,sample(primos,length(primos)*0.25))
mvectoresTEMP1 <- c(mvectoresTEMP1, sample(noprimos,length(noprimos)*0.75))
mvectores1 <- sample(mvectoresTEMP1)

#50/50
mvectores2 <- c()
mvectoresTEMP2 <- c()
mvectoresTEMP2 <- c(mvectoresTEMP2,sample(primos,length(primos)*0.5))
mvectoresTEMP2 <- c(mvectoresTEMP2, sample(noprimos,length(noprimos)*0.5))
mvectores2 <- sample(mvectoresTEMP2)

#75/25
mvectores3 <- c()
mvectoresTEMP3 <- c()
mvectoresTEMP3 <- c(mvectoresTEMP3,sample(primos,length(primos)*0.75))
mvectoresTEMP3 <- c(mvectoresTEMP3, sample(noprimos,length(noprimos)*0.25))
mvectores3 <- sample(mvectoresTEMP3)

#Rangos del vector con proporcionalidad 50% primos / 50% no primos
original1 <- mvectores1[1]:mvectores1[length(mvectores1)]
invertido1 <- mvectores1[length(mvectores1)]:mvectores1[1]

#Rangos del vector con proporcionalidad 25% primos / 75% no primos
original2 <- mvectores2[1]:mvectores2[length(mvectores2)]
invertido2 <- mvectores2[length(mvectores2)]:mvectores2[1]

#Rangos del vector con proporcionalidad 75% primos / 25% no primos
original3 <- mvectores3[1]:mvectores3[length(mvectores3)]
invertido3 <- mvectores3[length(mvectores3)]:mvectores3[1]

suppressMessages(library(doParallel))
Aot1 <-  numeric()
Ait1 <-  numeric()
Aat1 <-  numeric()

Aot2 <-  numeric()
Ait2 <-  numeric()
Aat2 <-  numeric()

Aot3 <-  numeric()
Ait3 <-  numeric()
Aat3 <-  numeric()

Bot1 <-  numeric()
Bit1 <-  numeric()
Bat1 <-  numeric()

Bot2 <-  numeric()
Bit2 <-  numeric()
Bat2 <-  numeric()

Bot3 <-  numeric()
Bit3 <-  numeric()
Bat3 <-  numeric()

Cot1 <-  numeric()
Cit1 <-  numeric()
Cat1 <-  numeric()

Cot2 <-  numeric()
Cit2 <-  numeric()
Cat2 <-  numeric()

Cot3 <-  numeric()
Cit3 <-  numeric()
Cat3 <-  numeric()



nucleos <- detectCores(logical = FALSE)-1 #Numero de nucleos del computador
replicas <- 10 #Numero de veces a repetir el experimento

for(p in 1:3) { #Opciones de proporcion 1- 25%, 2- 50%, 3- 75%
  if(p == 1) {
    for(nuc in nucleos:1) { #Variacion el numero de nucleos
      registerDoParallel(makeCluster(detectCores() - nuc))
      
      for (r in 1:replicas) { #Repetir el experimento cuantas veces la variable replica indique
        
        if(nuc == 1) { #comparando si se asigno 1 nucleo
          Aot1 <- c(Aot1, system.time(foreach(n = original1, .combine=c) %dopar% primo(n))[3]) # de menor a mayor 
          Ait1 <- c(Ait1, system.time(foreach(n = invertido1, .combine=c) %dopar% primo(n))[3]) # de mayor a menor 
          Aat1 <- c(Aat1, system.time(foreach(n = sample(original1), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
        
        }
        if(nuc == 2) {
          Aot2 <- c(Aot2, system.time(foreach(n = original1, .combine=c) %dopar% primo(n))[3]) # de menor a mayor 
          Ait2 <- c(Ait2, system.time(foreach(n = invertido1, .combine=c) %dopar% primo(n))[3]) # de mayor a menor 
          Aat2 <- c(Aat2, system.time(foreach(n = sample(original1), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
          
        }
        if(nuc == 3) {
          Aot3 <- c(Aot3, system.time(foreach(n = original1, .combine=c) %dopar% primo(n))[3]) # de menor a mayor 
          Ait3 <- c(Ait3, system.time(foreach(n = invertido1, .combine=c) %dopar% primo(n))[3]) # de mayor a menor 
          Aat3 <- c(Aat3, system.time(foreach(n = sample(original1), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
          
        }
        
        
      }
      stopImplicitCluster()
    }
  }
  
  if(p == 2) { #comprobando si se asigno 2 nucleos
    for(nuc in nucleos:1) { #Variacion el numero de nucleos
      registerDoParallel(makeCluster(detectCores() - nuc))
      
      for (r in 1:replicas) { #Repetir el experimento cuantas veces la variable replica indique
        
        if(nuc == 1) {
          Bot1 <- c(Bot1, system.time(foreach(n = original2, .combine=c) %dopar% primo(n))[3]) # de menor a mayor 
          Bit1 <- c(Bit1, system.time(foreach(n = invertido2, .combine=c) %dopar% primo(n))[3]) # de mayor a menor 
          Bat1 <- c(Bat1, system.time(foreach(n = sample(original2), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
          
        }
        if(nuc == 2) {
          Bot2 <- c(Bot2, system.time(foreach(n = original2, .combine=c) %dopar% primo(n))[3]) # de menor a mayor 
          Bit2 <- c(Bit2, system.time(foreach(n = invertido2, .combine=c) %dopar% primo(n))[3]) # de mayor a menor 
          Bat2 <- c(Bat2, system.time(foreach(n = sample(original2), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
          
        }
        if(nuc == 3) {
          Bot3 <- c(Bot3, system.time(foreach(n = original2, .combine=c) %dopar% primo(n))[3]) # de menor a mayor 
          Bit3 <- c(Bit3, system.time(foreach(n = invertido2, .combine=c) %dopar% primo(n))[3]) # de mayor a menor 
          Bat3 <- c(Bat3, system.time(foreach(n = sample(original2), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
          
        }
        
        
      }
      stopImplicitCluster()
    }
  }
  
  if(p == 3) { #comprobando si se asigno 3 nucleos
    for(nuc in nucleos:1) { #Variacion el numero de nucleos
      registerDoParallel(makeCluster(detectCores() - nuc))
      
      for (r in 1:replicas) { #Repetir el experimento cuantas veces la variable replica indique
        
        if(nuc == 1) {
          Cot1 <- c(Cot1, system.time(foreach(n = original3, .combine=c) %dopar% primo(n))[3]) # de menor a mayor 
          Cit1 <- c(Cit1, system.time(foreach(n = invertido3, .combine=c) %dopar% primo(n))[3]) # de mayor a menor 
          Cat1 <- c(Cat1, system.time(foreach(n = sample(original3), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
          
        }
        if(nuc == 2) {
          Cot2 <- c(Cot2, system.time(foreach(n = original3, .combine=c) %dopar% primo(n))[3]) # de menor a mayor 
          Cit2 <- c(Cit2, system.time(foreach(n = invertido3, .combine=c) %dopar% primo(n))[3]) # de mayor a menor 
          Cat2 <- c(Cat2, system.time(foreach(n = sample(original3), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
          
        }
        if(nuc == 3) {
          Cot3 <- c(Cot3, system.time(foreach(n = original3, .combine=c) %dopar% primo(n))[3]) # de menor a mayor 
          Cit3 <- c(Cit3, system.time(foreach(n = invertido3, .combine=c) %dopar% primo(n))[3]) # de mayor a menor 
          Cat3 <- c(Cat3, system.time(foreach(n = sample(original3), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
          
        }
        
        
      }
      stopImplicitCluster()
    }
  }
}

tiempos1 <- c()
tiempos2 <- c()
tiempos3 <- c()

for(t in 1:replicas){
  
  tiempos1 <- c(tiempos1,Aot1[t])
  tiempos1 <- c(tiempos1,Ait1[t])
  tiempos1 <- c(tiempos1,Aat1[t])
  tiempos1 <- c(tiempos1,Aot2[t])
  tiempos1 <- c(tiempos1,Ait2[t])
  tiempos1 <- c(tiempos1,Aat2[t])
  tiempos1 <- c(tiempos1,Aot3[t])
  tiempos1 <- c(tiempos1,Ait3[t])
  tiempos1 <- c(tiempos1,Aat3[t])
  
  tiempos2 <- c(tiempos2,Bot1[t])
  tiempos2 <- c(tiempos2,Bit1[t])
  tiempos2 <- c(tiempos2,Bat1[t])
  tiempos2 <- c(tiempos2,Bot2[t])
  tiempos2 <- c(tiempos2,Bit2[t])
  tiempos2 <- c(tiempos2,Bat2[t])
  tiempos2 <- c(tiempos2,Bot3[t])
  tiempos2 <- c(tiempos2,Bit3[t])
  tiempos2 <- c(tiempos2,Bat3[t])
  
  tiempos3 <- c(tiempos3,Cot1[t])
  tiempos3 <- c(tiempos3,Cit1[t])
  tiempos3 <- c(tiempos3,Cat1[t])
  tiempos3 <- c(tiempos3,Cot2[t])
  tiempos3 <- c(tiempos3,Cit2[t])
  tiempos3 <- c(tiempos3,Cat2[t])
  tiempos3 <- c(tiempos3,Cot3[t])
  tiempos3 <- c(tiempos3,Cit3[t])
  tiempos3 <- c(tiempos3,Cat3[t])

  
}

Ndatos <- (nucleos*replicas)

Dnucleos1 <- data.frame(Nucleos=c(rep(1,Ndatos),rep(2,Ndatos),rep(3,Ndatos)), Orden= c("1","2","3"), Tiempos= tiempos1)
Dnucleos2 <- data.frame(Nucleos=c(rep(1,Ndatos),rep(2,Ndatos),rep(3,Ndatos)), Orden= c("1","2","3"), Tiempos= tiempos2)
Dnucleos3 <- data.frame(Nucleos=c(rep(1,Ndatos),rep(2,Ndatos),rep(3,Ndatos)), Orden= c("1","2","3"), Tiempos= tiempos3)

# The palette with grey:
cbPalette <- c("#A5DEEF", "#E9C590", "#D0ACE2")

p <- ggplot(Dnucleos1, aes(x= Orden, y = Tiempos, colour=Orden,fill=Orden))
p + geom_boxplot(alpha=0.8) + facet_grid(~Nucleos) + scale_fill_manual(values=cbPalette) + scale_colour_manual(values=c("blue","red","purple"))

p <- ggplot(Dnucleos2, aes(x= Orden, y = Tiempos, colour=Orden,fill=Orden))
p + geom_boxplot(alpha=0.8) + facet_grid(~Nucleos) + scale_fill_manual(values=cbPalette) + scale_colour_manual(values=c("blue","red","purple"))


p <- ggplot(Dnucleos3, aes(x= Orden, y = Tiempos, colour=Orden,fill=Orden))
p + geom_boxplot(alpha=0.8) + facet_grid(~Nucleos) + scale_fill_manual(values=cbPalette) + scale_colour_manual(values=c("blue","red","purple"))








