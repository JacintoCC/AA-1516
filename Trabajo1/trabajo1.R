# Generación y visualización de datos

simula_unif <- function(N, dim, rango){
  lista <- matrix(runif(N*dim, min =  rango[1], max = rango[2]), dim, N)
  return(lista)
}

simula_gauss <- function(N, dim, sigma){
  lista <- matrix(rnorm(N*dim, sd = sigma), dim, N)
  return(lista)
}

lista_unif <- simula_unif(50, 2, c(-50, 50))
lista_gauss <- simula_gauss(50, 2, c(5,7))

plot(lista_gauss[1,],lista_gauss[2,], xlab="Eje X", ylab="Eje Y")
plot(lista_unif[1,], lista_unif[2,], xlab="Eje X", ylab="Eje Y")

boxplot(lista_unif[1,],lista_unif[2,])
boxplot(lista_gauss[1,],lista_gauss[2,])

simula_recta <- function(intervalo){
  puntos <- simula_unif(2,2,intervalo)
  a <- (puntos[2,2]-puntos[2,1])/(puntos[1,2]-puntos[1,1])
  b <- puntos[2,1] - a * puntos[1,1]
  return(c(a,b))
}

f <- functon(x,y){
  coefs <- simula_recta(c(-15,15))
  a <- coefs[1]
  b <- coefs[2]
  return(y - a*x -b > 0)
}
