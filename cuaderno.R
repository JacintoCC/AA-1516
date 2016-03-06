valores <- rep(1:12,4)
nombres <- rep(c("As","Dos","Tres","Cuatro","Cinco", "Seis", "Siete", "Ocho","Nueve","Sota","Caballo","Rey"),4)
palos <- c(rep("Oros",12),rep("Espadas",12),rep("Bastos",12),rep("Copas",12))
baraja <- data.frame(valores,nombres,palos)


barajar <- function(){
  assign("baraja",baraja[sample(dim(baraja)[1]),],1)
}


sacar_carta <- function(){
  carta <- baraja[1,]
  assign("baraja",baraja[-1,],1)
  carta
}
