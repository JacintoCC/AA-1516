# Generación y visualización de datos

# Establecimiento de la semilla para reproducir los datos del informe
set.seed(314159)
X11()

# Función para realizar paradas
stp <- function(){
  cat("Pulse una tecla para continuar", "\n")
  t<- invisible(readLines("stdin", n=1))
}

# Función para simular N datos uniformes en dim dimensiones
simula_unif <- function(N, dim, rango){
  # Tomamos N*dim muestras de una uniforme de rango dado
  lista <- matrix(runif(N*dim, min =  rango[1], max = rango[2]), N, dim)
  return(lista)
}

# Función para simular N datos de una distribución normal en dim dimensiones
simula_gauss <- function(N, dim, sigma){
  #Tomamos N*dim elementos de una normal, tomando la desviación estándar del vector sigma
  lista <- matrix(rnorm(N*dim, sd = sigma), N, dim, byrow=TRUE)
  return(lista)
}

# Generamos los datos
lista_unif <- simula_unif(50, 2, c(-50, 50))
lista_gauss <- simula_gauss(50, 2, c(5,7))


# Representación de los datos
plot(lista_unif[,1], lista_unif[,2], xlab="Eje X", ylab="Eje Y",
     main="Muestra N=50 distribución uniforme" )
stp()
plot(lista_gauss[,1],lista_gauss[,2], xlab="Eje X", ylab="Eje Y",
     main="Muestra N=50 distribución normal")
stp()

# Función para generar una recta aleatoria que pase por dos puntos en un determinado intervalo.
simula_recta <- function(intervalo){
  puntos <- simula_unif(2,2,intervalo)
  a <- (puntos[2,2]-puntos[1,2])/(puntos[2,1]-puntos[1,1])
  b <- puntos[1,2] - a * puntos[1,1]

  f <- function(x,y){
    return(y - a*x -b)
  }
  return(f)
}

# Generación de una recta aleatoria en [-50,50]. Etiquetado de la muestra uniforme con respecto a esta recta.
intervalo <- c(-50, 50)
f <- simula_recta(intervalo)
etiqueta <- apply(lista_unif, 1, function(X) sign(f(X[1],X[2])))
stp()

# Función para dibujar una función en un determinado intervalo.
draw_function <- function(interval_x, interval_y, f, levels = 0, col = 1, add = FALSE){
  x <- seq(interval_x[1],interval_x[2],length=1000)
  y <- seq(interval_y[1],interval_y[2],length=1000)
  z <- outer(x,y, f) # Matriz con los resultados de hacer f(x,y)

  # Levels = 0 porque queremos pintar f(x,y)=0
  contour(x,y,z, levels=0, col = col, add = add)
}

# Dibujamos la recta aleatoria generada y los puntos con un color que refleje la etiqueta.
draw_function(c(-50,50), c(-50,50), f)
title(main="a*x+b", ylab = "lista_unif[,2]", xlab = "lista_unif[,1]")
points(lista_unif[,1], lista_unif[,2], col = (etiqueta+3))
stp()

# Definición de las funciones cuadráticas y representación gráfica
f_1 <- function(x,y){
  return((x - 10)^2 + (y - 20)^2 - 400)
}

etiqueta_1 <- apply(lista_unif, 1, function(X) sign(f_1(X[1],X[2])))

draw_function(c(-50,50), c(-50,50), f_1)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_1+3))
title(main=expression((x - 10)^2 + (y - 20)^2 - 400))
stp()


f_2 <- function(x,y){
  return(0.5*(x + 10)^2 + (y - 20)^2 - 400)
}

etiqueta_2 <- apply(lista_unif, 1, function(X) sign(f_2(X[1],X[2])))

draw_function(c(-50,50), c(-50,50),f_2)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_2+3))
title(main=expression(0.5*(x + 10)^2 + (y - 20)^2 - 400))
stp()


f_3 <- function(x,y){
  return(0.5*(x - 10)^2 - (y + 20)^2 - 400)
}

etiqueta_3 <- apply(lista_unif, 1, function(X) sign(f_3(X[1],X[2])))

draw_function(c(-50,50), c(-50,50), f_3)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_3+3))
title(main=expression(0.5*(x - 10)^2 - (y + 20)^2 - 400))
stp()

f_4 <- function(x,y){
  return(y - 20*x^2- 5*x + 3)
}

etiqueta_4 <- apply(lista_unif, 1, function(X) sign(f_4(X[1],X[2])))

draw_function(c(-50,50), c(-50,50), f_4)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_4+3))
title(main=expression(y = 20*x^2+ 5*x - 3))
stp()


# Función para modificar un porcentaje de un vector de etiquetas
modify_rnd_bool_subvector <- function(v, perc = 0.1){
  #Almacenamos una copia para poder saber qué puntos debemos cambiar en el original.
  mod_v <- v

  #Contamos el número de puntos que corresponde cambiar para hacerlo sólo si hay alguno
  length_change <- round(length(which(v == 1))*perc)
  if( length_change >= 1){
    to_change <- sample(which(v == 1), length_change )
    mod_v[to_change] <- -1
  }

  #Contamos el número de puntos que corresponde cambiar para hacerlo sólo si hay alguno
  length_change <- round(length(which(v == -1))*perc)
  if( length_change >= 1){
    to_change <- sample(which(v == -1), length_change )
    mod_v[to_change] <- 1
  }

  return(mod_v)
}

# Modificación de etiquetas y representación gráfica
etiqueta_mod <- modify_rnd_bool_subvector(etiqueta)

draw_function(c(-50,50), c(-50,50), f)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_mod+3))
title(main="Etiquetas modificadas para f")
stp()

etiqueta_mod_1 <- modify_rnd_bool_subvector(etiqueta_1)

draw_function(c(-50,50), c(-50,50), f_1)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_mod_1+3))
title(main="Etiquetas modificadas para f_1")
stp()


etiqueta_mod_2 <- modify_rnd_bool_subvector(etiqueta_2)

draw_function(c(-50,50), c(-50,50), f_2)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_mod_2+3))
title(main="Etiquetas modificadas para f_2")
stp()


etiqueta_mod_3 <- modify_rnd_bool_subvector(etiqueta_3)

draw_function(c(-50,50), c(-50,50), f_3)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_mod_3+3))
title(main="Etiquetas modificadas para f_4")
stp()


etiqueta_mod_4 <- modify_rnd_bool_subvector(etiqueta_4)

draw_function(c(-50,50), c(-50,50), f_4)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_mod_4+3))
title(main="Etiquetas modificadas para f_3")
stp()


      # AJUSTE DEL ALGORITMO PERCEPTRON

# Función para obtener una solución mediante el algoritmo PLA
ajusta_PLA <- function(datos, label, max_iter, vini, draw_iterations = FALSE){
  sol <- vini
  iter <- 0
  changed <- TRUE

  while ( iter < max_iter && changed){
    # Comprobaremos en cada vuelta si hemos hecho algún cambio
    changed <- FALSE
    for( inner_iter in 1:nrow(datos) ){
      # Añadimos coeficiente independiente
      x <- c(datos[inner_iter,],1)

      # Modificamos la solución si encontramos un punto mal clasificado.
      if( sign(crossprod(x,sol)) != label[inner_iter]){
        sol <- sol + label[inner_iter] * x
        changed <- TRUE
      }
    }
    # Dibujamos cada iteración del algoritmo, haciendo una pausa para visualizarlo
    if(draw_iterations){
      draw_function(c(-50,50), c(-50,50), function(x,y) y +sol[1]/sol[2]*x
                    +sol[3]/sol[2], col = 4)
      title(main=paste("Iteración ", iter,sep=" "))
      points(lista_unif[,1], lista_unif[,2], col = (label+3))
      Sys.sleep(0.5)
    }
    iter <- iter+1
  }

  # Normalizamos la solución.
  sol <- sol/sol[length(sol)-1]

  # Devolvemos el hiperplano obtenido y el número de iteraciones realizadas
  return( list( hiperplane = sol, iterations = iter))
}

# Ejemplo de la ejecución del PLA
sol_pla <- ajusta_PLA(lista_unif,etiqueta, 200,rep(0,3))$hiperplane
draw_function(c(-50,50), c(-50,50), function(x,y) y+sol_pla[1]*x+sol_pla[3], col = 4)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta+3))
draw_function(c(-50,50), c(-50,50), f, col = 3, add = TRUE)
title(main="Ejecución PLA")
stp()

# Ejecuciones del algoritmo PLA para hallar la media de iteraciones
iterations <- numeric(10)
iterations[1] <- ajusta_PLA(lista_unif,etiqueta, 10000,rep(0,3))$iterations

for(i in 2:10){
  v_ini <- runif(3)
  iterations[i] <- ajusta_PLA(lista_unif,etiqueta, 10000, v_ini)$iterations
}

print(c("Media de iteraciones ", mean(iterations)))
stp()

# Función para contar el número de errores de una función con respecto a las etiquetas originales
count_errors <- function(f, datos, label){
  #Conteo de errores
  signs <- apply(datos, 1, function(x) return(sign(f(c(x,1)))))
  return( sum(signs != label) )
}

# Función para convertir un hiperplano en una función
hiperplane_to_function <- function( vec ){
  f <- function(x){
    return( crossprod(vec,x) )
  }
  return(f)
}

# Muestra de la ejecución y el número de errores para distintos números de ejecuciones
sol <- ajusta_PLA(lista_unif,etiqueta_mod, 10,rep(0,3))$hiperplane
cat("Errores con 10 iteraciones", count_errors(hiperplane_to_function(sol),
                                               lista_unif, etiqueta_mod ), "\n")
draw_function(c(-50,50), c(-50,50), function(x,y) y+sol[1]*x+sol[3], col = 4)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_mod+3))
title(main="Ejecución con 10 iteraciones")
stp()

sol <- ajusta_PLA(lista_unif,etiqueta_mod, 100,rep(0,3))$hiperplane
cat("Errores con 100 iteraciones",count_errors(hiperplane_to_function(sol),
                                               lista_unif, etiqueta_mod ), "\n")
draw_function(c(-50,50), c(-50,50), function(x,y) y+sol[1]*x+sol[3], col = 4)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_mod+3))
title(main="Ejecución con 100 iteraciones")
stp()

sol <- ajusta_PLA(lista_unif,etiqueta_mod, 1000,rep(0,3))$hiperplane
cat("Errores con 1000 iteraciones",count_errors(hiperplane_to_function(sol),
                                                lista_unif, etiqueta_mod ), "\n")
draw_function(c(-50,50), c(-50,50), function(x,y) y+sol[1]*x+sol[3], col = 4)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_mod+3))
title(main="Ejecución con 1000 iteraciones")
stp()

# Muestra de la ejecución y el número de errores para distintos números de ejecuciones
#   con datos no linealmente separables y cuadráticos
sol <- ajusta_PLA(lista_unif,etiqueta_mod_1, 10,rep(0,3))$hiperplane
cat("Errores con 10 iteraciones", count_errors(hiperplane_to_function(sol),
                                            lista_unif, etiqueta_mod_1 ), "\n")
draw_function(c(-50,50), c(-50,50), function(x,y) y+sol[1]*x+sol[3], col = 4)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_mod_1+3))
title(main="Ejecución con 10 iteraciones")
stp()

sol <- ajusta_PLA(lista_unif,etiqueta_mod_1, 100,rep(0,3))$hiperplane
cat("Errores con 100 iteraciones", count_errors(hiperplane_to_function(sol),
                                            lista_unif, etiqueta_mod_1 ), "\n")
draw_function(c(-50,50), c(-50,50), function(x,y) y+sol[1]*x+sol[3], col = 4)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_mod_1+3))
title(main="Ejecución con 100 iteraciones")
stp()

sol <- ajusta_PLA(lista_unif,etiqueta_mod_1, 1000,rep(0,3))$hiperplane
cat("Errores con 1000 iteraciones", count_errors(hiperplane_to_function(sol),
                                            lista_unif, etiqueta_mod_1 ), "\n")
draw_function(c(-50,50), c(-50,50), function(x,y) y+sol[1]*x+sol[3], col = 4)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_mod_1+3))
title(main="Ejecución con 1000 iteraciones")
stp()


# Mostramos la ejecución del algoritmo tanto en la versión que converge como la que no
ajusta_PLA(lista_unif,etiqueta, 1000,rep(0,3), TRUE)$hiperplane
stp()
ajusta_PLA(lista_unif,etiqueta_mod, 1000,rep(0,3), TRUE)$hiperplane
stp()

# Modificación del algoritmo PLA para quedarnos con la mejor solución encontrada
ajusta_PLA_MOD <- function(datos, label, max_iter, vini, draw_iterations = FALSE){
  sol <- vini
  iter <- 0
  changed <- TRUE
  current_sol <- sol

  while ( iter < max_iter && changed){
    # Comprobaremos en cada vuelta si hemos hecho algún cambio
    current_errors <- count_errors(hiperplane_to_function(sol), datos, label )
    changed <- FALSE

    for( inner_iter in 1:nrow(datos) ){
      # Añadimos coeficiente independiente
      x <- c(datos[inner_iter,],1)

      if( sign(crossprod(x,current_sol)) != label[inner_iter]){
        current_sol <- current_sol + label[inner_iter] * x
        changed <- TRUE
      }
    }

    if(draw_iterations){
      # Dibujamos tanto la solución actual como la mejor encontrada
      draw_function(c(-50,50), c(-50,50),
                    function(x,y) y*sol[2] +sol[1]*x +sol[3], col = 4)
      draw_function(c(-50,50), c(-50,50),
                    function(x,y) y*current_sol[2] +current_sol[1]*x +current_sol[3],
                    col = 5, add=TRUE)
      points(lista_unif[,1], lista_unif[,2], col = (label+3))
      title(main=paste("Iteración ", iter,sep=" "))
      Sys.sleep(0.5)
    }

    if( current_errors >= count_errors(hiperplane_to_function(current_sol),
                                       datos, label )){
      sol <- current_sol
    }

    iter <- iter+1
  }
  sol <- sol/sol[length(sol)-1]

  return( list( hiperplane = sol, iterations = iter))
}

# Mostramos la ejecución del algoritmo modificado con los datos que no convergen
ajusta_PLA_MOD(lista_unif,etiqueta_mod, 1000,rep(0,3), TRUE)
stp()

# Muestra de cómo se comporta para datos que no son linealmente separables
sol_cuadratic_1 <- ajusta_PLA_MOD(lista_unif, etiqueta_1, 1000, rep(0,3))$hiperplane
cat("Errores f_1", count_errors(hiperplane_to_function(sol_cuadratic_1),
                                lista_unif, etiqueta_1 ), "\n")
draw_function(c(-50,50), c(-50,50), function(x,y)
  y +sol_cuadratic_1[1]*x +sol_cuadratic_1[3], col = 4)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_1+3))
title(main="Ajuste lineal para f1")
stp()

sol_cuadratic_2 <- ajusta_PLA_MOD(lista_unif, etiqueta_2, 1000, rep(0,3))$hiperplane
cat("Errores f_2", count_errors(hiperplane_to_function(sol_cuadratic_2),
                                lista_unif, etiqueta_2 ), "\n")
draw_function(c(-50,50), c(-50,50), function(x,y)
  y +sol_cuadratic_2[1]*x +sol_cuadratic_2[3], col = 4)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_2+3))
title(main="Ajuste lineal para f2")
stp()

sol_cuadratic_3 <- ajusta_PLA_MOD(lista_unif, etiqueta_3, 1000, rep(0,3))$hiperplane
cat("Errores f_3", count_errors(hiperplane_to_function(sol_cuadratic_3),
                                lista_unif, etiqueta_3 ), "\n")
draw_function(c(-50,50), c(-50,50), function(x,y)
  y +sol_cuadratic_3[1]*x +sol_cuadratic_3[3], col = 4)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_3+3))
title(main="Ajuste lineal para f3")
stp()

sol_cuadratic_4 <- ajusta_PLA_MOD(lista_unif, etiqueta_4, 1000, rep(0,3))$hiperplane
cat("Errores f_4", count_errors(hiperplane_to_function(sol_cuadratic_4),
                                lista_unif, etiqueta_4 ), "\n")
draw_function(c(-50,50), c(-50,50), function(x,y)
  y +sol_cuadratic_4[1]*x +sol_cuadratic_4[3], col = 4)
points(lista_unif[,1], lista_unif[,2], col = (etiqueta_4+3))
title(main="Ajuste lineal para f4")
stp()


# Transformación de los puntos para poder aplicar PLA
lista_unif_transf <- matrix(apply(lista_unif,1,
   function(X)c((X[1]-10)^2,(X[2]-20)^2)),50,2,byrow=T)
plot(lista_unif_transf[,1], lista_unif_transf[,2], col = (etiqueta_1+3))
title(main="Transformación de los datos")
stp()

# Aplicamos PLA y deshacemos la transformación
sol_transf <- sol_transf <- ajusta_PLA(lista_unif_transf, etiqueta_1, 4000, rep(0,3))$hiperplane
cat("Errores transformado", count_errors(hiperplane_to_function(sol_transf),
                                lista_unif_transf, etiqueta_1 ), "\n")
plot(lista_unif_transf[,1], lista_unif_transf[,2], col = (etiqueta_1+3))
draw_function(c(-10,1000), c(-10,1000), function(x,y)
  y +sol_transf[1]*x +sol_transf[3], col = 4,add=TRUE)
title(main="PLA en datos transformados")
stp()

plot(lista_unif[,1], lista_unif[,2], col = (etiqueta_1+3))
draw_function(c(-50,50), c(-50,50), function(x,y)
    (y-20)^2 +sol_transf[1]*(x-10)^2 +sol_transf[3], col = 4,add=TRUE)
draw_function(c(-50,50), c(-50,50), f_1, col = 3,add=TRUE)
title(main="PLA deshecha la transformación")
stp()

  # REGRESIÓN LINEAL

# Lectura de datos y dotación de forma
datos <- scan("datos/zip.train",sep=" ")
datos <- matrix(datos,ncol=257,byrow=T)
datos <- datos[-(which(datos[,1] != 1 & datos[,1]!=5)),]

number <- datos[,1]
pixels <- array(t(datos[,2:257]), dim=c(16,16,nrow(datos)))

# Muestra de las imágenes leídas
for(i in 1:dim(pixels)[3]){
  image(1:16,1:16,pixels[,16:1,i],col = gray(32:0/32))
  Sys.sleep(0.5)
}
stp()


# Función para calcular la simetría vertical de una matriz
simetria_vertical <- function(M){
  -sum(apply(M, 1, function(x) sum(abs(x-x[length(x):1]))))
}

# Cálculo de la intensidad y la simetría vertical
means <- apply(pixels, 3, mean)
sim_vertical <- apply(pixels,3,simetria_vertical)


# Representación de la intensidad y la simetría
number_colors <- numeric(length(number))
number_colors[which(number==1)] <- 4
number_colors[which(number==5)] <- 3
data_list <- list('number'=number, 'pixels'=pixels, 'mean'=means,
                  'v_symmetry'=sim_vertical, 'colors'=number_colors )
plot(data_list$mean, data_list$v_symmetry, col = data_list$colors )
title(main="Intensidad y simetría")
stp()

# Función para calcular la pseudoinversa de una matriz
pseudoinv <- function(X){
  desc <- svd(X)
  d <- round(desc$d, digits=5)
  d[abs(d)>0] <- 1/d[abs(d)>0]
  pseudo_inv <- desc$v %*% diag(d) %*% t(desc$u)
  return(pseudo_inv)
}

# Función para calcular la regresión lineal de unos datos y unas etiquetas.
regress_lin <- function(datos, label){
  pseudo_inv <- pseudoinv(t(datos)%*%datos)
  return(pseudo_inv%*%t(datos)%*%label)
}

# Muestra de la regresión lineal de la simetría con respecto a la intensidad
plot(data_list$mean, data_list$v_symmetry, col = data_list$colors )
coefs <- regress_lin(cbind(means,rep(1,length(means))), sim_vertical)
abline(coefs[2],coefs[1])
title(main="Regresión")
stp()

# Muestra de la recta que clasifica los datos en función del número que representa
label <- numeric(length(number))
label[which(number==1)] <- -1
label[which(number==5)] <- 1
coefs2 <- regress_lin(cbind(means,sim_vertical,rep(1,length(means))), label)
abline(-coefs2[3]/coefs2[2],-coefs2[1]/coefs2[2],col=2)
stp()


  # Regresión en clasificación

# Generación de muestras uniformes aleatorias, regresión y cálculo de Ein y Eout
N <- 100
num_repeticiones <- 1000
E_in_vector <- numeric(num_repeticiones)
E_out_vector <- numeric(num_repeticiones)

for( i in 1:num_repeticiones){
  # Generación de una recta aleatoria
  f <- simula_recta(c(-10,10))

  # Generación de una muestra y etiquetado
  muestra_in <- simula_unif(N, 2, c(-10, 10))
  etiqueta_in <- apply(muestra_in, 1, function(X) sign(f(X[1],X[2])))

  # Cálculo de la regresión
  coefs_rl <- regress_lin(cbind(muestra_in[,1],muestra_in[,2], rep(1,N)),matrix(etiqueta_in,N,1))
  g <- function(x,y){coefs_rl[2]*y + coefs_rl[1]*x + coefs_rl[3]}

  # Etiquetado según la regresión
  rl_etiqueta_in <- apply(muestra_in, 1, function(X) sign(g(X[1],X[2])))

  # Cálculo del error en la muestra
  E_in <-sum(etiqueta_in != rl_etiqueta_in)/N*100
  E_in_vector[i] <- E_in

  # Generación de una muestra externa a la usada para calcular la regresión
  muestra_out <- simula_unif(N, 2, c(-10, 10))
  etiqueta_out <- apply(muestra_out, 1, function(X) sign(f(X[1],X[2])))

  #Etiquetado según la regresión
  rl_etiqueta_out <- apply(muestra_out, 1, function(X) sign(g(X[1],X[2])))

  # Cálculo del error fuera de la muestra usada para la regresión
  E_out <- sum(etiqueta_out != rl_etiqueta_out)/N*100
  E_out_vector[i] <- E_out
}

cat( "Ein. Media = ", mean(E_in_vector), "\n Max = ", max(E_in_vector),
       "\n Min = ", min(E_in_vector), "\n Desv. Típica = ", sd(E_in_vector), "\n")

cat( "Eout. Media = ", mean(E_out_vector), "\n Max = ", max(E_out_vector),
       "\n Min = ", min(E_out_vector), "\n Desv. Típica = ", sd(E_out_vector), "\n")
stp()


# Ejecución conjunta de la regresión y culminación con PLA
N <- 10
num_repeticiones <- 1000
iterations_vector <- numeric(num_repeticiones)

for( i in 1:num_repeticiones){
  # Generación de una recta aleatoria
  f <- simula_recta(c(-10,10))

  # Generación de una muestra y etiquetado
  muestra_in <- simula_unif(N, 2, c(-10, 10))
  etiqueta_in <- apply(muestra_in, 1, function(X) sign(f(X[1],X[2])))

  # Cálculo de la regresión
  coefs_rl <- regress_lin(cbind(muestra_in[,1],muestra_in[,2], rep(1,N)),
                          matrix(etiqueta_in,N,1))

  #Ejecución del algoritmo PLA
  iterations[i] <- ajusta_PLA(muestra_in,etiqueta_in, 10000,coefs_rl)$iterations
}

cat( "Iterations. Media = ", mean(iterations), "\n Max = ", max(iterations),
       "\n Min = ", min(iterations), "\n Desv. Típica = ", sd(iterations), "\n")
stp()

# Ejecución de la regresión para funciones no lineales
N <- 1000

#Definición de la función
f <- function(x,y){
  x^2+y^2-25
}

# Generación de una muestra, etiquetado y representación gráfica para mostrar la forma de los datos
muestra_8 <- simula_unif(N, 2, c(-10, 10))
etiquetas_8 <- modify_rnd_bool_subvector(apply(muestra_8, 1, function(X) sign(f(X[1],X[2]))))
plot(muestra_8[,1],muestra_8[,2],col=etiquetas_8+3)
title(main="Representación datos")
stp()


for( i in 1:num_repeticiones){
  # Generación de una muestra y etiquetado
  muestra_8 <- simula_unif(N, 2, c(-10, 10))
  etiquetas_8 <- modify_rnd_bool_subvector(apply(muestra_8, 1, function(X) sign(f(X[1],X[2]))))

  # Cálculo de la regresión
  coefs_rl <- regress_lin(cbind(muestra_8[,1],muestra_8[,2], rep(1,N)),matrix(etiquetas_8,N,1))
  g <- function(x,y){coefs_rl[2,1]*y + coefs_rl[1,1]*x + coefs_rl[3,1]}

  # Etiquetado según la regresión
  rl_etiqueta_8 <- apply(muestra_8, 1, function(X) sign(g(X[1],X[2])))

  # Cálculo del error en la muestra
  E_in <-sum(etiquetas_8 != rl_etiqueta_8)/N*100
  E_in_vector[i] <- E_in
}
cat( "Ein. Media = ", mean(E_in_vector), "\n Max = ", max(E_in_vector),
       "\n Min = ", min(E_in_vector), "\n Desv. Típica = ", sd(E_in_vector), "\n")
stp()

# Regresión lineal con funciones cuadráticas
for( i in 1:num_repeticiones){
  # Generación de una muestra y etiquetado, modificando un 10%
  muestra_8 <- simula_unif(N, 2, c(-10, 10))
  etiquetas_8 <- modify_rnd_bool_subvector(apply(muestra_8, 1, function(X) sign(f(X[1],X[2]))))

  # Cálculo de la regresión
  coefs_rl <- regress_lin(cbind(muestra_8[,1],muestra_8[,2],muestra_8[,1]*muestra_8[,2],
                                muestra_8[,1]**2,muestra_8[,2]**2,rep(1,N)),
                          matrix(etiquetas_8,N,1))
  g <- function(x,y){coefs_rl[1]*x + coefs_rl[2]*y +coefs_rl[3]*x*y +
                      coefs_rl[4]*x^2 + coefs_rl[5]*y^2 + coefs_rl[6]}

  # Etiquetado según la regresión
  rl_etiqueta_in <- apply(muestra_8, 1, function(X) sign(g(X[1],X[2])))

  # Cálculo del error en la muestra
  E_in <-sum(etiquetas_8 != rl_etiqueta_in)/N*100
  E_in_vector[i] <- E_in

  # Generación de una muestra externa a la usada para calcular la regresión
  muestra_8_out <- simula_unif(N, 2, c(-10, 10))
  etiqueta_8_out <- apply(muestra_8_out, 1, function(X) sign(f(X[1],X[2])))

  # Etiquetado según la regresión
  rl_etiqueta_8_out <- apply(muestra_8_out, 1, function(X) sign(g(X[1],X[2])))

  # Cálculo del error fuera de la muestra usada para la regresión
  E_out <- sum(etiqueta_8_out != rl_etiqueta_8_out)/N*100
  E_out_vector[i] <- E_out
}
draw_function(c(-10,10), c(-10,10), f, col = 3)
draw_function(c(-10,10), c(-10,10), g, col = 1, add = TRUE)
points(muestra_8[,1],muestra_8[,2],col=etiquetas_8+3)
title(main="Regresión lineal con funciones cuadráticas")
cat( "Ein. Media = ", mean(E_in_vector), "\n Max = ", max(E_in_vector),
       "\n Min = ", min(E_in_vector), "\n Desv. Típica = ", sd(E_in_vector), "\n")
cat( "Eout. Media = ", mean(E_out_vector), "\n Max = ", max(E_out_vector),
       "\n Min = ", min(E_out_vector), "\n Desv. Típica = ", sd(E_out_vector), "\n")
stp()
