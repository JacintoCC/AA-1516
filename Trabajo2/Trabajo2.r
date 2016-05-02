
set.seed(314159)
setwd("~/Documentos/Aprendizaje Automático/AA-1516/Trabajo2")

# Realiza una pausa durante la ejecución del código
pause <- function(){
  cat("Pulse cualquier tecla")
  s <- scan()
}

# Función para pintar una función declarada de forma implícita en un
# cierto intervalo
# @param interval_x Extremo inicial del intervalo
# @param interval_y Extremo final del intervalo
# @param f Función declarada de forma implícita a representar
# @param levels Niveles a pintar de la función. Por defecto, f = 0.
# @param col Color en el que se pinta la función. Por defecto, negro.
# @param add  Valor lógico que determina si la gráfica se añade al plot
#             actual
draw_function <- function(interval_x, interval_y, f, levels = 0, 
                          col = 1, add = FALSE){
  x <- seq(interval_x[1],interval_x[2],length=1000)
  y <- seq(interval_y[1],interval_y[2],length=1000)
  z <- outer(x,y, f) # Matriz con los resultados de hacer f(x,y)
  
  # Levels = 0 porque queremos pintar f(x,y)=0
  contour(x,y,z, levels=0, col = col, add = add, drawlabels = FALSE,
          ylab="", xlab="")
}

# Obtención de una función que recibe un vector como entrada
# a partir de un hiperplano y devuelve el producto escalar con los
# coeficientes del hiperplano.
# @param vec Vector de coeficientes del hiperplano
# @param change_signs Valor lógico que determina si debemos cambiar
#                     el signo de los coeficientes del vector
# @return función que realiza el producto escalar de un vector con 
#         los coeficientes del hiperplano
hypplane_to_func <- function( vec, change_signs = FALSE ){
  if(change_signs){
    vec[2:length(vec)] <- -vec[2:length(vec)]
  }
  f <- function(x){
    # @return( vec[1]*x[1]+vec[2]*x[2]+vec[3]*x[3] )
    return( x %*% vec )
  }
  return(f)
}

# Obtención de una función que recibe coordenadas 2D como entrada
# y devuelve el producto escalar con los coeficientes de una recta.
# @param vec Vector de coeficientes de la recta
# @param change_signs Valor lógico que determina si debemos cambiar
#                     el signo de los coeficientes del vector
# @return función que realiza el producto escalar de un vector con 
#         los coeficientes del hiperplano
vec3D_to_func2D <- function( vec, change_signs = FALSE ){
  if(change_signs){
    vec[c(1,3)] <- -vec[c(1,3)]
  }
  f <- function(x,y){
    return( vec[1]*x+vec[2]*y+vec[3] )
  }
  return(f)
}

# Método del gradiente descendente.
# @param fun Función sobre la que aplicar método del gradiente descendente
# @param v_ini Vector inicial
# @param lr Tasa de aprendizaje
# @param max_iterations Número máximo de iteraciones a realizar
# @param dif  Diferencia entre dos iteraciones con la que determinamos si
#             el método ha convergido
# @param draw_iterations  Valor lógico que determina si dibujar el valor
#                         de las iteraciones
# @param print_iterations Valor lógico que determina si imprimir el valor
#                         de las iteraciones
# @return min Mínimo alcanzado
# @return iterations Número de iteraciones realizadas
gradientDescent <- function(fun, v_ini, lr, max_iterations, 
                            dif = 0, draw_iterations=FALSE, 
                            print_iterations=FALSE){
  # Creación del vector donde guardaremos los valores obtenidos
  values <- vector(mode = "numeric", length = max_iterations+1)
  
  w <- v_ini   # Solución inicial
  aux <- fun(w)
  values[1] <- aux$value
  grad <- aux$derivative_f
  
  # Comenzamos el bucle
  iter <- 2
  stopped <- FALSE 
  
  while(iter <= max_iterations+1 && !stopped){
    # Actualización de w
    w <- w - lr * grad
    
    # Evaluamos la función
    aux <- fun(w)
    values[iter]<- aux$value
    grad <-  aux$derivative_f
    
    # Comprobamos si la diferencia entre dos iteraciones es menor a un umbral
    if( abs(values[iter-1] - values[iter]) < dif){
      stopped <- TRUE
    }
    # Imprimimos las iteraciones si se ha indicado así.
    if(print_iterations){
      cat("Iteration ",iter-1,"\n")
      print(w)
      print(values[iter])
    }
    iter <- iter +1
  }
  # Dibujamos las iteraciones 
  if(draw_iterations){
    plot(1:iter,values[1:iter], xlab=" ", ylab=" ")
  }
  
  return(list('min' = w, 'iterations' = iter-2))
}

# EJERCICIO 1

# Apartado a1)

# Función E apartado 1a)
# @param w Vector en 2D donde evaluamos la función
# @return value Valor de la función en w
# @return derivative_f Gradiente de f en w
E_1a <- function(w){
  value <- (w[1]*exp(w[2]) - 2*w[2]*exp(-w[1]))^2
  derivative_f<-c(2*(w[1]*exp(w[2])-2*w[2]*exp(-w[1]))*
                    (exp(w[2])+ 2*w[2]*exp(-w[1])),
                  2*(w[1]*exp(w[2])-2*w[2]*exp(-w[1]))*
                    (w[1]*exp(w[2])- 2*exp(-w[1])))
  return(list('value' = value, 'derivative_f'= derivative_f))
}

results_1a <- gradientDescent(E_1a, v_ini = c(0,0.1), 
                              lr = 0.1, max_iterations = 15)
print(results_1a$iterations)
print(results_1a$min)
print(E_1a(results_1a$min)$value)
pause()

# Apartado a2)

print(gradientDescent(E_1a, v_ini = c(0,0.1), lr = 0.1, 
                      max_iterations = 10, print_iterations = TRUE))
pause()

# Apartado b1)

# Función E apartado 1b)
# @param w Vector en 2D donde evaluamos la función
# @return value Valor de la función en w
# @return derivative_f Gradiente de f en w
# @return hessian_f Matriz hessiana de f en w
E_1b <- function(w){
  value <- w[1]^2 + 2*w[2]^2 + 2*sin(2*pi*w[1])*sin(2*pi*w[2])
  derivative_f <- c(2*w[1] + 4*pi*cos(2*pi*w[1])*sin(2*pi*w[2]),
                    4*w[2] + 4*pi*sin(2*pi*w[1])*cos(2*pi*w[2]))
  hessian_f <- matrix(c(2 - 8*pi^2*cos(2*pi*w[1])*sin(2*pi*w[2]),
                        8*pi^2*cos(2*pi*w[1])*cos(2*pi*w[2]),
                        8*pi^2*cos(2*pi*w[1])*cos(2*pi*w[2]),
                        4 - 8*pi^2*sin(2*pi*w[1])*sin(2*pi*w[2]))
                      ,2,2,TRUE)
  return(list('value' = value, 'derivative_f'= derivative_f,
              'hessian_f' = hessian_f))
}

print(gradientDescent(E_1b, v_ini = c(1,1), lr = 0.01, 
                      max_iterations = 30, draw_iterations = TRUE))
title(main="Grad. Desc. LR = 0.01", 
      ylab = "Valor función", xlab = "Iteraciones")
pause()

print(gradientDescent(E_1b, v_ini = c(1,1), lr = 0.1, 
                      max_iterations = 50, draw_iterations = TRUE))
title(main="Grad. Desc. LR = 0.1", 
      ylab = "Valor función", xlab = "Iteraciones")
pause()

# Apartado b2)

vectors_ini <- matrix(c(0.1,0.1,1,1,-0.5,-0.5,-1,-1),ncol=2,byrow=TRUE)
vectors_min <- t(apply(vectors_ini,1, 
                       function(x) gradientDescent(E_1b, v_ini =x , 
                                                   lr = 0.01, max_iterations= 100)$min))
print(vectors_min)
t(apply(vectors_min,1, function(x) E_1b(x)$value))
pause()

# EJERCICIO 2

# Método de coordenada descendente.
# @param fun Función sobre la que aplicar método de la coordenada descendente
# @param v_ini Vector inicial
# @param lr Tasa de aprendizaje
# @param max_iterations Número máximo de iteraciones a realizar
# @param dif  Diferencia entre dos iteraciones con la que determinamos si
#             el método ha convergido
# @param draw_iterations  Valor lógico que determina si dibujar el valor
#                         de las iteraciones
# @param print_iterations Valor lógico que determina si imprimir el valor
#                         de las iteraciones
# @return min Mínimo alcanzado
# @return iterations Número de iteraciones realizadas
coordinateDescent <- function(fun, v_ini, lr, max_iterations, 
                              dif = 0, draw_iterations=FALSE,
                              print_iterations=FALSE){
  
  # Creación del vector donde guardaremos los valores obtenidos
  values <- vector(mode = "numeric", length = max_iterations+1)
  
  w <- v_ini   # Solución inicial
  aux <- fun(w)
  values[1] <- aux$value
  grad_u <- aux$derivative_f[1]
  
  # Comenzamos el bucle
  iter <- 2
  stopped <- FALSE 
  
  while(iter <= max_iterations+1 && !stopped){
    # Primer paso. Movimiento en u
    w[1] <- w[1] - lr * grad_u
    
    # Segundo paso. Movimiento en v
    grad_v <- fun(w)$derivative_f[2]
    w[2] <- w[2] - lr * grad_v
    
    # Evaluamos la función
    aux <- fun(w)
    values[iter]<- aux$value
    grad_u <-  aux$derivative_f[1]
    
    # Comprobamos si la diferencia entre dos iteraciones es menor 
    # a un umbral
    if( abs(values[iter-1] - values[iter]) < dif){
      stopped <- TRUE
    }
    
    # Mostramos el punto y su valor en la función en cada iteración
    if(print_iterations){
      cat("Iteration ",iter-1,"\n")
      print(w)
      print(values[iter])
    }
    
    iter <- iter +1
  }
  
  # Dibujamos las iteraciones si se ha indicado
  if(draw_iterations){
    plot(1:iter,values[1:iter], ylab=" ", xlab=" ")
  }
  
  return(list('min' = w, 'iterations' = iter-2))
}

# Apartado 2a)

min_2a <- coordinateDescent(E_1a, v_ini = c(0,0.1), lr = 0.1, 
                            max_iterations = 15, draw_iterations = TRUE)$min
title(main="Coord. Desc. LR = 0.1", 
      ylab = "Valor función", xlab = "Iteraciones")
print(E_1a(min_2a)$value)

# EJERCICIO 3

# Método de Newton.
# @param fun Función sobre la que aplicar método de Newton
# @param v_ini Vector inicial
# @param lr Tasa de aprendizaje
# @param max_iterations Número máximo de iteraciones a realizar
# @param dif  Diferencia entre dos iteraciones con la que determinamos si
#             el método ha convergido
# @param draw_iterations  Valor lógico que determina si dibujar el valor
#                         de las iteraciones
# @param print_iterations Valor lógico que determina si imprimir el valor
#                         de las iteraciones
# @return min Mínimo alcanzado
# @return iterations Número de iteraciones realizadas
newtonMethod <- function(fun, v_ini, lr, max_iterations, dif = 0,
                         draw_iterations=FALSE,
                         print_iterations=FALSE){
  # Creación del vector donde guardaremos los valores obtenidos
  values <- vector(mode = "numeric", length = max_iterations+1)
  
  w <- v_ini   # Solución inicial
  aux <- fun(w)
  values[1] <- aux$value
  increment <- solve(aux$hessian)%*%aux$derivative_f
  
  # Comenzamos el bucle
  iter <- 2
  stopped <- FALSE 
  
  while(iter <= max_iterations+1 && !stopped){
    # Actualización de w
    w <- w - lr * increment
    
    # Evaluamos la función
    aux <- fun(w)
    values[iter]<- aux$value
    increment <- solve(aux$hessian)%*%aux$derivative_f
    
    # Comprobamos si la diferencia entre dos iteraciones es menor 
    # a un umbral
    if( abs(values[iter-1] - values[iter]) < dif){
      stopped <- TRUE
    }
    # Mostramos el punto y su valor en la función en cada iteración
    if(print_iterations){
      cat("Iteration ",iter-1,"\n")
      print(w)
      print(values[iter])
    }
    iter <- iter +1
  }
  
  # Dibujamos las iteraciones si se ha indicado
  if(draw_iterations){
    plot(1:iter,values[1:iter], ylab=" ", xlab=" ")
  }
  
  return(list('min' = w, 'iterations' = iter-2))
}

# Apartado a)

print(newtonMethod(E_1b, v_ini = c(1,1), lr = 0.01, 
                   max_iterations =50, draw_iterations = TRUE))
title(main = "Método de Newton", ylab = "Valor de la función",
      xlab = "Iteraciones")
pause()

print(newtonMethod(E_1b, v_ini = c(1,1), lr = 0.1, 
                   max_iterations = 50, draw_iterations = TRUE))
title(main = "Método de Newton", ylab = "Valor de la función",
      xlab = "Iteraciones")
pause()

vectors_ini <- matrix(c(0.1,0.1,1,1,-0.5,-0.5,-1,-1),ncol=2,byrow=TRUE)
vectors_min <- t(apply(vectors_ini,1, function(x) newtonMethod(E_1b,
                                                               v_ini =x , lr = 0.01, max_iterations= 100)$min))
print(vectors_min)
t(apply(vectors_min,1, function(x) E_1b(x)$value))
pause()

# EJERCICIO 4

# Obtención de una muestra de datos de una distribución uniforme de 
# varias dimensiones
# @param N  Número de datos de la muestra
# @param dim  Dimensión de la muestra
# @param rango  Rango de las dimensiones donde tomar la muestra
# @return muestra Muestra generada
simula_unif <- function(N, dim, rango){
  # Tomamos N*dim muestras de una uniforme de rango dado
  muestra <- matrix(runif(N*dim, min =  rango[1], max = rango[2]), N, dim)
  return(muestra)
}

# Obtención de una recta aleatoria (función lineal en 2D)
# @param  intervalo Intervalo donde se generan dos puntos con los que se
#         calculan los parámetros de la recta
# @return f Recta
simula_recta <- function(intervalo){
  puntos <- simula_unif(2,2,intervalo)
  a <- (puntos[2,2]-puntos[1,2])/(puntos[2,1]-puntos[1,1])
  b <- puntos[1,2] - a * puntos[1,1]
  
  f <- vec3D_to_func2D(c(a,1,b))
  return(f)
}

# Apartado a)

N <- 100
recta_4a <- simula_recta(c(-1,1))
muestra_4a <- simula_unif(N, 2, c(-1, 1))
etiquetas_4a <- apply(muestra_4a, 1, 
                      function(X) sign(recta_4a(X[1],X[2])))

# Norma euclídea
# @param x Vector
# @return Norma euclídea del vector.
norm_v <- function(x) sqrt(sum(x^2))

# Método de gradiente descendiente estocástico
# @param datos Datos clasificados de una población
# @param valores  Etiquetas de los datos
# @param max_iter Número máximo de iteraciones
# @param vini Vector inicial
# @param lr Tasa de aprendizaje
# @param draw_iterations Valor lógico que determina si se dibuja cada
#         iteración
# @return hiperplane Hiperplano obtenido
# @return iterations Iteraciones realizadas del método
SGD <- function(datos, valores, max_iter, vini= c(0,0,0), 
                lr = 0.01, draw_iterations = FALSE){
  sol <- vini
  iter <- 0
  stopped <- FALSE
  
  while ( iter < max_iter && !stopped){
    permutation = sample(nrow(datos))
    prev_sol <- sol
    
    for( inner_iter in permutation ){
      # Añadimos coeficiente independiente
      x <- c(datos[inner_iter,],1)
      y <- valores[inner_iter]
      
      gr <- lr*y*x/(1+exp(y*(sol%*%x)))
      sol <- sol + gr
    }
    
    if(norm_v(sol - prev_sol) < 0.01)
      stopped = TRUE
    if(draw_iterations){
      draw_function(c(-1,1), c(-1,1), vec3D_to_func2D(sol), 
                    col = 4)
      title(main=paste("Iteración ", iter,sep=" "), ylab ="Eje Y",
            xlab = "Eje X")
      points(datos[,1], datos[,2], col = (valores+2))
      Sys.sleep(0.5)
    }
    
    iter <- iter+1
  }
  
  # Normalizamos la solución.
  sol <- sol/sol[length(sol)-1]
  
  # Devolvemos el hiperplano obtenido y el número 
  # de iteraciones realizadas
  return( list( hyperplane = sol, iterations = iter))
}

sol_4a <- SGD(muestra_4a, etiquetas_4a, 1000)
g_4a <- vec3D_to_func2D(sol_4a$hyperplane)
draw_function(c(-1,1),c(-1,1), g_4a)
points(muestra_4a[,1],muestra_4a[,2],col=etiquetas_4a+3)
print(sol_4a$iterations)
pause()

# Apartado b

muestra_4a_out <- simula_unif(N, 2, c(-1, 1))
etiquetas_4a_out <- apply(muestra_4a_out, 1, 
                          function(X) sign(recta_4a(X[1],X[2])))

# Etiquetado según la regresión
rl_etiqueta_4a_out <- apply(muestra_4a_out, 1, 
                            function(X) sign(g_4a(X[1],X[2])))

# Cálculo del error fuera de la muestra usada para la regresión
E_out <- sum(etiquetas_4a_out != rl_etiqueta_4a_out)/N*100
pause()

# Apartado c)

# Vector con los errores en cada iteración
E_out_4c <- vector(mode="numeric", length = 100)

for( i in 1:100){
  # Generación de los datos para cada iteración
  recta_4c <- simula_recta(c(-1,1))
  muestra_4c <- simula_unif(N, 2, c(-1, 1))
  etiquetas_4c <- apply(muestra_4c, 1, 
                        function(X) sign(recta_4c(X[1],X[2])))
  
  # Estimación de la función
  sol_4c <- SGD(muestra_4c, etiquetas_4c, 1000)$hyperplane
  g_4c <- vec3D_to_func2D(sol_4c)
  
  # Etiquetado según el signo de la recta
  muestra_4c_out <- simula_unif(N, 2, c(-1,1))
  etiquetas_4c_out <- apply(muestra_4c_out, 1, 
                            function(X) sign(recta_4c(X[1],X[2])))
  
  # Etiquetado según la regresión
  rl_etiquetas_4c_out <- apply(muestra_4c_out, 1, 
                               function(X) sign(g_4c(X[1],X[2])))
  
  # Cálculo del error fuera de la muestra usada para la regresión
  E_out_4c[i] <- sum(etiquetas_4c_out != rl_etiquetas_4c_out)/N*100
}

cat("Media E_out: ", mean(E_out_4c))
pause()

# EJERCICIO 5

datos_train <- scan("datos/zip.train",sep=" ")
datos_train <- matrix(datos_train, ncol=257, byrow=T)
datos_train <- datos_train[datos_train[,1] == 1 | datos_train[,1]==5,]

number_train <- datos_train[,1]
pixels_train <- array(t(datos_train[,2:257]), dim=c(16,16,nrow(datos_train)))

datos_test <- scan("datos/zip.test",sep=" ")
datos_test <- matrix(datos_test,ncol=257,byrow=T)
datos_test <- datos_test[datos_test[,1] == 1 | datos_test[,1]==5,]

number_test <- datos_test[,1]
pixels_test <- array(t(datos_test[,2:257]), dim=c(16,16,nrow(datos_test)))

# Cálculo de la simetría vertical dada una matriz
# @param M matriz
# @return Simetría vertical
simetria_vertical <- function(M){
  -sum(apply(M, 1, function(x) sum(abs(x-x[length(x):1]))))
}

# Cálculo de las medias para cada imagen de test y train
means_train <- apply(pixels_train, 3, mean)
means_test <- apply(pixels_test, 3, mean)

# Cálculo de la simetría vertical para cada imagen de test y train
sim_vertical_train <- apply(pixels_train,3,simetria_vertical)
sim_vertical_test <- apply(pixels_test,3,simetria_vertical)

# Conteo de errores según el etiquetado de una función y las etiquetas reales
# @param f Función que realizará el etiquetado
# @param data Datos a etiquetar
# @param label Etiquetas originales
# @return Número de datos erróneamente etiquetados por f
count_errors <- function(f, data, label){
  #Conteo de errores
  signs <- apply(data, 1, function(x) return(sign(f(c(x,1)))))
  return( sum(signs != label) )
}

# Método PLA Pocket
# @param data Datos sobre los que se realiza el método PLA
# @param label Etiqueta de estos datos
# @param max_iter Número máximo de iteraciones
# @param vini Vector inicial
# @param draw_iterations Valor lógico que determina si se van dibujando
#         las iteraciones
# @return hyperplane Hiperplano solución obtenido
# @return iterations Número de iteraciones
ajusta_PLA_MOD <- function(data, label, max_iter, vini, 
                           draw_iterations = FALSE){
  sol <- vini
  iter <- 0
  changed <- TRUE
  current_sol <- sol
  
  while ( iter < max_iter && changed){
    # Comprobaremos en cada vuelta si hemos hecho algún cambio
    current_errors <- count_errors(hypplane_to_func(sol), data, label )
    changed <- FALSE
    
    for( inner_iter in 1:nrow(data) ){
      # Añadimos coeficiente independiente
      x <- c(data[inner_iter,],1)
      
      if( sign(crossprod(x,current_sol)) != label[inner_iter]){
        current_sol <- current_sol + label[inner_iter] * x
        changed <- TRUE
      }
    }
    
    # Dibujo de la solución actual y la mejor encontrada.
    if(draw_iterations){
      draw_function(c(-50,50), c(-50,50),vec3D_to_func2D(sol), col = 4)
      draw_function(c(-50,50), c(-50,50),vec3D_to_func2D(current_sol),
                    col = 5, add=TRUE)
      points(lista_unif[,1], lista_unif[,2], col = (label+3))
      title(main=paste("Iteración ", iter,sep=" "))
      Sys.sleep(0.5)
    }
    
    # Actualización de la mejor solución encontrada.
    if( current_errors >= count_errors(hypplane_to_func(current_sol),
                                       data, label )){
      sol <- current_sol
    }
    
    iter <- iter+1
  }
  sol <- sol/sol[length(sol)-1]
  
  return( list( hyperplane = sol, iterations = iter))
}

# Pseudoinversa de una matriz
# @param X  Matriz de la que queremos calcular la pseudoinversa
# @return matriz pseudoinversa de X
pseudoinv <- function(X){
  desc <- svd(X)
  d <- round(desc$d, digits=5)
  d[abs(d)>0] <- 1/d[abs(d)>0]^2
  pseudo_inv <- desc$v %*% diag(d) %*% t(desc$v)
  return(pseudo_inv)
}

# Regresión lineal para ajustar unos datos a unos valores
# @param datos Datos sobre los que se realiza la regresión lineal.
# @param label Valores a los que ajustar la regresión.
# @return Coeficientes obtenidos por la regresión lineal.
regress_lin <- function(datos, label){
  pseudo_inv <- pseudoinv(datos)
  return(pseudo_inv%*%t(datos)%*%label)
}

label_5 <- numeric(length(number_train))
label_5[number_train==1] <- 1
label_5[number_train==5] <- -1

coefs_rl_5 <- regress_lin(cbind(means_train,sim_vertical_train,
                                rep(1,length(means_train))),
                          label_5)
coefs_5 <- ajusta_PLA_MOD(cbind(means_train,sim_vertical_train),
                          label_5, 100, coefs_rl_5)$hyperplane

# Apartado a)

n_train_colors <- numeric(length(number_train))
n_train_colors[number_train==1] <- 4
n_train_colors[number_train==5] <- 3
data_list_train <- list('number'=number_train, 'pixels'=pixels_train,
                        'mean'=means_train, 'v_symmetry'= sim_vertical_train,
                        'colors'=n_train_colors )

draw_function(c(-1,0.2),c(-260,10),vec3D_to_func2D(coefs_rl_5), col = 2)
draw_function(c(-1,0.2),c(-260,10),vec3D_to_func2D(coefs_5), col=4, add=TRUE)
points(data_list_train$mean, data_list_train$v_symmetry, 
       col = data_list_train$colors )
title(main="Datos Train")
pause()

n_test_colors <- numeric(length(number_test))
n_test_colors[which(number_test==1)] <- 4
n_test_colors[which(number_test==5)] <- 3
data_list_test <- list('number'=number_test, 'pixels'=pixels_test,
                       'mean'=means_test,'v_symmetry'=sim_vertical_test,
                       'colors'=n_test_colors )
draw_function(c(-1,0.2),c(-260,10),vec3D_to_func2D(coefs_rl_5), col = 2)
draw_function(c(-1,0.2),c(-260,10),vec3D_to_func2D(coefs_5), col=4, add=TRUE)
points(data_list_test$mean, data_list_test$v_symmetry, 
       col = data_list_test$colors )
title(main="Datos Test")
pause()

# Apartado b)

E_in_5b = count_errors(hypplane_to_func(coefs_5),
                       cbind(means_train, sim_vertical_train), 
                       label_5)/length(number_train)

label_test_5 <- numeric(length(number_test))
label_test_5[number_test==1] <- 1
label_test_5[number_test==5] <- -1

E_test_5b = count_errors(hypplane_to_func(coefs_5),
                         cbind(means_test, sim_vertical_test),
                         label_test_5)/length(number_test)

cat("Ein: \t",E_in_5b,
    "\nEtest: \t", E_test_5b)
pause()

# Apartado 4c)

data_transf_5d <- cbind(means_train,sim_vertical_train,
                        means_train^2,sim_vertical_train^2,
                        means_train*sim_vertical_train,
                        means_train^3,sim_vertical_train^3,
                        means_train^2*sim_vertical_train, 
                        means_train*sim_vertical_train^2)

coefs_rl_5d <- regress_lin(cbind(data_transf_5d,
                                 rep(1,length(number_train))), label_5)

estimated_f_5d <- function(x,y){
  coefs_rl_5d[1]*x + coefs_rl_5d[2]*y + coefs_rl_5d[3]*x^2 +
    coefs_rl_5d[4]*y^2 + coefs_rl_5d[5]*x*y +
    coefs_rl_5d[6]*x^3 + coefs_rl_5d[7]*y^3 +
    coefs_rl_5d[8]* x^2*y + coefs_rl_5d[9]*x*y^2 + coefs_rl_5d[10]
} 

draw_function(c(-1,0.2),c(-260,10),estimated_f_5d, col=4)
points(data_list_train$mean, data_list_train$v_symmetry,
       col = data_list_train$colors)
pause()

draw_function(c(-1,0.2),c(-260,10),estimated_f_5d, col=4)
points(data_list_test$mean, data_list_test$v_symmetry, 
       col = data_list_test$colors)
pause()

E_in_5d = count_errors(hypplane_to_func(coefs_rl_5d),
                       data_transf_5d, 
                       label_5)/length(number_train)

data_transf_test_5d <- cbind(means_test, sim_vertical_test,
                             means_test^2,sim_vertical_test^2,
                             means_test*sim_vertical_test,
                             means_test^3,sim_vertical_test^3,
                             means_test^2*sim_vertical_test,
                             means_test*sim_vertical_test^2)

E_test_5d = count_errors(hypplane_to_func(coefs_rl_5d),
                         data_transf_test_5d,
                         label_test_5)/length(number_test)

cat("Ein: \t",E_in_5d,
    "\nEtest: \t", E_test_5d)
pause()


###
# SECCIÓN SOBREAJUSTE
###

# EJERCICIO 1

# Apartado a)

# Evaluación de los polinomios de Lagrange en un punto.
# @param x Punto en el que evaluar los polinomios.
# @param k Grado máximo del polinomio de Lagrange a evaluar.
# @return Vector de longitud k+1 con las evaluaciones de los polinomios.
lagrange_poly <- function(x,k){
  if (k ==0){
    return( 1 )
  }
  else if (k ==1){
    return( c(1,x) )
  }
  
  # Creación del vector
  l_poly <- vector(mode="numeric", length = k+1)
  l_poly[1] <- 1
  l_poly[2] <- x
  
  # Recurrencia.
  for( i in 2:k){
    l_poly[i+1] <- (2*i-1)/i*x*l_poly[i] -
      (i-1)/i*l_poly[i-1]
  }
  
  return( l_poly )
}

Q_f <- 3
N <- 10
sigma <- 0.2

a_Q <- rnorm(Q_f+1)/sqrt(sum(1/(2*(0:Q_f)+1)))

# Función objetivo
# @param x Punto (o vector de puntos) en el que evaluar la sumatoria 
#           de polinomios de Legendre
# @return Vector con la evaluación de la sumatoria de polinomios en x.
f_obj <- function(x){  sapply(x, function(y) crossprod(a_Q,
                                                       lagrange_poly(y,Q_f))) }

x <- runif(N, min=-1, max=1)
y <- f_obj(x) + sigma*rnorm(N)

coefs_g2 <- regress_lin(t(sapply(x,function(i) i^(0:2))), y)
coefs_g10 <- regress_lin(t(sapply(x,function(i) i^(0:20))), y)

g2 <- function(x){
  sapply(x, function(y) crossprod(coefs_g2, y^(0:2)))
}

g10 <- function(x){
  sapply(x, function(y) crossprod(coefs_g10, y^(0:20)))
}

plot(seq(-1,1,by=0.01), sapply(seq(-1,1,by=0.01), f_obj), type ="l", col=4,
     main = "g2, g10, f", xlab = "Eje X", ylab = "Eje Y")
points(seq(-1,1,by=0.01), sapply(seq(-1,1,by=0.01), g2), type ="l", col=2)
points(seq(-1,1,by=0.01), sapply(seq(-1,1,by=0.01), g10), type ="l", col=3)
points(x,y)
pause()

# Apartado c)

cat( "Error g10:\t", 
     integrate(function(x) (g10(x)-f_obj(x))^2,-1,1,
               subdivisions = 10000L)$value)
cat( "Error g2:\t", 
     integrate(function(x) (g2(x)-f_obj(x))^2,-1,1,
               subdivisions = 10000L)$value)
pause()


# EJERCICIO 2

Q_f <- 20
N <- 50
sigma <- 1

error_g2 = vector(mode = "numeric", length = 150)
error_g10 =vector(mode = "numeric", length = 150)

for(i in 1:150){
  a_Q <- rnorm(Q_f+1)/sqrt(sum(1/(2*(0:Q_f)+1)))
  f_obj <- function(x){  sapply(x, function(y) crossprod(a_Q,
                                                         lagrange_poly(y,Q_f))) }
  x <- runif(N, min=-1, max=1)
  y <- f_obj(x) + sigma*rnorm(N)
  coefs_g2 <- regress_lin(t(sapply(x,function(i) i^(0:2))), y)
  coefs_g10 <- regress_lin(t(sapply(x,function(i) i^(0:10))), y)
  
  g2 <- function(x){
    sapply(x, function(y) crossprod(coefs_g2, y^(0:2)))
  }
  
  g10 <- function(x){
    sapply(x, function(y) crossprod(coefs_g10, y^(0:10)))
  }
  
  error_g10[i] <- integrate(function(x) (g10(x)-f_obj(x))^2,
                            -1,1,subdivisions = 100L)$value
  error_g2[i]  <- integrate(function(x)  (g2(x)-f_obj(x))^2,
                            -1,1,subdivisions = 100L)$value
}

cat("Error medio en g10=\t", mean(error_g10))
cat("Error medio en g2=\t", mean(error_g2))

# Apartado b)

Q_f = seq(1,100,by=9)
N = seq(20,100,by=10)
sigma = seq(0, 2, by=0.5)

# Obtención del error fuera de los datos de entrenamiento dadas dos funciones
# @param f_obj Función objetivo
# @param f Función estimada.
# @param range Intervalo en el que obtener el error.
# @return Error fuera de los datos de entrenamiento
getEout <- function(f_obj, f, range = c(-1,1)){
  E_out = 0
  pob = seq(range[1],range[2],by=0.2)
  for( i in 1:10){
    E_out <- E_out + (f_obj(pob[i])-f(pob[i]))^2
  }
  
  return( E_out/10 )
}

# Obtención de un polinomio por regresión lineal ajustado a una función
# @param x Puntos en los que se ha evaluado una función
# @param y Evaluación de la función
# @param grade Grado máximo del polinomio que aproxime la función
# @return Función que mejor aproxima a los datos.
getAproxPolynom <- function(x,y,grade){
  
  coefs <- regress_lin(t(sapply(x,function(i) i^(0:grade))), y)
  
  polynom <- function(x){
    sapply(x, function(y) crossprod(coefs, y^(0:grade)))
  }
  
  return(polynom)
}

# EJERCICIO 3

Q_f <- 20
N <- 50
sigma <- 1

error_g2 = vector(mode = "numeric", length = 150)
error_g10 =vector(mode = "numeric", length = 150)

for(i in 1:150){
  a_Q <- rnorm(Q_f+1)/sqrt(sum(1/(2*(0:Q_f)+1)))
  f_obj <- function(x){ 
    sapply(x, function(y){
      sign(crossprod(a_Q[2:Q_f], lagrange_poly(y,Q_f)[2:Q_f]) +
             rnorm(1,sd=sigma))})
  }
  x <- runif(N, min=-1, max=1)
  y <- f_obj(x) + sigma*rnorm(N)
  coefs_g2 <- regress_lin(t(sapply(x,function(i) i^(0:2))), y)
  coefs_g10 <- regress_lin(t(sapply(x,function(i) i^(0:10))), y)
  
  g2 <- function(x){
    sapply(x, function(y) crossprod(coefs_g2, y^(0:2)))
  }
  
  g10 <- function(x){
    sapply(x, function(y) crossprod(coefs_g10, y^(0:10)))
  }
  
  error_g10[i] <- getEout(g10,f_obj)
  error_g2[i]  <- getEout(g2,f_obj)
}

cat("Error medio en g10=\t", mean(error_g10))
cat("Error medio en g2=\t", mean(error_g2))
pause()


###
# SECCIÓN DE REGULARIZACIÓN Y SELECCIÓN DE MODELOS
###

# EJERCICIO 1

# Muestra de una distribución normal de varias dimensiones.
# @param N Tamaño de la muestra
# @param dim Número de dimensiones
# @return sigma Vector de las desviaciones típicas.
# @return Muestra obtenida.
simula_gauss <- function(N, dim, sigma){
  # Tomamos N*dim elementos de una normal, 
  # tomando la desviación estándar del vector sigma
  lista <- matrix(rnorm(N*dim, sd = sigma), N, dim, byrow=TRUE)
  return(lista)
}

N <- 100
sigma <- 0.5
d <- 3
x <- simula_gauss(N, d, 1)
w <- simula_gauss(1, d+1, 1)
y <- apply(x, 1, function(x_i)crossprod(t(w),c(x_i,1))+rnorm(1,sd=sigma))
wd_lamda <- 0.05/N


# Regresión lineal con decaimiento
# @param data Datos sobre los que ajustar la regresión.
# @param label Valores a los que ajustar la regresión.
# @param weight_decay Factor de decaimiento
# @return Coeficientes de la regresión lineal con decaimiento.
regress_wd <- function(data, label, weigth_decay = 0){
  inv <- solve(t(data)%*%data + 
                 weigth_decay*diag(ncol(data)))
  return(inv%*%t(data)%*%label)
}

coefs_rl_1 <- regress_wd(cbind(x,1), y, wd_lamda)
e <- vector(mode = "numeric", length = N)
for( n in 1:N ){
  e[n]= (y[n]-crossprod(coefs_rl_1,c(x[n,],1)))^2
}
cat("Media de error: ",mean(e),"\n")
pause()
cat("Error medio en g10=\t", mean(error_g10))
cat("Error medio en g2=\t", mean(error_g2))
pause()


errores_lambda = vector(mode = "numeric", length = 21)

for(lambda in seq(0,1,by=0.05)){
  coefs_rl_1 <- regress_wd(cbind(x,1), y, lambda)
  e <- vector(mode = "numeric", length = N)
  for( n in 1:N ){
    e[n]= (y[n]-crossprod(coefs_rl_1,c(x[n,],1)))^2
  }
  errores_lambda[20*lambda+1] = mean(e)
}

plot(seq(0,1,by=0.05),errores_lambda, main = "Error para lambda",
     ylab = "Error", xlab = "lambda")
pause()

N_values = seq(d+15, d+115, by=10)

for( N in N_values){
  x <- simula_gauss(N, d, 1)
  w <- simula_gauss(1, d+1, 1)
  y <- apply(x, 1, function(x_i)
    crossprod(t(w),c(x_i,1))+sigma*rnorm(1))
  wd_lamda <- 0.05/N
  
  e <- vector(mode = "numeric", length = N)
  for( n in 1:N ){
    Dn = list('data'=cbind(x[1:N != n,],1), 
              'value'=y[1:N != n])
    coefs <- regress_wd(Dn$data, Dn$value, wd_lamda)
    e[n]= (y[n]-crossprod(coefs,c(x[n,],1)))^2
  }
  cat("For N=",N,"\t Ecv=",mean(e),"\n")
}

repetitions = 10**3
e_1_means = vector(mode = "numeric", length = 11)
e_2_means = vector(mode = "numeric", length = 11)
E_N_means = vector(mode = "numeric", length = 11)
e_1_vars = vector(mode = "numeric", length = 11)
e_2_vars = vector(mode = "numeric", length = 11)
E_N_vars = vector(mode = "numeric", length = 11)

for( N in N_values ){
  wd_lamda <- 0.05/N
  e_1_vector = vector("numeric",length = repetitions)
  e_2_vector = vector("numeric",length = repetitions)
  E_N_vector = vector("numeric",length = repetitions)
  for ( iter in 1:repetitions ){
    x <- simula_gauss(N, d, 1)
    w <- t(simula_gauss(1, d+1, 1))
    y <- apply(x, 1, function(x_i)
      crossprod(w,c(x_i,1))+sigma*rnorm(1))
    
    e <- vector(mode = "numeric", length = N)
    for( n in 1:N ){
      Dn = list('data'=cbind(x[1:N != n,],1), 
                'value'=y[1:N != n])
      coefs <- regress_wd(Dn$data, Dn$value, wd_lamda)
      e[n]= (y[n]-crossprod(coefs,c(x[n,],1)))^2
    }
    
    e_1_vector[iter] = e[1]
    e_2_vector[iter] = e[2]
    E_N_vector[iter] = mean(e)
  }
  e_1_means[(N-18)/10+1] = mean(e_1_vector)
  e_2_means[(N-18)/10+1] = mean(e_2_vector)
  E_N_means[(N-18)/10+1] = mean(E_N_vector)
  e_1_vars[(N-18)/10+1] = var(e_1_vector)
  e_2_vars[(N-18)/10+1] = var(e_2_vector)
  E_N_vars[(N-18)/10+1] = var(E_N_vector)
  cat("For N =",N,
      "\n\t e1: mean = ",mean(e_1_vector),"\t\t var = ",var(e_1_vector),
      "\n\t e2: mean = ",mean(e_2_vector),"\t\t var = ",var(e_2_vector),
      "\n\t Ecv: mean = ",mean(E_N_vector),"\t var = ",
      var(E_N_vector),"\n")
}

means_1 <- cbind(E_N_means,e_1_means,e_2_means)
matplot(seq(d+15, d+115, by=10), means_1, type="l", main = "Media del error",
        xlab = "Número de datos", ylab = "Error")
pause()

vars_1 <- cbind(E_N_vars,e_1_vars,e_2_vars)
matplot(N_values, vars_1, type="l", main = "Varianza media del error",
        xlab = "Número de datos", ylab = "Varianza")
pause()

repetitions = 10**3
e_1_means_freal = vector(mode = "numeric", length = 11)
e_2_means_freal = vector(mode = "numeric", length = 11)
E_N_means_freal = vector(mode = "numeric", length = 11)

for( N in N_values ){
  wd_lamda <- 0.05/N
  e_1_vector_freal = vector("numeric",length = repetitions)
  e_2_vector_freal = vector("numeric",length = repetitions)
  E_N_vector_freal = vector("numeric",length = repetitions)
  for ( iter in 1:repetitions ){
    x <- simula_gauss(N, d, 1)
    w <- t(simula_gauss(1, d+1, 1))
    y <- apply(x, 1, function(x_i)
      crossprod(w,c(x_i,1))+sigma*rnorm(1))
    
    e_freal <- vector(mode = "numeric", length = N)
    for( n in 1:N ){
      Dn = list('data'=cbind(x[1:N != n,],1), 
                'value'=y[1:N != n])
      coefs <- regress_wd(Dn$data, Dn$value, wd_lamda)
      e_freal[n]= (crossprod(w,c(x[n,],1))-
                     crossprod(coefs,c(x[n,],1)))^2
    }
    
    e_1_vector_freal[iter] = e_freal[1]
    e_2_vector_freal[iter] = e_freal[2]
    E_N_vector_freal[iter] = mean(e_freal)
  }
  e_1_means_freal[(N-18)/10+1] = mean(e_1_vector_freal)
  e_2_means_freal[(N-18)/10+1] = mean(e_2_vector_freal)
  E_N_means_freal[(N-18)/10+1] = mean(E_N_vector_freal)
}

means_1_freal <- cbind(E_N_means_freal,e_1_means_freal,e_2_means_freal)
matplot(seq(d+15, d+115, by=10), means_1_freal, type="l", 
        main = "Media del error con respecto a la f original",
        xlab = "Número de datos", ylab = "Error")

pause()

#Apartado f

ratios = e_1_vars/(E_N_vars*N_values)
plot(N_values, ratios, main = "Ratios sin regularización", xlab = "Nº datos",
     ylab = "Neff/N")
points(N_values,rep(1,length(N_values)),type="l",lty=2)
points(N_values,rep(sum(ratios*N_values)/sum(N_values),length(N_values)),
       type="l",lty=2, col=2)
pause()

# Apartado g

repetitions = 10**3
e_1_means_g = vector(mode = "numeric", length = 11)
e_2_means_g = vector(mode = "numeric", length = 11)
E_N_means_g = vector(mode = "numeric", length = 11)
e_1_vars_g = vector(mode = "numeric", length = 11)
e_2_vars_g = vector(mode = "numeric", length = 11)
E_N_vars_g = vector(mode = "numeric", length = 11)

for( N in N_values ){
  wd_lamda <- 2.5/N
  e_1_vector_g = vector("numeric",length = repetitions)
  e_2_vector_g = vector("numeric",length = repetitions)
  E_N_vector_g = vector("numeric",length = repetitions)
  for ( iter in 1:repetitions ){
    x <- simula_gauss(N, d, 1)
    w <- simula_gauss(1, d+1, 1)
    y <- apply(x, 1, function(x_i)
      crossprod(t(w),c(x_i,1))+sigma*rnorm(1))
    
    e_g <- vector(mode = "numeric", length = N)
    for( n in 1:N ){
      Dn = list('data'=cbind(x[1:N != n,],1), 
                'value'=y[1:N != n])
      coefs <- regress_wd(Dn$data, Dn$value, wd_lamda)
      e_g[n]= (y[n]-crossprod(coefs,c(x[n,],1)))^2
    }
    
    e_1_vector_g[iter] = e_g[1]
    e_2_vector_g[iter] = e_g[2]
    E_N_vector_g[iter] = mean(e_g)
  }
  e_1_means_g[(N-18)/10+1] = mean(e_1_vector_g)
  e_2_means_g[(N-18)/10+1] = mean(e_2_vector_g)
  E_N_means_g[(N-18)/10+1] = mean(E_N_vector_g)
  e_1_vars_g[(N-18)/10+1] = var(e_1_vector_g)
  e_2_vars_g[(N-18)/10+1] = var(e_2_vector_g)
  E_N_vars_g[(N-18)/10+1] = var(E_N_vector_g)
  cat("For N =",N,
      "\n\t e1: mean = ",mean(e_1_vector_g),
      "\t\t var = ",var(e_1_vector_g),
      "\n\t e2: mean = ",mean(e_2_vector_g),
      "\t\t var = ",var(e_2_vector_g),
      "\n\t Ecv: mean = ",mean(E_N_vector_g),"\t var = ",
      var(E_N_vector_g),"\n")
}
pause()

plot(N_values, ratios, main = "Ratios con regularización", 
     xlab = "Nº datos", ylab = "Neff/N")
points(N_values,rep(1,length(N_values)),type="l",lty=2)
points(N_values,rep(sum(ratios*N_values)/sum(N_values),length(N_values)),
       type="l",lty=2, col=2)
ratios_g = e_1_vars_g/(E_N_vars_g*N_values)
points(N_values, ratios_g, col=3)
points(N_values,rep(sum((ratios_g*N_values))/sum(N_values),
                    length(N_values)),
       type="l",lty=2, col=4)
pause()

cat("var(ratios) =", var(ratios))
cat("var(ratios_g) =", var(ratios_g))
pause()

