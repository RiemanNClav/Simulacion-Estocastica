

#En los ejercicios 3-9 usa simulación para aproximar las siguientes integrales. 
#3
g <- function(x){
  exp(exp(x))
}

h <- function(y,a,b){
  (b-a)*g(a+((b-a)*y))
}

muestra <- vector(mode = 'numeric', length = 10000)
for (i in 1:10000) {
  u = runif(1)
  muestra[i] = h(u,0,1)
}

aproximacion_integral = mean(muestra)
print(aproximacion_integral)

#-----------------------------------------------------------------------------
#4.
g <- function(x){
  (1-x^(2))^(3/2)
}

h <- function(y,a,b){
  (b-a)*g(a+((b-a)*y))
}

muestra <- vector(mode = 'numeric', length = 10000)
for (i in 1:10000) {
  u = runif(1)
  muestra[i] = h(u,0,1)
}

aproximacion_integral = mean(muestra)
print(aproximacion_integral)

#------------------------------------------------------------------------------
#5.
g <- function(x){
  exp(x+x^(2))
}

h <- function(y,a,b){
  (b-a)*g(a+((b-a)*y))
}

muestra <- vector(mode = 'numeric', length = 10000)
for (i in 1:10000) {
  u = runif(1)
  muestra[i] = h(u,-2,2)
}

aproximacion_integral = mean(muestra)
print(aproximacion_integral)


#------------------------------------------------------------------------------
#6. 
func_g <- function(x){
  x*((1+x^2)^-2)
}

func_h <- function(y){
  func_g(1/y-1)/(y^2)
}
#Para calcular el parametro tetha, usaremos la ley de los grandes numeros
#suponer "y" variable aletoria entre 0 y 1.
#muestrear muchas veces esa uniforme la evaluamos en func_h y sacamos un promedio

muestra <- vector(mode = 'numeric', length = 10000)

for (i in 1:10000) {
  u = runif(1)
  muestra[i] = func_h(u)
}

aproximacion_integral = mean(muestra)
print(aproximacion_integral)

#-------------------------------------------------------------------------------
#7.
func_g <- function(x){
  exp(-x^(2))
}

func_h <- function(y){
  func_g(1/y-1)/(y^2)
}


muestra <- vector(mode = 'numeric', length = 10000)

for (i in 1:10000) {
  u = runif(1)
  muestra[i] = func_h(u)
}

aproximacion= mean(muestra)

aprox1 = aproximacion * (-1)
aprox2 = aproximacion
print(aprox1 + aprox2)








