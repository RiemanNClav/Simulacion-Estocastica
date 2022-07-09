runif(1) #  Numero aleatorio entre 0 y 1. 
runif(2) #2 Numeros aletorios entre 0 y 1.

set.seed(1234)

uniformes = runif(10000)
hist(uniformes)

#aproximando integrales con simulación

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


#--------------------------------------------------------------------------
  
muestras = 10000

N = vector(mode = 'numeric', length = muestras)
for(i in 1:muestras){

  acum = runif(1)
  N[i] = 1
  while(acum<=1){
    acum = acum + runif(1)
    N[i] = N[i]+1
  }

}
sum(N)
mean(N)






