#Simular una uniforme discreta. 

m = 1000
n = 10
u = runif(m)
X = floor(n*u)+1

hist(X)


#Simular distribución geometrica.

n = 1000
p = 0.2
q = 1-p
u = runif(1:n)

X = floor(log(u)/log(q))+1
hist(X)

#Simular números aleatorios de una variable aleatoria con distribución Poisson. 
#DiStribución Poisson Parámetro L. 

L = 4

n = 1000
X = vector(mode = 'double', n)

for(j in 1:n){

  u = runif(1)
  i = 0
  p = exp(-L)
  F = p
  
  while(u>=F){
    p = L*p/(1+i)
    F = F+p
    i = i+1
    
  }
  X[j] = i
}

mean(X)
hist(X)

#-----------------------------

#Permutaciones aleatorias

n = 50

P = 1:n


for(k in n:1){

  I = floor(runif(1)*k) + 1
  Aux = P[I]
  
  P[I] = P[k]
  
  P[k] = Aux
}



