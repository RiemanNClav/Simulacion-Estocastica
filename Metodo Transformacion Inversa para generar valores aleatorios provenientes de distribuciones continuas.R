#objetivo es simular numeros poisson, simular uniformes hasta que sean menores que e^{-lambda}

#simular un numero con distribucion poisson
 L = 5
 num_sim = 1000
 
 X = vector(mode = 'double', length = num_sim)
 
 for(i in 1:num_sim){
   n = 1
   u = runif(1)
   while(u >=exp(-L)){
     u = u * runif(1)
     n =n + 1
     
   }
   
   n = n-1
   X[i] = n
 }
 
 barplot(tabulate(X))
 
 
 #para simular continuas invertir la función de distribución. 
 
 #generar los valores de una variable aleatoria gamma. 
 #simular numeros con distribución Gamma(n,L)
 
 L = 0.3
 n = 4
 num_sim = 10000
 
 G = vector(mode = 'double', length = num_sim)
 
 for(i in 1:num_sim){
 
   u = runif(n)
   G[i] = (-1/L)*sum(log(u))
 
 }
 hist(G)
 