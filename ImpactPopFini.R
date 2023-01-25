set.seed(1954)

n = 100
mu = 3
B = 1000 #Nombre de répétitions
out = rep(NA,B) 
print(out)

?rexp

for(i in 1:B){
  samp = rexp(n, rate = 1/mu) #Échantillonnage d'une loi exponentielle
  out[i] = mean(samp)  
}

print(samp)
print(out)

mean(samp)
var(samp)

mu^2 / n

n = 100
mu = 3
B = 1000 
out = replicate(B,mean(rexp(n,rate = 1/mu)))
var(out)

#----- Tirage d’une population finie --------

N = 1000
n = 100
mu = 3
B = 5000 
pop = rexp(N,rate = 1/mu)
out = replicate(B,mean(sample(pop,size=n)))

print(out)

var(out)
