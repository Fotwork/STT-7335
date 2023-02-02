set.seed(1635)

#Définir une fonction pour la simulation
sim = function(p, prob_rep = c(0.2, 0.4)){ 
  #on peut varier p la proportion de la pop dans l'échantillon non-probabiliste et les valeurs de p1 et p2
  
  N=1000
  n1=N*0.05
  n2=N*p
  B=100
  est_eas = rep(NA,B)
  est_R = rep(NA,B)
  prob_Y = 0.5 #probabilité que la variable binaire = 1
  
  pop_y = rbinom(N,1,prob_Y) 
  probs_R = (pop_y==0)*prob_rep[1] + (pop_y==1)*prob_rep[2]
  
  
  for(i in 1:B){
    samp_eas = sample(1:N, n1) #prob égales
    samp_R = sample(1:N, n2, prob = probs_R)
    est_eas[i] = mean(pop_y[samp_eas])
    est_R[i] = mean(pop_y[samp_R])
  }
  #  mean((est_eas - mean(pop_y))^2)
  #  mean((est_R - mean(pop_y))^2)
  
  ratio  = mean((est_eas - mean(pop_y))^2) / mean((est_R - mean(pop_y))^2)
  
  return(ratio)
}

ps = seq(0.1,0.9,length=100)
out = rep(NA,100)
for( i in 1:100 ){
  out[i] = sim(ps[i])
}
plot(ps,out, type = "b", pch = 16, las =1, xlab = "Proportion de la population dans l'échantillon non-probabiliste", ylab = "Ratio", main = "Ratio de l'EQM de l'échantillon ÉAS représentant 5% de la population et de l'EQM de l'échantillon non-probabiliste. ")
abline(h=1)

