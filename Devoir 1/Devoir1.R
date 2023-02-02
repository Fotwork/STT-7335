###---------------QUESTION 1----------------###

#loading data set 
data(g2016)

#getting some information from it
help(g2016)
colnames(g2016)
dim(g2016)
head(g2016, n=1)$cces_n_vv

## Data Quality 

alabama_state <- head(g2016, n=1)
alabama_mu <- alabama_state$votes_djt/alabama_state$tot_votes
alabama_muhat <- alabama_state$cces_totdjt_vv/alabama_state$cces_n_vv
alabama_N <- alabama_state$tot_votes
alabama_n <- alabama_state$cces_n_vv

data_quality = ddc(mu = alabama_mu, muhat = alabama_muhat, N = alabama_N, n = alabama_n)

## Data Quantity

f <- alabama_n/alabama_N

data_quantity <- sqrt(1-f/f)
print(data_quantity)

## Problem Difficulty

problem_difficulty <- 
  
###---------------QUESTION 2----------------###

## Simulation jeu de données suivant loi normale bivariée.
set.seed(92639)
  
launch_simulation = function(imputation_method, model_stat){
  mu <- c(100, 12) # vecteur de moyennes pour Y1 et Y2
  cov_matrix <- matrix(c(169, 19.5, 19.5, 9), nrow = 2) # matrice de covariance
  pop_simu = 250
  data <- mvrnorm(n = pop_simu, mu = mu, Sigma = cov_matrix) # génération des données
  colnames(data) <- c("Y1", "Y2") # noms de colonnes pour les données
  data <- model_stat(data)
  data <- imputation_method(data)
  
  return(data)
}

### MODELS STATISTIQUES ###

## MCAR

MCAR = function(data){
  missing_prob <- 0.5 # probabilité de valeurs manquantes pour Y2
  missing_indicator <- rbinom(pop_simu, size = 1, prob = missing_prob) # génère un indicateur binaire pour déterminer si Y2 est manquant
  data[,"Y2"][missing_indicator == 1] <- NA
  
  return(data)
}

## MAR

MAR = function(data){
  new_data <- data
  indx <- order(new_data[,"Y1"])[1:125]
  new_data[indx, "Y2"] <- NA
  
  return(new_data)
}


### METHODES D'IMPUTATION ###

## Imputation par la moyenne 

imputation_by_mean = function(data){
  
  mean_Y2 <- mean(data[ , "Y2"], na.rm = TRUE) # Calcule la moyenne de Y2 en ignorant les valeurs manquantes
  data[ ,"Y2"][is.na(data[, "Y2"])] <- mean_Y2 # Remplace les valeurs manquantes de Y2 par la moyenne
  
  return(data)
}

## Imputation par regression linéaire.

imputation_by_lr = function(data){
  model = lm(Y2 ~ Y1, data = as.data.frame(data), na.action = na.omit)
  data[is.na(data[ ,"Y2"]), "Y2"] = predict.lm(model, as.data.frame(data[is.na(data[ ,"Y2"]), ]))
  
  return(data)
}

## Imputation par regression linéaire stochastique

imputation_by_lr_sto = function(data){
  model = lm(Y2 ~ Y1, data = as.data.frame(data), na.action = na.omit)
  imputations = predict.lm(model, as.data.frame(data[is.na(data[ ,"Y2"]), ]))
  errors = rnorm(length(imputations), mean = 0, sd = summary(model)$sigma)
  imputations = imputations + errors
  data[is.na(data[ ,"Y2"]), "Y2"] = imputations
  
  return(data)
  
}


## Evaluation des estimateurs de Y2

evaluate_estimators = function(imputation_method, model_stat){
  
  n_simulations <- 300 # Nombre de simulations
  mean_Y2_estimates <- numeric(n_simulations) # Initialise un vecteur pour stocker les estimateurs de la moyenne de Y2
  var_Y2_estimates <- numeric(n_simulations) # Initialise un vecteur pour stocker les estimateurs de la variance de Y2
  cor_Y1_Y2_estimates <- numeric(n_simulations)
  
  for (i in 1:n_simulations) {
    data <- launch_simulation(imputation_method, model_stat)
    var_Y2_estimates[i] <- var(data[ , "Y2"])
    mean_Y2_estimates[i] <- mean(data[ , "Y2"])
    cor_Y1_Y2_estimates[i] <- cov(data[ , "Y1"], data[ , "Y2"])
  }
  
  var_Y2_estimate <- sum(var_Y2_estimates)/n_simulations
  mean_Y2_estimate <- sum(mean_Y2_estimates)/n_simulations
  cor_Y1_Y2_estimate <- sum(cor_Y1_Y2_estimates)/n_simulations
  
  out <- matrix(c(12, 9, 19.5, mean_Y2_estimate, var_Y2_estimate, cor_Y1_Y2_estimate), nrow = 2, ncol = 3, byrow = TRUE)
  colnames(out) = c("Moyenne Y2", "Variance Y2", "Covariance Y1 Y2")
  rownames(out) = c("Réel", "Estimateur")
  
  return(out)
}

mean_mcar = evaluate_estimators(imputation_by_mean, MCAR)
print(mean_mcar)

mean_mar <- evaluate_estimators(imputation_by_mean, MAR)
print(mean_mar)

lr_mar <- evaluate_estimators(imputation_by_lr, MAR)
print(lr_mar)

lr_sto_mar <- evaluate_estimators(imputation_by_lr_sto, MAR)
print(lr_sto_mar)

