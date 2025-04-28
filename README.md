# clase-modelos
clase del 28 de abril modelos y simulación
library("pacman")
p_load(actuar, dplyr)

#######3.1 Probabilidad condicional

#Simulacion de datos
options(scipen =999)
set.seed(12345)

#Generar 2 vectores de 1000 datos con ~Bernoulli
A <-rbinom(1000, 1, 0.5)
B <-rbinom(1000, 1, 0.5)

#Proba Marginal
P_A <-mean(A)
P_B <-mean(B)

#Proba condicional

#Seleccionar los elementos de A donde B es =1
P_A_B <-mean(A[B ==1]) #ya que es una variable binaria 0 o 1

#la media de estos valores es equivalente a la proporcion de veces que
#A es 1 cuando B es 1
cat("La proba condicional es: ", P_A_B, "\n")

#Probabilidad de la intersección de A y B
P_A_y_B <- mean (A==1 & B==1)
cat("La proba de intersección es:", P_A_y_B, "\n")
cat("La proba usando marginales: ", P_A*P_B, "\n")
#Dan diferentes porque la muestra esta condicionada a 1000 observaciones, entra mas sean mas parecidas seran


#######3.2 Media, varianza y correlación de datos sim

#Simulacion de datos de reclamaciones y edad
set.seed(12345)
edad <- rnorm(5000, mean=45, sd=2) #Edad de los asegurados
reclam <- rlnorm(5000, meanlog=2.5, sdlog=2.5)  # ~reclamaciones

#Estimacion de la media, varianza y correlación
media_reclam <- mean(reclam, na.rm = T)
varianza_reclam <- var(reclam, na.rm = T)
correlacion <- cor(edad, reclam)

list(media_reclam, varianza_reclam, correlacion)

#######3.3 Prueba de hipotesis
set.seed(12345)
reclam_anteriores <- rlnorm(1000, meanlog=-1.5, sdlog=2.1)
reclam_actuales <- rlnorm(1000, meanlog =2, sdlog=1.3)

#Estimacion de la media y el error estandar
mean_recla_ant<- mean(reclam_anteriores)
mean_recla_act<- mean(reclam_actuales)
desv_ant <-sd(reclam_anteriores)/ sqrt(length(reclam_anteriores))
desv_act <- sd(reclam_actuales) / sqrt(length(reclam_actuales))

#Intervalos de confianza al 95%
alpha <- 0.05
z <- qnorm(1-alpha/2)

ci_lower_ant <- mean_recla_ant - z*desv_ant
ci_upper_ant <- mean_recla_ant + z*desv_ant

ci_lower_act <- mean_recla_act - z*desv_act
ci_upper_act <- mean_recla_act + z*desv_act

#Resultados de las media para ambos años
cat("La media de las reclamaciones anteriores son:", mean_recla_ant, "\n")
cat("La media de las reclamaciones actuales son:", mean_recla_act, "\n")

#Resultados de los IC
cat("Los intervalos de las reclamaciones anteriores son: [", ci_lower_ant, ",", ci_upper_ant, "]\n")
cat("Los intervalos de las reclamaciones actuales son: [", ci_lower_act, ",", ci_upper_act, "]\n")

#Prueba de hipotesis para comparar medias
t.test(reclam_anteriores, reclam_actuales, paired = FALSE) #FALSE ya que las muestras son individuales 

#######3.4 Media y varianza condicional
set.seed(12345)
X <- rlnorm(1000, meanlog=0.5, sdlog =1.2)
Y <- rpareto(1000, shape=2, scale=90)

#Calcular la media y varianza condicional
#Dado que X y Y son indep , E[X|Y] = E[X] y VAR[X|Y] = VAR[X]

#Media de X
E_X <-mean(X)

#Varianza de X
Var_X <- var(X)

#Media condicional de X dado Y
E_X_Y <-rep(E_X, length(Y))

#Varianza condicional de X  dado Y
Var_X_Y <- rep(Var_X, length(Y))

#Verificar las propiedades
#E[E[X|Y]] = E[X]
E_E_X_Y <- mean(E_X_Y)

#E[VAR[X|Y]] + VAR[E[X|Y]] = VAR[X]
E_Var_X_Y <-mean(Var_X_Y)
Var_E_X_Y <- var(E_X_Y)

#Resultados
#Media condicional
cat("E[X]:", E_X, "\n")
cat("E[E[X|Y]]:", E_E_X_Y, "\n")

#Esperanza de varianza y varianza de esperanza
cat("E[Var[X|Y]]:", E_Var_X_Y, "\n")
cat("Var[E[X|Y]]:", Var_E_X_Y, "\n")

#Varianza condicional
cat("Var[X]:", Var_X, "\n")
cat("E[Var[X|Y]] + Var[E[X|Y]]:", E_Var_X_Y + Var_E_X_Y, "\n")

#######3.5 Tecnica Bootstrap
#vas checando los errores, el error te sirve para checar que tan bien ajustado esta tu modelo
set.seed(12345)

reclam <- rlnorm(20, meanlog=2.5, sdlog= 2.5)
bootstrap_mse <- function(data, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(mean(mse_values))
}

bootstrap_mse <- function(data, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(mean(mse_values))
}

bootstrap_desv <- function(data, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(sd(mse_values) / sqrt(length(mse_values)))
}
#Estimacion del MSE usando bootstrap
n_bootstrap <- 100000
mse_estimate <-bootstrap_mse(reclam, n_bootstrap)
mse_desvz<-bootstrap_desv(reclam, n_bootstrap)
cat("El MSE es", format(mse_estimate, big.mark = ","), "\n")

#IC del MSE
alpha <- 0.05
z <- qnorm(1-alpha/2)

ci_lower_mse <- mse_estimate - z*mse_desvz
ci_upper_mse <- mse_estimate + z*mse_desvz

#Graficar densidad del MSE y los IC al 95%
plot(density(mse_estimate,mse_desvz), 
     main = "Distribución Bootstrap del MSE",
     xlab = "MSE", ylab = "Densidad",
     col = "blue", lwd = 2)
abline(v = mse_estimate, col = "red", lwd = 2, lty = 2)
abline(v = ci_lower_mse, col = "darkgreen", lwd = 2, lty = 3)
abline(v = ci_upper_mse, col = "darkgreen", lwd = 2, lty = 3)
legend("topright", 
       legend = c("Densidad Bootstrap", 
                  paste("Media =", format(mse_estimate, digits = 5)), 
                  "IC 95%"),
       col = c("blue", "red", "darkgreen"), 
       lty = c(1, 2, 3), lwd = 2)

#3.5 con archivo reclam.csv

reclam <- c(144,	134,	185,	141,	205,	126,	123,	152,	123,	215,	170,	165,	180,	175,	160,	185,	168,	172,	178,	169) 
bootstrap_mse <- function(data, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(mean(mse_values))
}

bootstrap_mse <- function(data, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(mean(mse_values))
}

bootstrap_desv <- function(data, n_bootstrap){
  mse_values <- numeric(n_bootstrap)
  for (i in 1:n_bootstrap) {
    sample_data <- sample(data, replace = TRUE)
    mse_values[i] <- mean((sample_data - mean(sample_data))^2)
  }
  return(sd(mse_values) / sqrt(length(mse_values)))
}
#Estimacion del MSE usando bootstrap
n_bootstrap <- 100000
mse_estimate <-bootstrap_mse(reclam, n_bootstrap)
mse_desvz<-bootstrap_desv(reclam, n_bootstrap)
cat("El MSE es", format(mse_estimate, big.mark = ","), "\n")

#IC del MSE
alpha <- 0.05
z <- qnorm(1-alpha/2)

ci_lower_mse <- mse_estimate - z*mse_desvz
ci_upper_mse <- mse_estimate + z*mse_desvz

#Graficar densidad del MSE y los IC al 95%
plot(density(mse_estimate,mse_desvz), 
     main = "Distribución Bootstrap del MSE archivo reclam.csv",
     xlab = "MSE", ylab = "Densidad",
     col = "blue", lwd = 2)
abline(v = mse_estimate, col = "red", lwd = 2, lty = 2)
abline(v = ci_lower_mse, col = "darkgreen", lwd = 2, lty = 3)
abline(v = ci_upper_mse, col = "darkgreen", lwd = 2, lty = 3)
legend("topright", 
       legend = c("Densidad Bootstrap", 
                  paste("Media =", format(mse_estimate, digits = 5)), 
                  "IC 95%"),
       col = c("blue", "red", "darkgreen"), 
       lty = c(1, 2, 3), lwd = 2)

