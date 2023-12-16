rm(list = ls())
setwd('C:/Users/USUARIO/Documents/Universidad/2023-2S/Estadística Bayesiana/Caso 3')
#Librerías
library(Boom) #dirichlet
library(coda) #tamaños efectivos de muestra
library(xtable) #exportar a latex
library(ggplot2)#gráficas
library(reshape2) #formato long
library(readxl)
library(mvtnorm) #normal multivariado
library(pracma)
library(dplyr)
library(gridExtra)

#--------Funciones previas-----------
EMC <- function(x){
  #Error de montecarlo
  neff <- effectiveSize(x)
  return(sd(x)/sqrt(neff))
}
CVMC <- function(x){
  #cv de monte carlo
  cv <- 100*abs(EMC(x)/mean(x))
  return(cv)
}
cv <- function(x){
  #Coeficiente de variacion
  return(100*abs(sd(x)/mean(x)))
}

tr <- function(A){
  return(sum(diag(A)))
}

#-------PRIMER INCISO--------------

#Datos
ns <- c(493, 257, 227, 48, 41, 38, 28, 11, 3, 54)
thetas <- c(0.411, 0.214, 0.189, 0.040, 0.034, 0.032, 
            0.023, 0.009, 0.003, 0.045)
n <- sum(ns)
a <- b <- 1
k <- length(thetas)

#Expresión de la log distribución de log de alpha

dgama1 <- function(r, t){
  b <- (exp(r) - 1)*sum(log(t)) + r - exp(r) + lgamma(10*exp(r)) - 10*lgamma(exp(r))
  return(b)
}

#Valor incial
theta <- thetas
gama <- 0.3

#Parámetro de ajuste
delta <- 1.5

#Número de iteraciones
B <- 101000 #total iteraciones
C <- 1000 #calentamiento
A <- 10 #Amplitud del muestreo

#Almacenamiento
MC <- matrix(NA, nrow = (B-C)/A, ncol = k+1)
LL <- matrix(NA, nrow= (B-C)/A, ncol = 1)
index <- seq(from = 1, to = B, by = A)
ind <- 1

#Tasa de aceptación
ac <- 0
set.seed(1)
for (b in 1:B){
  #Actualización de theta
  theta <- rdirichlet(n = 1, nu = rep(exp(gama), k) + ns)
  
  #Actualización de alpha - metropolis
  gamaI <- rnorm(n = 1, mean = gama, sd = sqrt(delta))
  
  #Tasa de aceptación
  log.r <- dgama1(gamaI, theta) - dgama1(gama, theta)
  if (runif(1) < exp(log.r)){
    gama <- gamaI
    ac <- ac + 1
  }
  
  if ((b-C) %in% index){
    LL[ind,] <- sum(dmultinom(x = ns, size = n, prob = theta, log = T))
    MC[ind,] <- c(theta, exp(gama))
    ind <- ind + 1
  }
}
ac/B*100
#Diagnostico de convergencia
EMC1 <- apply(MC, 2, EMC) #Error de monte carlo
CVMC1 <- apply(MC, 2, CVMC) #CV de monte carlo
neff1 <- effectiveSize(MC) #Tamaño efectivo de muestra
plot(1:10000, LL[,1], xlab = 'Iteraciones', ylab = 'Log Verosimlitud', pch = 20)

#Criterios de información
l1 <- sum(dmultinom(x = ns, size = n, prob = colMeans(MC[,c(1:10)]), log = T))
pDIC1 <- 2*(l1 - mean(LL[,1])); pDIC1
DIC1 <- -2*l1 + 2*pDIC1; DIC1

#Inferencia
phi <- round(colMeans(MC), 5)
IC <- round(apply(MC, 2, function(x) quantile(x, c(0.025, 0.975))), 5)
Resul <- data.frame(Estimacion = phi*100, CV = apply(MC, 2, cv) ,IC_Inf = IC[1,]*100, IC_sup = IC[2,]*100)
Resul[11, ] <- Resul[11,]/100
xtable::xtable(Resul[1:3,], digits = 4)

#Registraduría
Resul$Real <- c(49.2, 18.71, 20.10, 2.14, 2.28, 1.02, 1.13, 0.51, 0.16, 4.87, NA)
Resul$Name <- c('C. F. Galán', 'G. Bolivar', 'J. D. Oviedo', 'D. Molano',
               'R. Lara', 'J. L. Vargas', 'J. E. Robledo', 'N. Ramos', 
               'R. A. Quintero', 'Voto en Blanco', 'Alpha')

#Visual - Estimacion vs Registraduria
# df_melt <- melt(Resul[c(1:3),c(1,5,6)], id.vars = "Name", variable.name = "tipo", value.name = "valor")
# ggplot(df_melt, aes(x = Name, y = valor, fill = tipo)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   theme_minimal() + 
#   labs(x = "", y = "% Votos", fill = "", title = 'Estimación vs Real') +
#   theme(plot.title = element_text(hjust = 0.5),
#         legend.position = 'bottom',
#         legend.direction = 'horizontal',
#         axis.text.x = element_text(face = 'bold', size = 10)) + 
#   geom_text(aes(label = paste0(round(valor, 2), "%")), vjust = -0.3, 
#             position = position_dodge(0.9)) +
#   scale_fill_manual(values = c('lightblue', '#D2241F')) + 
#   scale_y_continuous(limits = c(0,60))
ggplot(data = Resul[c(1:3),], aes(x = Name)) +
  geom_errorbar(aes(ymin = IC_Inf, ymax = IC_sup), size = 1, width = 0.1) +
  geom_point(aes(x = Name, y = Estimacion, color = "Estimación"), size = 2) +
  geom_point(aes(x = Name, y = Real, color = "Valor Real"), size = 2) + 
  labs(x = '', y = '% votos', title = 'Estimación vs Real') +
  scale_color_manual(values = c("Estimación" = "blue", "Valor Real" = "red"),
                     labels = c("Estimación", "Valor Real"), name = NULL) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        axis.text.x = element_text(face = 'bold', size = 10))


#-------SEGUNDO INCIDO------
YX.test <- as.data.frame(YX.test)
YX.train <- as.data.frame(YX.train)
# write_xlsx(YX.test, 'YX.test.xlsx')
# write_xlsx(YX.train, 'YX.train.xlsx')

#DATOS
X.train <- as.matrix(YX.train[,-1]) #matriz de diseño
Y.train <- as.matrix(YX.train[,1]) #matriz de respuesta
X.test <- as.matrix(YX.test[,-1])
Y.test <- as.matrix(YX.test[,1])
n.train <- nrow(X.train)
p <- ncol(X.train)
#Previa Unitaria
{
#Parametros de ols
Beta_ols <- solve(t(X.train)%*%X.train)%*%t(X.train)%*%Y.train
Sig2_ols <- sum((Y.train - X.train%*%Beta_ols)^2)/(n.train-p)

#Hiperparámetros
Bo <- Beta_ols
Sig2o <- n.train*Sig2_ols*solve(t(X.train)%*%X.train)
nu0 <- 1
s20 <- Sig2_ols

#Algunos términos
s <- solve(Sig2o)
sb <- s%*%Bo
xs <- t(X.train)%*%X.train

#Valores inciales
set.seed(1)
Betas <- c(rmvnorm(n = 1, mean = Bo, sigma = Sig2o))
set.seed(1)
Sigma <- 1/rgamma(n = 1, shape = nu0*0.5, rate = nu0*s20*0.5)
B <- 21000
C <- 1000
A <- 2
MC2 <- matrix(NA, nrow = (B-C)/A, ncol = p+1)
LL <- matrix(NA, nrow = (B-C)/A, ncol = 1)
index <- seq(from = 1, to = B, by = A)
ind <- 1
set.seed(1)
for (b in 1:B){
  Vbeta <- solve(s + 1/Sigma*xs)
  Betas <- c(rmvnorm(n = 1, mean = Vbeta%*%(sb + 1/Sigma*t(X.train)%*%Y.train), 
                   sigma = Vbeta))
  Sigma <- 1/rgamma(n = 1, shape = 0.5*(nu0 + n.train), rate = 0.5*sum((Y.train - X.train%*%Betas)^2))
  if ((b-C) %in% index){
    MC2[ind, ] <- c(Betas, Sigma)
    LL[ind, ] <- sum(dmvnorm(x = drop(Y.train), mean = drop(X.train%*%matrix(Betas)), sigma = diag(Sigma, n.train, n.train), log = TRUE))
    ind <- ind + 1
  }
}

#----Diagnostico----
EMC2 <- apply(MC2, 2, EMC)
CVMC2 <- apply(MC2, 2, CVMC)
neff2 <- effectiveSize(MC2)
plot(1:10000, LL[,1], xlab = 'Iteraciones', ylab = 'Log Verosimilitud', 
     pch = 20, main = 'Previa unitaria')
#----Inferencia----
Yhat.test <- X.test%*%matrix(colMeans(MC2[,-65]))
Epsilon <- mean(abs(Y.test - Yhat.test))

g1 <- ggplot(data = data.frame(cbind(Y.test, Yhat.test))) +
  geom_point(aes(x = X1, y = X2), color ='blue') + 
  labs(y = expression(widehat(Y)[test]), x = expression(Y[test]), 
       title = paste('Previa Unitaria', '\n', paste0('EAM: ', round(Epsilon, 3)))) +
  geom_abline(intercept = 0, slope =1, color='red', linewidth = 1) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)) +
  scale_x_continuous(breaks = seq(from = -1.5, to = 2, by = 0.5)) + 
  scale_y_continuous(breaks = seq(from = -1.5, to = 3, by = 0.5))

#----DIC----
pDIC <- 2*(dmvnorm(x = drop(Y.train), mean = drop(X.train%*%matrix(colMeans(MC2[,-65]))), sigma = diag(mean(MC2[,65]), n.train, n.train), log = TRUE) -
  mean(LL[,1]))

DIC <- -2*dmvnorm(x = drop(Y.train), mean = drop(X.train%*%matrix(colMeans(MC2[,-65]))), sigma = diag(mean(MC2[,65]), n.train, n.train), log = TRUE) +
  2*pDIC

#----Bondad de Ajuste del modelo----
BA1 <- matrix(NA, nrow = 10000, ncol = 1)
set.seed(1)
for (i in 1:10000){
  y <- rmvnorm(n = 1, mean = X.train%*%matrix(MC2[i,-65]), sigma = diag(MC2[i,65], n.train, n.train))
  BA1[i, ] <- mean(c(y))
}
}
#Previa G
{
#Hiperparámetros
g <- n.train
S20 <- Sig2_ols
nu0 <- 1

#Valores inciales
IX <- solve(t(X.train)%*%X.train)%*%t(X.train)
Vbeta <- (g/(g+1))*solve(t(X.train)%*%X.train)
Ebeta <- (g/(g+1))*IX%*%Y.train
SSSg <- as.numeric(t(Y.train)%*%(diag(1, n.train) - g/(g+1)*X.train%*%IX)%*%Y.train)

MC3 <- matrix(NA, nrow = 10000, ncol = p + 1)
colnames(MC3) <- c(paste0('Beta', 1:p), 'Sigma2')
B <- 10000
set.seed(1)
for(b in 1:B){
  sig2_mc <- 1/rgamma(n = 1, shape = 0.5*(nu0 + n.train), rate = 0.5*(nu0*s20 + SSSg))
  Beta_mc <- c(rmvnorm(n = 1, mean = Ebeta, sigma = Vbeta*sig2_mc))
  MC3[b,] <- c(Beta_mc, sig2_mc) 
}
LL3 <- matrix(NA, nrow = 10000, ncol = 1)
for (b in 1:10000){
  LL3[b, ] <- sum(dmvnorm(x = drop(Y.train), 
                          mean = drop(X.train%*%matrix(MC3[b,1:64])), 
                          sigma = diag(MC3[b, 65], n.train, n.train), log = TRUE))
}

#Estimación de y.test
Yhat2.test <- X.test%*%as.matrix(colMeans(MC3[,-65]))

#Error absoluto medio
Epsilon2 <- mean(abs(Y.test - Yhat2.test))

#Gráfica Y.test vs ^Y.test
g2 <- ggplot(data = data.frame(cbind(Y.test, Yhat2.test))) +
  geom_point(aes(x = X1, y = X2), color ='blue') + 
  labs(y = expression(widehat(Y)[test]), x = expression(Y[test]), 
       title = paste('Previa g', '\n', paste0('EAM: ', round(Epsilon2, 3)))) +
  geom_abline(intercept = 0, slope =1, color='red', linewidth = 1) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)) + 
  scale_x_continuous(breaks = seq(from = -1.5, to = 2, by = 0.5)) +
  scale_y_continuous(breaks = seq(from = -1.5, to = 3, by = 0.5))
#Bondad de ajuste
BA2 <- matrix(NA, nrow = 10000, ncol = 1)
for (i in 1:B){
  y <- rmvnorm(n = 1, mean = X.train%*%matrix(MC3[i,-65]), sigma = diag(MC3[i,65], n.train, n.train))
  BA2[i, ] <- mean(c(y))
}

#DIC
pDIC2 <- 2*sum(dmvnorm(x = drop(Y.train), mean = drop(X.train%*%matrix(as.numeric(colMeans(MC3[,1:64])))), 
                    sigma = diag(as.numeric(mean(MC3[,65])), n.train, n.train), log = TRUE) -
              mean(LL4$logV))

DIC2 <- -2*dmvnorm(x = drop(Y.train), mean = drop(X.train%*%matrix(colMeans(MC3[,1:64]))), sigma = diag(mean(MC3[,65]), n.train, n.train), log = TRUE) +
  2*pDIC2
}
#regresión rigida
{
# Definir funciones para las DCC
#Parametros de ols
Beta_ols <- solve(t(X.train)%*%X.train)%*%t(X.train)%*%Y.train
Sig2_ols <- sum((Y.train - X.train%*%Beta_ols)^2)/(n.train-p)

#Hiperparámetros
Bo <- Beta_ols
nu0 <- 1
s20 <- Sig2_ols
a_lambda <- 1
b_lambda <- 2


# DCC para beta condicional a lambda y sigma^2
sample_beta_conditional <- function(lambda, sigma_squared, X, y) {
  cov_matrix <- solve((t(X) %*% X +  lambda * diag(ncol(X))/sigma_squared))
  mean_vector <- cov_matrix %*% ((t(X) %*% y)/sigma_squared)
  beta_sample <- mvrnorm(1, mu = mean_vector, Sigma = sigma_squared * cov_matrix)
  return(beta_sample)
}

# DCC para sigma^2 condicional a lambda y beta
sample_sigma_squared_conditional <- function(lambda, beta, nu0, sigma_squared_0, X, y) {
  shape <- (nu0 + p + length(y)) / 2
  rate <- (nu0 * sigma_squared_0 + sum((y - X %*% beta)^2) + (sum(beta^2) * lambda))/ 2
  sigma_squared_sample <- 1 / rgamma(1, shape = shape, rate = rate)
  return(sigma_squared_sample)
}

# DCC para lambda condicional a beta y sigma^2
sample_lambda_conditional <- function(beta, a_lambda, b_lambda) {
  a_lambda_prime <- a_lambda + 0.5 * p
  b_lambda_prime <- b_lambda + 0.5 * sum(beta^2)
  lambda_sample <- rgamma(1, shape = a_lambda_prime, rate = b_lambda_prime)
  return(lambda_sample)
}

# Actualizar el código de Muestreo de Gibbs
Gibbs <- function(B, Calen, ampli, Y, X, Bo, nu0, s20, a_lambda, b_lambda) {
  # almacenamiento
  MC <- matrix(data = NA, nrow = (B - Calen) / ampli, ncol = ncol(X) + 2)
  LL <- matrix(data = NA, nrow = (B - Calen) / ampli, ncol = 1)
  # Muestro sistemÃ¡tico
  index <- seq(from = 1, to = B, by = ampli)
  ind <- 1
  # cadena
  sigma_squared <- s20  # inicialización de sigma_squared
  for (b in 1:B) {
    # actualizar lambda
    lambda <- sample_lambda_conditional(Bo, a_lambda, b_lambda)
    # actualizar beta
    beta <- sample_beta_conditional(lambda, sigma_squared, X, Y)
    # actualizar sigma^2
    sigma_squared <- sample_sigma_squared_conditional(lambda, beta, nu0, s20, X, Y)
    # almacenar valores
    if ((b - Calen) %in% index) {
      MC[ind, ] <- c(beta, sigma_squared, lambda)
      LL[ind] <- sum(dnorm(x = Y, mean = X %*% beta, sd = sqrt(sigma_squared), log = TRUE))
      ind = ind + 1
    }
  }
  # salida
  MC <- as.data.frame(MC)
  LL <- as.data.frame(LL)
  colnames(MC) <- c(paste0("beta", 1:ncol(X)), "sigma_squared", "lambda")
  colnames(LL) <- c("logV")
  return(list(BETA = MC, LL = LL))
}
set.seed(1)
MC4 <- Gibbs(B = 21000, Calen = 1000, ampli = 2, Y = Y.train, X = X.train, Bo = Beta_ols, nu0 = nu0, s20 = s20, a_lambda = a_lambda, b_lambda = b_lambda)
plot(1:10000, MC4$LL$logV, pch = 20, ylab = 'Log versomilitud',
     xlab = 'Iteraciones', main = 'Regresión rígida')
#Convergencia
CVMC4 <- apply(MC4$MC, 2, CVMC)
#Estimación de y.test
Yhat3.test <- X.test%*%as.matrix(colMeans(MC4[,1:64]))

#Error absoluto medio
Epsilon3 <- mean(abs(Y.test - Yhat3.test))

#Gráfica Y.test vs ^Y.test
g3 <- ggplot(data = data.frame(cbind(Y.test, Yhat3.test))) +
  geom_point(aes(x = X1, y = X2), color ='blue') + 
  labs(y = expression(widehat(Y)[test]), x = expression(Y[test]), 
       title = paste('Regresión rígida', '\n', paste0('EAM: ', round(Epsilon4, 3)))) +
  geom_abline(intercept = 0, slope =1, color='red', linewidth = 1) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)) +
  scale_x_continuous(breaks = seq(from = -1.5, to = 2, by = 0.5)) +
  scale_y_continuous(breaks = seq(from = -1.5, to = 3, by = 0.5))

#Bondad de ajuste
BA4 <- matrix(NA, nrow = 10000, ncol = 1)
for (i in 1:10000){
  y <- rmvnorm(n = 1, mean = X.train%*%matrix(as.numeric(MC4[i,1:64])), sigma = diag(as.numeric(MC4$MC[i,65]), n.train, n.train))
  BA4[i,1] <- mean(c(y))
}

#DIC
pDIC3 <- 2*(dmvnorm(x = drop(Y.train), mean = drop(X.train%*%matrix(as.numeric(colMeans(MC4[,1:64])))), sigma = diag(as.numeric(mean(MC4[,65])), n.train, n.train), log = TRUE) -
             mean(LL4[,1]))

DIC3 <- -2*dmvnorm(x = drop(Y.train), mean = drop(X.train%*%matrix(colMeans(MC4[,1:64]))), sigma = diag(mean(MC4[,65]), n.train, n.train), log = TRUE) +
  2*pDIC3


}
#Regresion errores correlacionados
{
#Previa
nu0 <- 1
s20 <- Sig2_ols
a <- 0
b <- 1
tao2 <- 50
Sigma_0 <- diag(1/tao2, nrow = p, ncol = p)

#Base para la construcción de la matriz de correlacion
D <- abs(outer( (1:n.train),(1:n.train) ,"-"))
delta <- 0.35
#Valores inciales
fit <- lm(Y.train ~ -1 + X.train)
Beta <- fit$coef
S2 <- summary(fit)$sigma^2
set.seed(1)
rho <- runif(1)
B <- 21000
C1 <- 1000
A <- 2
ac <- 0
MC5 <- matrix(NA, nrow = (B-C1)/A, ncol = p+2)
LL5 <- matrix(NA, nrow = (B-C1)/A, ncol = 1)
index <- seq(from = 1, to = B, by = A)
ind <- 1
set.seed(123)
for (b in 1:B){
  #Simulación de beta
  C <- rho^D
  inv.C <- solve(C)
  #Varianza de beta 
  V.beta <- solve(Sigma_0 + 1/S2*t(X.train)%*%inv.C%*%X.train)
  #Media de beta
  E.beta <- V.beta%*%(1/S2*t(X.train)%*%inv.C%*%Y.train)
  Beta <- t(rmvnorm(n = 1, mean = E.beta, sigma = V.beta))
  
  #Simular sigma^2
  S2 <- 1/rgamma(n = 1, shape = 0.5*(nu0 + n.train), 
                 rate = 0.5*(nu0*s20 + t(Y.train - X.train%*%Beta)%*%inv.C%*%(Y.train - X.train%*%Beta)))
  
  #Simulacion de rho - metropolis
  
  #Propuesta
  rho.p <- abs(runif(n=1, min = rho - delta, rho + delta))
  rho.p <- min(rho.p, 2-rho.p)
  
  #Tasa de aceptacion
  log.r <- -0.5*((determinant(rho.p^D,log=TRUE)$mod - determinant(rho^D,log=TRUE)$mod) + 
                  tr((Y.train-X.train%*%Beta)%*%t(Y.train-X.train%*%Beta)%*%(solve(rho.p^D) - solve(rho^D)))/S2)
  if (runif(1) < exp(log.r)){
    rho <- rho.p
    ac <- ac + 1
  }
  
  #Almacenamiento
  if ((b-C1) %in% index){
    MC5[ind, ] <- c(c(Beta), S2, rho)
    #Log versomilitud
    LL5[ind, ] <- sum(dmvnorm(x = drop(Y.train), mean = drop(X.train%*%Beta), sigma = S2*(rho^D), log = TRUE))
    ind <- ind + 1
  }
}
#----Diagnóstico de convergencia-----
EMC5 <- apply(MC5, 2, EMC)
CVMC5 <- apply(MC5, 2, CVMC)
neff5 <- effectiveSize(MC5)
plot(1:10000, LL5[,1], pch = 20, xlab = 'Iteraciones',
     ylab = 'Log Verosimiltiud', main = 'Errores correlacionados', col ='red')

Yhat4.test <- X.test%*%as.matrix(colMeans(MC5[,1:64]))
Epsilon4 <- mean(abs(Y.test - Yhat5.test))
g4 <- ggplot(data = data.frame(cbind(Y.test, Yhat4.test))) +
  geom_point(aes(x = X1, y = X2), color ='blue') + 
  labs(y = expression(widehat(Y)[test]), x = expression(Y[test]), 
       title = paste('Errores Correlacionados', '\n', paste0('EAM: ', round(Epsilon4, 3)))) +
  geom_abline(intercept = 0, slope =1, color='red', linewidth = 1) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 13),
        axis.title.y = element_text(size = 13)) +
  scale_x_continuous(breaks = seq(from = -1.5, to = 2, by = 0.5)) +
  scale_y_continuous(breaks = seq(from = -1.5, to = 3, by = 0.5))



#----Bondad de ajuste----
BA5 <- matrix(NA, nrow = 10000, ncol = 1)
set.seed(123)
for (i in 1:10000){
  y <- rmvnorm(n = 1, mean = X.train%*%matrix(MC5[i,1:64]), sigma = MC5[i,65]*(MC5[i,66]^D))
  BA5[i, ] <- mean(c(y))
}

#----DIC-----

pDIC5 <- 2*sum(dmvnorm(x = drop(Y.train), mean = drop(X.train%*%matrix(colMeans(MC5[,1:64]))),
                    sigma = mean(MC5[,65])*(mean(MC5[,66])^D), log = TRUE) -
             mean(LL5[,1]))

DIC5 <- -2*dmvnorm(x = drop(Y.train), 
                   mean = drop(X.train%*%matrix(colMeans(MC5[,1:64]))), 
                   sigma = mean(MC5[,65])*(mean(MC5[,66])^D), log = TRUE) +
  2*pDIC5
}

#----Graficas del CVMC----
t <- data.frame('CVMC' = c(CVMC2, CVMC4, CVMC5), Tipo = rep(c('Previa U', 'Rígida','E. Corr.'), c(65,66,66)))
ggplot(t, aes(x = Tipo, y = CVMC)) + 
  labs(x = '', y = '% CVMC', title = 'CVMC') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_boxplot(geom = "errorbar", width = 0.15) +
  geom_boxplot(fill = 'blue', alpha = 0.7, outlier.colour = 'red', width = 0.5) +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.text.y =  element_text(face = 'bold', size = 13)) +
  scale_x_discrete(limits = c("E. Corr.",  "Rígida", "Previa U")) +
  coord_flip()
#----Y.test vs Yhat.test----
g <- grid.arrange(g1, g2, g3, g4, ncol = 2)

#----Graficas bondad de ajuste----
par(mfrow = c(2,2))
#Unitaria
hist(BA1[,1], freq = FALSE, col = 'gray', border = 'gray', breaks = 30,
     main = paste0('Previa Unitaria', '\n', 'PPP: ', 
                   round(mean(BA1[,1] < mean(Y.train)), 3)), 
     xlab = expression(bar(Y)), 
     ylab = '', ylim = c(0, 12), xlim = c(-0.2, 0.2))
axis(1)
axis(2)
lines(density(BA1[,1]), lwd = 2, col = 'red')
abline(v = c(mean(Y.train), quantile(BA1[,1], c(0.025, 0.975))), lwd = 2, 
       col = c('blue', 'green', 'green'),lty = c(1, 2, 2))
legend(x = 'topright',legend = c('Densidad', 'IC 95%', 'Est. Obser.'),
       lty = c(1,2,1), lwd = c(2,2,2), col = c('red', 'green', 'blue'),
       bty = 'n')

#previa g
hist(BA2[,1], freq = FALSE, col = 'gray', border = 'gray', breaks = 30, axes = FALSE,
     main = paste0('Previa g', '\n', 'PPP: ', 
                   round(mean(BA2[,1] < mean(Y.train)), 3)), 
     xlab = expression(bar(Y)), 
     ylab = '', ylim = c(0, 12), xlim = c(-0.2, 0.2))
axis(1)
axis(2)
lines(density(BA2[,1]), lwd = 2, col = 'red')
abline(v = c(mean(Y.train), quantile(BA2[,1], c(0.025, 0.975))), lwd = 2, 
       col = c('blue', 'green', 'green'),lty = c(1, 2, 2))
legend(x = 'topright',legend = c('Densidad', 'IC 95%', 'Est. Obser.'),
       lty = c(1,2,1), lwd = c(2,2,2), col = c('red', 'green', 'blue'),
       bty = 'n')

#Errores correlacionados
hist(BA4[,1], freq = FALSE, col = 'gray', border = 'gray', breaks = 30, axes = FALSE,
     main = paste0('Rígida', '\n', 'PPP: ', 
                   round(mean(BA4[,1] < mean(Y.train)), 3)), 
     xlab = expression(bar(Y)), 
     ylab = '', ylim = c(0, 12), xlim = c(-0.2, 0.2))
axis(1)
axis(2)
lines(density(BA4[,1]), lwd = 2, col = 'red')
abline(v = c(mean(Y.train), quantile(BA4[,1], c(0.025, 0.975))), lwd = 2, 
       col = c('blue', 'green', 'green'),lty = c(1, 2, 2))
legend(x = 'topright',legend = c('Densidad', 'IC 95%', 'Est. Obser.'),
       lty = c(1,2,1), lwd = c(2,2,2), col = c('red', 'green', 'blue'),
       bty = 'n')

#Errore correlacionados
hist(BA5[,1], freq = FALSE, col = 'gray', border = 'gray', breaks = 30, axes = FALSE,
     main = paste0('Errores correlacionados', '\n', 'PPP: ', 
                   round(mean(BA5[,1] < mean(Y.train)), 3)), 
     xlab = expression(bar(Y)), 
     ylab = '', ylim = c(0, 12), xlim = c(-0.2, 0.2))
axis(1)
axis(2)
lines(density(BA5[,1]), lwd = 2, col = 'red')
abline(v = c(mean(Y.train), quantile(BA5[,1], c(0.025, 0.975))), lwd = 2, 
       col = c('blue', 'green', 'green'),lty = c(1, 2, 2))
legend(x = 'topright',legend = c('Densidad', 'IC 95%', 'Est. Obser.'),
       lty = c(1,2,1), lwd = c(2,2,2), col = c('red', 'green', 'blue'),
       bty = 'n')
par(mfrow = c(1,1))
