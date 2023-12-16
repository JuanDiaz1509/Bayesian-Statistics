#Limpieza del entorno
rm(list = ls())

#Directorio de trabajo
setwd('C:/Users/Lab Datos/Downloads/Caso 2')

#Librerías
library(readxl) #Imprtancion de DB
library(readr)
library(dplyr) #Manipulacion de DB
library(ggplot2) #Gráficar
library(sf) #Manejo de archivos .shp
library(stringr) #Manipulacion de caracteres
library(patchwork) #Unión de gráficos en paneles
library(coda) #Tamaños efectivos de muestra 
library(RColorBrewer) #colores mapas
library(glmtoolbox) #R2adj
library(corrplot) #Matriz de incidencia
library(reshape2) #melt para formato long
 
#----Tratamiento de los datos---- 
data <- read.csv('SB11_20222.TXT', sep=";")
data <- data %>%
  filter(ESTU_NACIONALIDAD == 'COLOMBIA' & ESTU_ESTADOINVESTIGACION == 'PUBLICAR'
         & ESTU_PAIS_RESIDE == 'COLOMBIA', COLE_COD_DEPTO_UBICACION != 88) %>%
  rename(Dep = COLE_COD_DEPTO_UBICACION, Depto = COLE_DEPTO_UBICACION, 
         Depmun = COLE_COD_MCPIO_UBICACION, Mcpio = COLE_MCPIO_UBICACION,
         Puntaje = PUNT_GLOBAL) %>%
  mutate(Dep = as.character(Dep), Depmun = as.character(Depmun)) %>%
  select(Dep, Depto, Depmun, Mcpio, Puntaje) %>%
  as.data.frame()

data$Dep[data$Dep == '5'] <- '05'
data$Dep[data$Dep == '8'] <-'08'
data$Depmun3 <- paste0(data$Dep, str_sub(data$Depmun,-3))
data <- data %>% arrange(Depmun3) %>% as.data.frame()
#Conjunto de datos agrupado por departamento
data1 <- data %>%
  group_by(Dep, Depto) %>%
  summarise(
    Media = mean(Puntaje),
    S2 = var(Puntaje),
    NJ = n(),
    nMun = n_distinct(Mcpio)
  ) %>%
  mutate(Depto = str_to_title(chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", Depto))) %>%
  select(Dep, Depto, Media, S2, NJ, nMun) %>%
  arrange(Dep) %>% #Ordenamiento de acuerdo al codigo DANE
  as.data.frame()

#Conjunto de datos agrupado por municipio
data2 <- data %>%
  group_by(Depmun3, Mcpio) %>%
  summarise(
    Media = mean(Puntaje),
    S2 = var(Puntaje),
    njk = n(),
    Min = min(Puntaje),
    Max = max(Puntaje),
    Mediana = median(Puntaje),
    RIC = IQR(Puntaje)
  ) %>%
  select(Depmun3, Mcpio, Media, S2, njk, Min, Max, Mediana, RIC) %>%
  arrange(Depmun3) %>%
  as.data.frame()
data2 <- within(data2,{
  Dep = str_sub(Depmun3, 1, 2)
})

#----Funciones previas----
EMC <- function(x){
  #Error de montecarlo
  neff <- effectiveSize(x)
  return(sd(x)/sqrt(neff))
}
CVMC <- function(x){
  #cv de monte carlo
  cv <- 100*EMC(x)/mean(x)
  return(cv)
}
cv <- function(x){
  #Coeficiente de variacion
  return(100*abs(sd(x)/mean(x)))
}


#----Pregunta 1----
pobreza <- read_excel('pobreza monetaria.xls', sheet = 2)
pobreza <- pobreza[-c(1:10, 35:43), c(1,16)]
colnames(pobreza) <- c('Depto', 'Ano')
pobreza$Depto[pobreza$Depto == 'Bogotá D.C.'] <- 'Bogotá'
pobreza$Depto[pobreza$Depto == 'Valle del Cauca'] = 'Valle'
pobreza$Depto[pobreza$Depto == 'Norte de Santander'] = 'Norte Santander'
pobreza <- within(pobreza,{
  dpto <- str_to_lower(Depto)
  dpto <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", dpto)
  dpto <- str_to_title(dpto)
  Depto <- dpto
  rm(dpto)
})
pobreza <- pobreza %>% left_join(data1, by = 'Depto') %>%
  select(Dep, Depto, Ano) %>%
  arrange(Dep) %>%
  as.data.frame()

shp <- st_read('MGN_DPTO_POLITICO.shp', quiet = TRUE)

gg1 <- data1 %>% inner_join(shp, by = c("Dep" = "DPTO_CCDGO")) %>%
  select(Dep, Depto, Media, geometry) %>%
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = Media)) + 
  geom_sf_text(aes(label=ifelse(Media < 240,str_to_title(Depto),"")),col="black",
               fontface="bold", size=3,fun.geometry=function(x) sf::st_centroid(x)) +
  labs(title = 'Puntajes globales ICFES', x = 'Longitud',
       y = 'Latitud') + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) + 
  scale_fill_gradientn(colours = brewer.pal(n = 5, name = "Blues"), 
                       n.breaks=5)

#Limpiando para unir con pobreza por nombre ya que no se tiene el COD del DEPTO
shp2 <- within(shp,{
  dpto <- str_to_lower(DPTO_CNMBR)
  dpto <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", dpto)
  dpto <- str_replace_all(dpto, ",", "")
})
shp2 <- shp2 %>%
  filter(DPTO_CCDGO != '88') %>%
  select(DPTO_CCDGO, dpto, geometry)

#limpiando pobreza
pobreza2 <- within(pobreza,{
  dpto <- str_to_lower(Depto)
  dpto <- chartr("áéíóúÁÉÍÓÚ", "aeiouAEIOU", dpto)
  dpto <- str_replace_all(dpto, ",", "")
})

#Uniendo
gg2 <- shp2 %>% left_join(pobreza2, by = c('dpto' = 'dpto')) %>%
  select(DPTO_CCDGO, dpto, geometry, Ano) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = Ano)) + 
  labs(title = 'Pobreza monetaria 2018', x = 'Longitud',
       y = 'Latitud', fill = 'Incidencia') + 
  geom_sf_text(aes(label=ifelse(Ano > 40, str_to_title(dpto),"")),col="black",
               fontface="bold", size=3,fun.geometry=function(x) sf::st_centroid(x)) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) + 
  scale_fill_gradientn(colours = brewer.pal(n = 5, name = "Reds"), 
                       n.breaks=5)
map1 <- gg1 + gg2 + plot_layout(ncol = 2)
print(map1)
rm(gg1, gg2, shp2, pobreza2, shp)

#----Pregunta2----
#ICFES
shpMun <- st_read('MGN_MPIO_POLITICO.shp', quiet = TRUE)
shpMun <- shpMun %>% filter(DPTO_CCDGO != '88')
p2.1 <- right_join(data2, shpMun, by = c('Depmun3'='MPIO_CDPMP')) %>%
  filter(DPTO_CCDGO != 88) %>%
  select(Depmun3, Mcpio, Media, geometry) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = Media)) +
  labs(title = 'Puntajes globales por Muncipio ICFES', x = 'Longitud',
                                   y = 'Latitud') + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) + 
  scale_fill_gradientn(colours = brewer.pal(n = 5, name = "Blues"), 
                       n.breaks=5)

#Cobertura neta secundaria
EstEdu <- read_csv('estadísticas educación.csv')
EstEdu <- EstEdu %>%
  filter(AÑO == 2022 & str_sub(CÓDIGO_MUNICIPIO, 1, 2) != '88') %>%
  select(AÑO, CÓDIGO_MUNICIPIO, MUNICIPIO, COBERTURA_NETA_SECUNDARIA) %>%
  arrange(CÓDIGO_MUNICIPIO) %>%
  as.data.frame()
p2.2 <- inner_join(EstEdu, shpMun, by =c('CÓDIGO_MUNICIPIO' = 'MPIO_CDPMP')) %>%
  filter(DPTO_CCDGO != 88) %>%
  select(COBERTURA_NETA_SECUNDARIA, geometry) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = COBERTURA_NETA_SECUNDARIA)) +
  labs(title = 'Cobertura neta secundaria', x = 'Longitud',
       y = 'Latitud', fill = 'Cobertura') + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) + 
  scale_fill_gradientn(colours = brewer.pal(n = 5, name = "Reds"), 
                       n.breaks=5)

map2 <- p2.1 + p2.2 + plot_layout(ncol = 2)
print(map2)
rm(shpMun, p2.1, p2.2, map2)

#----Pregunta 4----

#Modelo 1
{
#Estadísticas suficientes
n <- nrow(data)
#Promedio de cada departamento
media <- mean(data1$Media)
std <- sd(data1$Puntaje)
sumy <- sum(data1$Puntaje)

#Hiperparámetros
mu0 <- 250
gama20 <- 50^2
nu0 = 1
s20 = 50^2
#Calentamiento
C <- 1000
B <- 100000 + C

#Matrix de montecarlo
MC1 <- as.data.frame(matrix(NA, nrow = (B-C)/10, ncol = 2))
colnames(MC1) <- c('Theta', 'Sigma2')
#Establezco valor inicial, en este caso el valor inicial es sigma
set.seed(20)
sigI <- 1/rgamma(n = 1, nu0/2, nu0*s20/2)
#Vector de las posiciones de muestreo sistemtico
index <- seq(from = 1, to = B, by = 10)
#indice
ind <- 1
set.seed(20)
for (b in 1:B){
  #Actualización de theta
  gama2n <- 1/((1/gama20) + (n/sigI))
  mun <- ((mu0/gama20) + (sum(data$Puntaje)/sigI))*gama2n
  theta <- rnorm(n = 1, mean = mun, sd = sqrt(gama2n))
  #Actualización de sigma
  nun <- nu0+n
  s2n <- (nu0*s20) + sum((data$Puntaje - theta)^2)
  sigI <- 1/rgamma(n = 1, shape = nun/2, rate = s2n/2)
  #Muestreo sistematico y exclución de las muestras de calentamiento
  if ((b-C) %in% index){
    MC1[ind,] <- c(theta, sigI)
    ind = ind + 1
  }
}
write_xlsx(MC1, 'MC1.xlsx')

#Validación de la cadena
par(mfrow = c(1,2))
plot(1:nrow(MC1), MC1$Theta, xlab = 'Iteraciones', ylab = expression(theta),
     main = 'Convergencia de la cadena', pch = 20 , col = '#3282F6')
plot(1:nrow(MC1), MC1$Sigma2, xlab = 'Iteraciones', ylab = expression(sigma^2),
     main = 'Convergencia de la cadena', pch = 20, col = '#3282F6')
par(mfrow = c(1,1))
#Logaritmo de la log-verosimilitus
LL <- NULL
for (i in 1:nrow(MC1)) {
  LL[i] <- sum(dnorm(x = data$Puntaje, mean = MC1$Theta[i], sd = sqrt(MC1$Sigma2[i]), log = TRUE))
}
LL <- as.data.frame(LL)
write_xlsx(LL, 'LogVeroM1.xlsx')
plot(1:nrow(MC1), LL, xlab = 'Iteraciones', ylab = 'Log-Verosimilitud')

#Análisis de cada de monte carlo
#Anpalisis de autocorrelación
par(mfrow = c(1,2))
acf(MC1$Theta, main =expression(theta))
acf(MC1$Sigma2, main = expression(sigma^2))
par(mfrow = c(1,1))
#Tamaño de muestra efectivo
neff1 <- effectiveSize(MC1)
neff1
#Error de monte carlo
EMC1 <- apply(MC1, 2, EMC)
#Coeficiente de variacion de MC
CVMC1 <- apply(MC1, 2, CVMC)

#Inferencias Modelo 1
Inf1 <- MC1 %>% select(Theta, Sigma2) %>%
  pivot_longer(cols = c(Theta, Sigma2), names_to = "name", values_to = "value") %>%
  group_by(name) %>%
  summarise(Media = mean(value), 
            cv = cv(value), 
            IC_inf = quantile(value, 0.025), 
            IC_sup = quantile(value, 0.975)) %>%
  mutate(across(where(is.numeric), ~ round(., 3))) %>%
  as.data.frame()
}

#Modelo 2 
{
Gibbs <- function(B, Calen, ampli, nj, yb, s2, mu0, g20, eta0, t20, nu0, s20) {
  #Datos crudos
  y <- data$Puntaje
  # tamaños
  n <- sum(nj)
  m <- length(nj)
  # valores iniciales
  theta <- yb
  sig2  <- mean(s2)
  mu    <- mean(theta)
  tau2  <- var(theta)
  # almacenamiento
  MC <- matrix(data = NA, nrow = (B-Calen)/ampli, ncol = m+3)
  LL <- matrix(data = NA, nrow = (B-Calen)/ampli, ncol = 1)
  #Muestro sistemático
  index <- seq(from = 1, to = B, by = ampli)
  ind <- 1
  # cadena
  for (b in 1:B) {
    # actualizar theta
    vtheta <- 1/(1/tau2 + nj/sig2)
    theta  <- rnorm(n = m, mean = vtheta*(mu/tau2 + nj*yb/sig2), sd = sqrt(vtheta))
    # actualizar sigma^2
    sig2 <- 1/rgamma(n = 1, shape = 0.5*(nu0 + n), rate = 0.5*(nu0*s20 + sum((nj-1)*s2 + nj*(yb - theta)^2)))
    # actualizar mu
    vmu <- 1/(1/g20 + m/tau2)
    mu  <- rnorm(n = 1, mean = vmu*(mu0/g20 + sum(theta)/tau2), sd = sqrt(vmu)) 
    # actualizar tau^2
    tau2 <- 1/rgamma(n = 1, shape = 0.5*(eta0 + m), rate = 0.5*(eta0*t20 + (m-1)*var(theta) + m*(mean(theta) - mu)^2))
    # almacenar valores
    if ((b-Calen) %in% index){
      MC[ind,] <- c(theta, sig2, mu, tau2)
      LL[ind] <- sum(dnorm(x = y, mean = rep(theta, nj), sd = sqrt(sig2), log = T))
      ind = ind + 1
    }
  }
  # salida
  MC <- as.data.frame(MC)
  LL <- as.data.frame(LL)
  colnames(MC) <- c(paste0("theta",1:m), "sig2", "mu", "tau2")
  colnames(LL) <- c("logV")
  return(list(THETA = MC, LL = LL))
}
set.seed(20)
MC2 <- Gibbs(B=101000, Calen=1000, ampli = 10, nj=data1$NJ, yb=data1$Media, s2=data1$S2, mu0 = 250, 
           g20 = 50^2, eta0 = 1, t20 = 50^2, 
           nu0 = 1, s20 = 50^2)
write_xlsx(MC2$THETA, 'MC2.xlsx')
write_xlsx(MC2$LL, 'LogVeroM2.xlsx')
#Validación de convergencia
par(mfrow = c(1,2))
plot(1:10000, MC2$LL$logV, xlab = 'Iteraciones', ylab = 'Log-verosimilitud', main = 'Modelo 2')
plot(1:10000, MC2$THETA$sig2, xlab = 'Iteraciones', ylab = expression(sigma^2))
par(mfrow = c(1,1))

#Tamaños efectos de muestra
neff2 <- effectiveSize(MC2$THETA)
summary(neff2[1:m]) #Tamaños efectivos de muestra de los departamentos
neff2[m+(1:3)] #Tamaño de muestra para los otros parametros
plot(1:ncol(MC2$THETA), neff2, main = 'Tamaños efectivos de muestra',
     xlab = expression(bold(Theta)), ylab = 'NEFF', pch = 20)
abline(a=10000, b=0, col = 'red', lwd = 2)

#Error de MC
EMC2 <- apply(MC2$THETA, 2, EMC)
summary(EMC2[1:m])
EMC2[m+(1:3)]
#Coeficiente de CV de MC
CVMC2 <- apply(MC2$THETA, 2, CVMC)
summary(CVMC2[1:m])
CVMC2[m+(1:3)]

#Inferencia Modelo 2

}

#Modelo 3 
{
Gibbs2 <- function(B, Calen, Ampli, nj, yb, s2, mu0, g20, eta0, t20, nu, al0, be0) {
  #Datos en crudo
  y <- data$Puntaje
  # tamaños
  n <- sum(nj)
  m <- length(nj)
  # valores iniciales
  theta <- yb
  sig2_j  <- s2  # sigma_j^2
  mu    <- mean(theta)
  tau2  <- var(theta)
  sig2  <- 100  # sigma^2
  # almacenamiento
  MC <- matrix(data = NA, nrow = (B-Calen)/Ampli, ncol = 2*m+3)
  LL    <- matrix(data = NA, nrow = (B-Calen)/Ampli, ncol = 1)
  #Muestreo sistemático
  index <- seq(from = 1, to = B-Calen, by = Ampli)
  ind <- 1
  # cadena
  for (b in 1:B) {
    # actualizar theta
    vtheta <- 1/(1/tau2 + nj/sig2_j)
    theta  <- rnorm(n = m, mean = vtheta*(mu/tau2 + nj*yb/sig2_j), sd = sqrt(vtheta))
    # actualizar sigma_j^2
    sig2_j <- 1/rgamma(n = m, shape = 0.5*(nu + nj), rate = 0.5*(nu*sig2 + (nj-1)*s2 + nj*(yb - theta)^2))
    # actualizar mu
    vmu <- 1/(1/g20 + m/tau2)
    mu  <- rnorm(n = 1, mean = vmu*(mu0/g20 + sum(theta)/tau2), sd = sqrt(vmu))
    # actualizar tau2
    tau2 <- 1/rgamma(n = 1, shape = 0.5*(eta0+m), rate = 0.5*(eta0*t20 + (m-1)*var(theta) + m*(mean(theta) - mu)^2))
    # actualizar sigma^2
    sig2 <- rgamma(n = 1, shape = 0.5*(al0 + m*nu), rate = 0.5*be0 + 0.5*nu*sum(1/sig2_j))
    # almacenar
    if ((b-Calen) %in% index){
      MC[ind,] <- c(theta, sig2_j, mu, tau2, sig2)
      # log-verosimilitud
      LL[ind] <- sum(dnorm(x = y, mean = rep(theta, nj), sd = sqrt(rep(sig2_j, nj)), log = T))
      ind <- ind + 1
    }
  }
  # fin de la cadena
  # salida
  colnames(MC) <- c(paste0("theta", 1:m), paste0("sig2_j", 1:m), "mu", "tau2", "sig2")
  colnames(LL) <- c("logV")
  MC <- as.data.frame(MC)
  LL    <- as.data.frame(LL)
  return(list(MC = MC, LL = LL))
}
set.seed(20)
MC3 <- Gibbs2(B = 101000, Calen = 1000, Ampli = 10, nj = data1$NJ, yb = data1$Media, s2 = data1$S2, 
            mu0 = 250, g20 = 50^2, eta0 = 1, t20 = 50^2, nu = 1, al0 = 1, be0 = 1/50^2)
write_xlsx(MC3$MC, 'MC3.xlsx')
write_xlsx(MC3$LL, 'LogVeroM3.xlsx')
#Validación de la convergencia - Logverosimilitud
plot(1:nrow(MC3$MC), MC3$LL$logV, xlab = 'Iteraciones', ylab = 'Log-verosimilitud', main = 'Modelo 3', pch = 20)
#Tamaños efectivos de muestra
neff3 <- effectiveSize(MC3$MC)
summary(neff3[1:m]) #Resumen de neff de los promedios
summary(neff3[m+1:2*m]) #Resumen de neff de los varianzas
neff3[65:67] #neff para mu y tao y sigma

#Errores de MC
EMC3 <- apply(MC3$MC, 2, EMC)
summary(EMC3[1:m])
summary(EMC3[m+1:2*m])
EMC3[65:67]

#CV de MC
CVMC3 <- apply(MC3$MC, 2, CVMC)
summary(CVMC3[1:m])
summary(CVMC3[m+1:2*m])
CVMC3[65:67]

#Inferencias Modelo 3
}

#Modelo 4 
{
d <- nrow(data2)
m <- nrow(data1)
Gibbs4 <- function(B, Calen, Ampli, nk, njk, yb_k, yb_jk, xi0, k20, mu0, g20, eta0, t20, nu0, sig20){
  # #Cantidades fijas
  # m #número de departamento
  # d #número de municipios
  # nk #número mcpio en el depto k
  # njk #número de estudiantes por muncipio j y departamento k- vector
  # yb_k #promedio de los puntajes por departametno
  # yb_jk #promedio de los punatajes en el municpio j, depto k - vector
  y <- data$Puntaje
  n <- sum(njk)
  m <- length(nk)
  d <- length(njk)
  
  #Almacenamiento
  MC <- as.data.frame(matrix(data = NA, nrow = (B-Calen)/Ampli, ncol = d+m+4))
  LL <- as.data.frame(matrix(data = NA, nrow = (B-Calen)/Ampli, ncol = 1))
  #Nombres de las variables
  names <- NULL
  for (k in 1:length(nk)) {
    for (j in 1:nk[k]) {
      names <- c(names, paste0('zeta', j, '_', k))
    }
  }
  #Muestreo sistematico
  index <- seq(from = 1, to = (B-Calen), by = Ampli)
  ind <- 1
  #Valores inciales
  theta <- yb_k
  mu <- mean(theta)
  tau2 <- var(yb_k)
  sig2 <- var(yb_jk)
  kapa2 <- var(y)
  zeta <- yb_jk
  factor <- rep(1:length(nk), nk)
  for (b in 1:B){
    #Medias de los municipios en el depto k
    zb_k <- as.vector(tapply(zeta, factor, sum)) #promedio del depto k
    
    #Actualización de theta_k
    vtheta <- 1/(nk/sig2 + 1/tau2)
    theta <- rnorm(n = m,  mean = (zb_k/sig2 + mu/tau2)*vtheta, 
                   sd = sqrt(vtheta))
    #Actualización de tau^2
    tau2 <- 1/rgamma(n = 1, shape = 0.5*(eta0 + m), 
                     rate = 0.5*(eta0*t20 + sum((theta - mu)^2)))
    #Actualización de mu
    vmu <- 1/(m/tau2 + 1/g20)
    mu <- rnorm(n = 1, mean = (sum(theta)/tau2 + mu0/g20)*vmu, 
                sd = sqrt(vmu))
    #Actualización de zeta_jk
    vzeta <- 1/(njk/kapa2 + 1/sig2)
    zeta <- rnorm(n = d, mean = (njk*yb_jk/kapa2 + rep(theta, nk)/sig2)*vzeta,
                  sd = sqrt(vzeta))
    #Actualización de kappa^2
    kapa2 <- 1/rgamma(n = 1, shape = (xi0 + n)*0.5, rate = 0.5*(xi0*k20 + sum((y - rep(zeta, njk))^2))) 
    #Actualización de sigma^2
    sig2 <- 1/rgamma(n = 1, shape = (nu0+d)*0.5, rate = 0.5*(nu0*sig20 + sum((zeta - rep(theta, nk))^2)))
    
    #Muestreo
    if ((b-Calen) %in% index){
      MC[ind,] <- c(zeta, theta, kapa2, mu, tau2, sig2)
      LL[ind,] <- sum(dnorm(x = y, mean = rep(zeta, njk), sd = sqrt(kapa2), log = T))
      ind <- ind + 1
    }
  }
  colnames(MC) <- c(names, paste0("theta",1:m), 'Kappa2', 'mu','tau2', 'sigma2')
  colnames(LL) <- colnames(LL) <- c("logV")
  return(list(MC = MC, LL = LL))
}
tictoc::tic()
set.seed(20)
MC4 <- Gibbs4(B = 101000, Calen = 1000, Ampli = 10, nk = data1$nMun, njk = data2$njk,
              yb_k = data1$Media, yb_jk = data2$Media, xi0 = 1, k20 = 50^2, mu0 = 250, g20 = 50^2, 
              eta0 = 1, t20 = 50^2, nu0 = 1, sig20 = 50^2)
tictoc::toc()
write_xlsx(MC4$MC, 'MC4.xlsx')
write_xlsx(MC4$LL, 'LogVeroM4.xlsx')

plot(1:nrow(MC4$LL), MC4$LL$logV, pch =20, main = 'Modelo 4',
     xlab = 'Iteraciones', ylab = 'Log-Verosimilitud')
#Tamaños efectivos de muestras
neff4 <- effectiveSize(MC4$MC)
summary(neff4[1:d]) #neff para medias por municipio
summary(neff4[d+1:m])#neff para medias por depto
neff4[1145:1148] #neff kappa, mu, tau2, sig2

#Errores de MC
EMC4 <- apply(MC4$MC, 2, EMC)
summary(EMC4[1:d]) #EMC para medias por mcpio
summary(EMC4[d+1:m]) #EMC para medias por depto
EMC4[d+m+1:4]

#CV de montecarlo
CVMC4 <- apply(MC4$MC, 2, CVMC)
summary(CVMC4[1:d]) #CVMC para medias por mcpio
summary(CVMC4[d+1:m]) #CVMC para medias por depto
CVMC4[d+m+1:4]

#Inferencias Modelo 4
}

#Modelo 5
{
Gibbs5 <- function(B, Calen, Ampli, y, IP, q, S2k, nk, njk, yb_k, yb_jk, mu0, g20, eta0, t20, nu, al0, be0, xi0, k20){
  #IP en el incidencia de pobreza monetaria
  #Cantidades fijas
  n <- length(y)
  m <- length(nk)
  d <- length(njk)
  #Almacenamiento
  MC <- as.data.frame(matrix(data = NA, nrow = (B-Calen)/Ampli, ncol = d+3*m+4))
  LL <- as.data.frame(matrix(data = NA, nrow = (B-Calen)/Ampli, ncol = 1))
  REGRE <- as.data.frame(matrix(data = NA, nrow = (B-Calen)/Ampli, ncol = 3))
  
  #Nombres de zeta
  names <- NULL
  for (k in 1:length(nk)) {
    for (j in 1:nk[k]) {
      names <- c(names, paste0('zeta', j, '_', k))
    }
  }
  #Muestreo sistematico
  index <- seq(from = 1, to = (B-Calen), by = Ampli)
  ind <- 1
  #Valores inciales
  theta <- yb_k
  sig2_k <- S2k
  mu <- mean(theta)
  tau2 <- var(yb_k)
  sig2 <- var(yb_jk)
  kapa2 <- var(y)
  zeta <- yb_jk
  factor <- rep(1:length(nk), nk)
  for (b in 1:B){
    zb_k <- as.vector(tapply(zeta, factor, sum)) #Total del depto k
    
    vtheta <- 1/(nk/sig2_k + 1/tau2)
    theta <- rnorm(n = m, mean = (zb_k/sig2_k + mu/tau2)*vtheta, sd = sqrt(vtheta))
    
    #mu - media de la distribución de las medias por depto
    vmu <- 1/(m/tau2 + 1/g20)
    mu <- rnorm(n = 1, mean = (sum(theta)/tau2 + mu0/g20)*vmu, sd = sqrt(vmu))
    
    #tau2- es al varianza de la dsitribución de las medias por depto
    tau2 <- 1/rgamma(n = 1, shape = 0.5*(eta0+m), rate = 0.5*(eta0*t20 + sum((theta-mu)^2)))
    
    #zeta_jk - media por municipio
    vzeta <- 1/(njk/kapa2 + 1/rep(sig2_k, nk))
    zeta <- rnorm(n = d, mean = ((njk*yb_jk/kapa2) + (rep(theta, nk)/rep(sig2_k, nk)))*vzeta, sd = sqrt(vzeta))
    
    #sigma^2_k - es la varianza de cada departamento
    rate <- as.vector(tapply((zeta - rep(theta, nk))^2, INDEX = rep(1:m, nk), FUN = sum))
    sig2_k <- 1/rgamma(n = m, shape = 0.5*(nu+nk), rate = 0.5*(nu*sig2 + rate)) #revisa vectores
    
    #sigma - parametro de dispersión de la distribucion de sigma2_k
    sig2 <- rgamma(n = 1, shape = 0.5*(al0 + nu*m), rate = 0.5*(be0 + nu*sum(1/sig2_k)))
    
    #kappa - varianza de cada yijk
    kapa2 <- 1/rgamma(n = 1, shape = 0.5*(xi0 + n), rate = 0.5*(xi0*k20 + sum((y - rep(zeta, njk))^2)))
    
    #Almacenamiento
    if ((b-Calen) %in% index){
      clusters <- kmeans(theta, centers = q)
      MC[ind,] <- c(zeta, theta, sig2_k, mu, tau2, sig2, kapa2, clusters$cluster)
      LL[ind,] <- sum(dnorm(x = y, mean = rep(zeta, njk), sd = sqrt(kapa2), log = T))
      model <- lm(IP ~ theta[-c(25:32)])
      REGRE[ind,] <- c(model$coefficients, adjR2(model))
      ind <- ind + 1
    }
  }
  colnames(MC) <- c(names, paste0("theta",1:m), paste0('sig2_k', 1:m), 'mu','tau2', 'sigma2', 'Kappa2',
                    paste0("xi", 1:m))
  colnames(LL) <- c("logV")
  colnames(REGRE) <- c('beta0', 'beta1', 'R2adj')
  return(list(MC = MC, LL = LL, Regre = REGRE))
}
tictoc::tic()
set.seed(20)
MC5 <- Gibbs5(B = 101000, Calen = 1000, Ampli = 10, y = data$Puntaje, IP = pobreza$Ano, q=5,
              S2k = data1$S2, nk = data1$nMun, njk = data2$njk, yb_k = data1$Media, 
              yb_jk = data2$Media, mu0 = 250, g20 = 50^2, eta0 = 1, t20 = 50^2, nu = 1,
              al0 = 1, be0 = 1/50^2, xi0 = 1, k20 = 50^2)
tictoc::toc()
write_xlsx(MC5$MC, 'MC5.xlsx')
write_xlsx(MC5$LL, 'LogVeroM5.xlsx')
write_xlsx(MC5$Regre, 'RegresionM5.xlsx')
}

#Log verosmilitud
{
dev.new()
par(mfrow = c(2,2))
plot(1:10000, MC2$LL$logV, pch = 20, main = 'Modelo 2',
     xlab = 'Iteraciones', ylab = 'Log-Verosmilitud',
     ylim = c(-2797800, -2775300))
plot(1:10000, MC3$LL$logV, pch = 20, main = 'Modelo 3', 
     xlab = 'Iteraciones', ylab = 'Log-Verosmilitud',
     ylim = c(-2797800, -2775300))
plot(1:10000, MC4$LL$logV, pch = 20, main = 'Modelo 4',
     xlab = 'Iteraciones', ylab = 'Log-Verosmilitud',
     ylim = c(-2797800, -2775300))
plot(1:10000, MC5$LL$logV, pch = 20, main = 'Modelo 5',
     xlab = 'Iteraciones', ylab = 'Log-Verosmilitud',
     ylim = c(-2797800, -2775300))
par(mfrow = c(1,1))

# par(mfrow = c(2,2))
# plot(1:10000, MC2$LL$logV, pch = 20, main = 'Modelo 2',
#      xlab = 'Iteraciones', ylab = 'Log-Verosmilitud',
#      ylim = c(-2798000, -2797040))
# plot(1:10000, MC3$LL$logV, pch = 20, main = 'Modelo 3', 
#      xlab = 'Iteraciones', ylab = 'Log-Verosmilitud',
#      ylim = c(-2798000, -2797040))
# plot(1:10000, MC4$LL$logV, pch = 20, main = 'Modelo 4',
#      xlab = 'Iteraciones', ylab = 'Log-Verosmilitud',
#      ylim = c(-2775550, -2775300))
# plot(1:10000, MC5$LL$logV, pch = 20, main = 'Modelo 5',
#      xlab = 'Iteraciones', ylab = 'Log-Verosmilitud',
#      ylim = c(-2775550, -2775300))
# par(mfrow = c(1,1))
}


#-----Pregunta 5-----
n <- length(data$Puntaje)
nj <- data1$NJ # nj : número de estudiantes por departamento (c)
njk <- data2$njk # njk : número de estudiantes por municipios (c)
m <- length(data1$NJ) # m : número de grupos (departamentos)
d <- length(data2$njk) # d : número de grupos (municipios)
y <- data$Puntaje # y  : puntaje de los estudiantes (c)
Y <- vector(mode = "list", length = m) # Y  : puntaje de los estudiantes (list)
g <- rep(NA, n)
for (j in 1:m) {
  idx <- data1$Dep == unique(data1$Dep)[j]
  g[idx] <- j
  Y[[j]] <- y[idx]
}
h <- rep(NA, n)
for (j in 1:d) {
  idxh <- data2$Depmun3 == unique(data2$Depmun3)[j]
  h[idxh] <- j
  Y[[j]] <- y[idxh]
}
#DIC y WAIC
{
  # DIC1
  LP1        <- as.numeric(LL$LL)
  theta_hat  <- mean(MC1$Theta)
  sigma2_hat <- mean(MC1$Sigma2)
  lpyth_m1   <- sum(dnorm(x = y, mean = theta_hat, sd = sqrt(sigma2_hat), log = T))
  pDIC_m1    <- 2*(lpyth_m1 - mean(LP1));pDIC_m1
  dic_m1     <- -2*lpyth_m1 + 2*pDIC_m1 ;dic_m1
  # WAIC
  lppd_m1  <- 0
  pWAIC_m1 <- 0
  for (i in 1:n) {
    # lppd
    tmp1 <- dnorm(x = y[i], mean = MC1$Theta, sd = sqrt(MC1$Sigma2))
    lppd_m1 <- lppd_m1 + log(mean(tmp1))
    # pWAIC
    tmp2 <- dnorm(x = y[i], mean = MC1$Theta, sd =  sqrt(MC1$Sigma2), log = T)
    pWAIC_m1 <- pWAIC_m1 + 2*(log(mean(tmp1)) - mean(tmp2))
  }
  waic_m1 <- -2*lppd_m1 + 2*pWAIC_m1;
  
  # DIC 2
  LP2        <- as.numeric(unlist(MC2$LL$logV))
  theta_hat  <- colMeans(MC2$THETA[,1:m])
  sigma2_hat <- mean(MC2$THETA[,(m+1)])
  lpyth_m2   <- sum(dnorm(x = y, mean = rep(theta_hat, nj), sd = sqrt(sigma2_hat), log = T))
  pDIC_m2    <- 2*(lpyth_m2 - mean(LP2));pDIC_m2
  dic_m2     <- -2*lpyth_m2 + 2*pDIC_m2;dic_m2
  # WAIC
  lppd_m2  <- 0
  pWAIC_m2 <- 0
  for (i in 1:n) {
    # lppd
    tmp1.2 <- dnorm(x = y[i], mean = MC2$THETA[,g[i]], sd = sqrt(MC2$THETA$sig2))
    lppd_m2 <- lppd_m2 + log(mean(tmp1.2))
    # pWAIC
    tmp2.2 <- dnorm(x = y[i], mean = MC2$THETA[,g[i]], sd = sqrt(MC2$THETA$sig2), log = T)
    pWAIC_m2 <- pWAIC_m2 + 2*(log(mean(tmp1.2)) - mean(tmp2.2))
  }
  waic_m2 <- -2*lppd_m2 + 2*pWAIC_m2;waic_m2
  
  # DIC3
  LP3        <- as.numeric(MC3$LL$logV)
  theta_hat3  <- colMeans(MC3$MC[,1:m])
  sigma2_hat3 <- colMeans(MC3$MC[,(m+1):(2*m)])
  lpyth_m3   <- sum(dnorm(x = y, mean = rep(theta_hat3, nj), sd = sqrt(rep(sigma2_hat3, nj)), log = T))
  pDIC_m3    <- 2*(lpyth_m3 - mean(LP3));pDIC_m3
  dic_m3     <- -2*lpyth_m3 + 2*pDIC_m3;dic_m3
  # WAIC
  g <- rep(NA, n)
  for (j in 1:m) {
    idx <- data1$Dep == unique(data1$Dep)[j]
    g[idx] <- j
    Y[[j]] <- y[idx]
  }
  lppd_m3  <- 0
  pWAIC_m3 <- 0
  sigma3 <- MC3$MC[,-c(1:m,65,66,67)]
  for (i in 1:n) {
    # lppd
    tmp1.3 <- dnorm(x = y[i], mean = MC3$MC[,g[i]], sd = sqrt(sigma3[,g[i]]))
    lppd_m3 <- lppd_m3 + log(mean(tmp1.3))
    # pWAIC
    tmp2.3 <- dnorm(x = y[i], mean = MC3$MC[,g[i]], sd = sqrt(sigma3[,g[i]]), log = T)
    pWAIC_m3 <- pWAIC_m3 + 2*(log(mean(tmp1.3)) - mean(tmp2.3))
  }
  waic_m3 <- -2*lppd_m3 + 2*pWAIC_m3
  
  # DIC4
  LP4        <- as.numeric(MC4$LL$logV)
  ceta_hat4  <- colMeans(MC4$MC[,1:d])
  kappa2_hat4 <- mean(MC4$MC$Kappa2)
  lpyth_m4   <- sum(dnorm(x = y, mean = rep(ceta_hat4, njk), sd = sqrt(kappa2_hat4), log = T))
  pDIC_m4    <- 2*(lpyth_m4 - mean(LP4));pDIC_m4
  dic_m4     <- -2*lpyth_m4 + 2*pDIC_m4;dic_m4
  # WAIC
  lppd_m4  <- 0
  pWAIC_m4 <- 0
  for (i in 1:n) {
    # lppd
    tmp1.4 <- dnorm(x = y[i], mean = MC4$MC[,h[i]], sd = sqrt(MC4$MC$Kappa2))
    lppd_m4 <- lppd_m4 + log(mean(tmp1.4))
    # pWAIC
    tmp2.4 <- dnorm(x = y[i], mean = MC4$MC[,h[i]], sd = sqrt(MC4$MC$Kappa2), log = T)
    pWAIC_m4 <- pWAIC_m4 + 2*(log(mean(tmp1.4)) - mean(tmp2.4))
  }
  waic_m4 <- -2*lppd_m4 + 2*pWAIC_m4
  
  # DIC5
  LP5         <- as.numeric(MC5$LL$logV)
  ceta_hat5   <- colMeans(MC5$MC[,1:d])
  kappa2_hat5 <- mean(MC5$MC$Kappa2)
  lpyth_m5    <- sum(dnorm(x = y, mean = rep(ceta_hat5, njk), sd = sqrt(kappa2_hat5), log = T))
  pDIC_m5     <- 2*(lpyth_m5 - mean(LP5));pDIC_m5
  dic_m5      <- -2*lpyth_m5 + 2*pDIC_m5;dic_m5
  # WAIC
  lppd_m5  <- 0
  pWAIC_m5 <- 0
  for (i in 1:n) {
    # lppd
    tmp1.5 <- dnorm(x = y[i], mean = MC5$MC[,h[i]], sd = sqrt(MC5$MC$Kappa2))
    lppd_m5 <- lppd_m5 + log(mean(tmp1.5))
    # pWAIC
    tmp2.5 <- dnorm(x = y[i], mean = MC5$MC[,h[i]], sd = sqrt(MC5$MC$Kappa2), log = T)
    pWAIC_m5 <- pWAIC_m5 + 2*(log(mean(tmp1.5)) - mean(tmp2.5))
  }
  waic_m5 <- -2*lppd_m5 + 2*pWAIC_m5
}
#-------Resumenes de los CV de MC:----------------------
CVMC1 <- apply(MC1, 2, CVMC)
xtable::xtable(t(as.data.frame(CVMC1)), digits = 3)
EMC1 <- apply(MC1, 2, EMC)
neff1 <- apply(MC1,2, effectiveSize)

#Modelo 2
EMC2 <- apply(MC2$THETA, 2, EMC)
neff2 <- apply(MC2$THETA,2, effectiveSize)
CVMC2 <- apply(MC2$THETA, 2, CVMC)
xtable::xtable(as.data.frame(cbind(EMC2[33:35], neff2[33:35], CVMC1[33:35])), digits = 3)
d <- data.frame(CVMC2[1:32])
colnames(d) <- c('CV')
ggplot(data = d, aes(x = '', y = CV),) + stat_boxplot(geom = "errorbar",
                                width = 0.15) +
  geom_boxplot(fill = 'blue', alpha = 0.7, outlier.colour = 'red') +
  labs(x = '', y = 'CVMC', title = 'CVMC para las medias por Departamento') +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.text.y =  element_text(face = 'bold'))

#Modelo 3 
EMC3 <- apply(MC3$MC, 2, EMC)
neff3 <- apply(MC3$MC,2, effectiveSize)
CVMC3 <- apply(MC3$MC, 2, CVMC)
xtable::xtable(as.data.frame(cbind(EMC3[65:67], neff3[65:67], CVMC3[65:67])), digits = 3)
d3Depto <- data.frame(CVMC3[1:32])
colnames(d3Depto) <- c('CV')
cvDepto <- ggplot(data = d3Depto, aes(x = '', y = CV),) + stat_boxplot(geom = "errorbar",
                                                      width = 0.15) +
  geom_boxplot(fill = 'blue', alpha = 0.7, outlier.colour = 'red') +
  labs(x = '', y = 'CVMC', title = 'Medias por Departamento') +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.text.y =  element_text(face = 'bold'))
d3Mun <- data.frame(CVMC3[33:64])
colnames(d3Mun) <- c('CV')
cvMun <- ggplot(data = d3Mun, aes(x = '', y = CV),) + stat_boxplot(geom = "errorbar",
                                                            width = 0.15) +
  geom_boxplot(fill = 'blue', alpha = 0.7, outlier.colour = 'red') +
  labs(x = '', y = 'CVMC', title = 'Varianzas por Departamento') +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.text.y =  element_text(face = 'bold'))
d3 <- cvDepto + cvMun + plot_layout(ncol = 2)
print(d3)
#Modelo4
EMC4 <- apply(MC4$MC, 2, EMC)
neff4 <- apply(MC4$MC,2, effectiveSize)
CVMC4 <- apply(MC4$MC, 2, CVMC)
xtable::xtable(as.data.frame(cbind(EMC4[1145:1148], neff4[1145:1148], CVMC4[1145:1148])), digits = 3)
d4Mun <- data.frame(CVMC4[1:1112])
colnames(d4Mun) <- c('CV')
cv4Mun <- ggplot(data = d4Mun, aes(x = '', y = CV),) + stat_boxplot(geom = "errorbar",
                                                                       width = 0.15) +
  geom_boxplot(fill = 'blue', alpha = 0.7, outlier.colour = 'red') +
  labs(x = '', y = 'CVMC', title = 'Medias por Municipio') +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.text.y =  element_text(face = 'bold'))
d4Depto <- data.frame(CVMC4[1113:1144])
colnames(d4Depto) <- c('CV')
cv4Depto <- ggplot(data = d4Depto, aes(x = '', y = CV),) + stat_boxplot(geom = "errorbar",
                                                                   width = 0.15) +
  geom_boxplot(fill = 'blue', alpha = 0.7, outlier.colour = 'red') +
  labs(x = '', y = 'CVMC', title = 'Medias por Departamento') +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.text.y =  element_text(face = 'bold'))
d4 <- cv4Mun + cv4Depto + plot_layout(ncol = 2)
print(d4)
#Modelo 5
EMC5 <- apply(MC5$MC, 2, EMC)
neff5 <- apply(MC5$MC,2, effectiveSize)
CVMC5 <- apply(MC5$MC, 2, CVMC)
MC5$MC[,1177:1180]

xtable::xtable(as.data.frame(cbind(EMC5[1177:1180], neff5[1177:1180], CVMC5[1177:1180])), digits = 3)
d5Mun <- data.frame(CVMC5[1:1112])
d5Dep <- data.frame(CVMC5[1113:1144])
d5VarDep <- data.frame(CVMC5[1145:1176])
colnames(d5Mun) <- c('CV')
colnames(d5Dep) <- c('CV')
colnames(d5VarDep) <- c('CV')
cv5Mun <- ggplot(data = d5Mun, aes(x = '', y = CV),) + stat_boxplot(geom = "errorbar",
                                                                    width = 0.15) +
  geom_boxplot(fill = 'blue', alpha = 0.7, outlier.colour = 'red') +
  labs(x = '', y = 'CVMC', title = 'Medias por Municipio') +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.text.y =  element_text(face = 'bold'))
cv5Dep <- ggplot(data = d5Dep, aes(x = '', y = CV),) + stat_boxplot(geom = "errorbar",
                                                                    width = 0.15) +
  geom_boxplot(fill = 'blue', alpha = 0.7, outlier.colour = 'red') +
  labs(x = '', y = 'CVMC', title = 'Medias por Departamento') +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.text.y =  element_text(face = 'bold'))
cv5VarDep <- ggplot(data = d5VarDep, aes(x = '', y = CV),) + stat_boxplot(geom = "errorbar",
                                                                    width = 0.15) +
  geom_boxplot(fill = 'blue', alpha = 0.7, outlier.colour = 'red') +
  labs(x = '', y = 'CVMC', title = 'Varianza por Departamento') +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        axis.title.y = element_text(face = 'bold'),
        axis.text.y =  element_text(face = 'bold'))

d5 <- cv5Mun + cv5Dep + cv5VarDep + plot_layout(ncol = 3)
print(d5)


#----Pregunta 6-----
meanMu <- c(mean(MC1$Theta), mean(MC2$THETA$mu), mean(MC3$MC$mu), mean(MC4$MC$mu),mean(MC5$MC$mu))
cvmu <- c(cv(MC1$Theta), cv(MC2$THETA$mu), cv(MC3$MC$mu), cv(MC4$MC$mu),cv(MC5$MC$mu))
IC_infMu <- c(quantile(MC1$Theta, 0.025), quantile(MC2$THETA$mu, 0.025), quantile(MC3$MC$mu, 0.025), 
              quantile(MC4$MC$mu, 0.025), quantile(MC5$MC$mu, 0.025))
IC_supMu <- c(quantile(MC1$Theta, 0.975), quantile(MC2$THETA$mu, 0.975), quantile(MC3$MC$mu, 0.975), 
              quantile(MC4$MC$mu, 0.975), quantile(MC5$MC$mu, 0.975))
xtable::xtable(data.frame(meanMu, IC_infMu, IC_supMu))
min_x <- min(c(MC2$THETA$mu, MC3$MC$mu, MC4$MC$mu, MC5$MC$mu))
max_x <- max(c(MC2$THETA$mu, MC3$MC$mu, MC4$MC$mu, MC5$MC$mu))
x_ticks <- seq(round(min_x, 0), round(max_x,0), by = 10)
dev.new()
par(mfrow = c(2,2))
hist(MC2$THETA$mu, main = expression(paste('Distribución de ', mu, ' para M2')), xlab = expression(mu), 
     ylab = expression(paste('p(', mu, ')')), freq = FALSE, col = 'gray', 
     border = 'gray', breaks = 30, axes = FALSE, xlim = c(min_x, max_x))
axis(1, at = x_ticks)
axis(2)
abline(v = c(IC_infMu[2], IC_supMu[2], meanMu[2]), lty = c(2,2,1), 
       col = c('red', 'red', 'blue'), lwd = 2)
legend('topleft', legend = c('IC 95%', 'Media'), lty = c(2,1),
       lwd = c(2,2), col = c('red','blue'), bty = 'n')

hist(MC3$MC$mu, main = expression(paste('Distribución de ', mu, ' para M3')), xlab = expression(mu), 
     ylab = expression(paste('p(', mu, ')')), freq = FALSE, col = 'gray', 
     border = 'gray', breaks = 30, axes = FALSE, xlim = c(min_x, max_x))
axis(1,at = x_ticks)
axis(2)
abline(v = c(IC_infMu[3], IC_supMu[3], meanMu[3]), lty = c(2,2,1), col = c('red', 'red', 'blue'), lwd = 2)
legend('topleft', legend = c('IC 95%', 'Media'), lty = c(2,1),
       lwd = c(2,2), col = c('red','blue'), bty = 'n')

hist(MC4$MC$mu, main = expression(paste('Distribución de ', mu, ' para M4')), xlab = expression(mu), 
     ylab = expression(paste('p(', mu, ')')), freq = FALSE, col = 'gray', 
     border = 'gray', breaks = 30, axes = FALSE, xlim = c(min_x, max_x))
axis(1,at = x_ticks)
axis(2)
abline(v = c(IC_infMu[4], IC_supMu[4], meanMu[4]), lty = c(2,2,1), col = c('red', 'red', 'blue'), lwd = 2)
legend('topleft', legend = c('IC 95%', 'Media'), lty = c(2,1),
       lwd = c(2,2), col = c('red','blue'), bty = 'n')

hist(MC5$MC$mu, main = expression(paste('Distribución de ', mu, ' para M5')), xlab = expression(mu), 
     ylab = expression(paste('p(', mu, ')')), freq = FALSE, col = 'gray', 
     border = 'gray', breaks = 30, axes = FALSE, xlim = c(min_x, max_x))
axis(1,at = x_ticks)
axis(2)
abline(v = c(IC_infMu[5], IC_supMu[5], meanMu[5]), lty = c(2,2,1), col = c('red', 'red', 'blue'), lwd = 2)
legend('topleft', legend = c('IC 95%', 'Media'), lty = c(2,1),
       lwd = c(2,2), col = c('red','blue'), bty = 'n')
par(mfrow = c(1,1))


#----Pregunta 7 ----
# Ordenamos el dataframe por puntaje
puntaje <- colMeans(MC5$MC[,1113:1144])
data1$ThetaHat <- puntaje
data1$IC_inf <- apply(MC5$MC[, 1113:1144], 2, function(x) quantile(x, c(0.025)))
data1$IC_sup <- apply(MC5$MC[, 1113:1144], 2, function(x) quantile(x, c(0.975)))
df <- data1[order(data1$ThetaHat, decreasing = TRUE),]

#Gráfico - ranking Bayesiano
ranking1 <- ggplot(data = df, aes(x = ThetaHat, y = reorder(Depto, +ThetaHat))) +
  geom_point(aes(color = ifelse((ThetaHat == 250 | (250 >= IC_inf & 250 <= IC_sup)), "Negro", ifelse(ThetaHat > 250, "Verde", "Rojo"))), position = position_dodge(width = 0.3)) +
  geom_errorbarh(aes(xmin = IC_inf, xmax = IC_sup, color = ifelse((ThetaHat == 250 | (250 >= IC_inf & 250 <= IC_sup)), "Negro", ifelse(ThetaHat > 250, "Verde", "Rojo"))), height = 0.2, 
                 position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("Negro" = "black", "Verde" = "darkgreen", "Rojo" = "darkred")) +
  labs(x = "Puntaje Promedio", y = "Departamento", title = 'Ranking Bayesiano') +
  geom_vline(xintercept = 250, color = 'red') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, family = "sans", face = 'bold')) +
  guides(color = FALSE) 

#Gráfico - ranking frecuentista
ranking2 <- ggplot(data = data1, aes(x = Media, y = reorder(Depto, Media), color = ifelse((Media == 250 | (250 >= Media - 1.96*sqrt(S2/NJ) & 250 <= Media + 1.96*sqrt(S2/NJ))), "Negro", ifelse(Media > 250, "Verde", "Rojo")))) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbarh(aes(xmin = Media - 1.96*sqrt(S2/NJ), xmax = Media + 1.96*sqrt(S2/NJ), color = ifelse((Media == 250 | (250 >= Media - 1.96*sqrt(S2/NJ) & 250 <= Media + 1.96*sqrt(S2/NJ))), "Negro", ifelse(Media > 250, "Verde", "Rojo"))), height = 0.2, position = position_dodge(width = 0.3)) +
  scale_color_manual(values = c("Negro" = "black", "Verde" = "darkgreen", "Rojo" = "darkred")) +
  labs(x = "Puntaje Promedio", y = "Departamento", title = 'Ranking Frecuentista') +
  geom_vline(xintercept = 250, color = 'red') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) + 
  guides(color = FALSE)

ranking <- ranking1 + ranking2 + plot_layout(ncol = 2)
print(ranking)

#----Pregunta 8----
#Matriz de incidencia para depto
A <- matrix(NA, nrow = m, ncol = m)
colnames(A) <- data1$Depto
xi <- MC5$MC[,c(1181:1212)]
B <- nrow(xi)
for (i in 1:m) {
  for (j in 1:m) {
    # Contar cuántas veces los departamentos i y j pertenecen al mismo cluster en todas las iteraciones
    total <- sum(xi[,i] == xi[,j])
    pr <- total/B
    A[i,j] <- total/B
  }
}
#write_xlsx(A, 'MatrizIncidencia.xlsx')

#Cluster posterior
set.seed(20)
clusters <- kmeans(colMeans(MC5$MC[,c(1113:1144)]), centers = 5)
data1$Grupo <- as.factor(as.vector(clusters$cluster))
centros <- clusters$centers
data1$etiquetas <- paste("Grupo", data1$Grupo, ": ", round(centros[data1$Grupo], 0))
#Mapa de los cluster
# col <- c('#FFD1DC', '#D0F0C0','#B5D8EB', '#FFFFD1', '#E6BEFF')
# col1 <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")
gg5 <- data1 %>% inner_join(shp, by = c("Dep" = "DPTO_CCDGO")) %>%
  select(Dep, Depto, Media, Grupo, geometry, etiquetas) %>%
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = etiquetas)) + 
  scale_fill_manual(values =  c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd")) + 
  labs(title = 'Agrupamiento de Departamentos', x = 'Longitud', y = 'Latitud',
       fill = '') + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.position = "bottom")

print(gg5)

#Gráfico de la matriz de incidencia
r <- order(colMeans(MC5$MC[,1113:1144]), decreasing = TRUE)
AOrder <- A[r,r]
corrplot(corr = AOrder, is.corr = FALSE, addgrid.col = NA, method = "color", tl.pos = "n")




#----Pregunta 9----
IP.hat <- matrix(NA, ncol = 8, nrow = 10000)
for (i in 1:8){
  r <- MC5$Regre$beta0 + MC5$Regre$beta1*MC5$MC[,1136 + i]
  IP.hat[,i] <- r
}
colnames(IP.hat) <- data1$Depto[25:32]
IP.hat <- as.data.frame(IP.hat)
colMeans(IP.hat)
xtable::xtable(data.frame(Media = colMeans(IP.hat),
           IC_inf = apply(IP.hat, 2, function(x) quantile(x, 0.025)),
           IC_Sup = apply(IP.hat, 2, function(x) quantile(x, 0.975))),
           digits = 3)
o <- data.frame(Dep = data1$Dep[25:32], Depto = names(IP.hat), Ano = as.numeric(colMeans(IP.hat)))
pobreza <- rbind(pobreza, o)

shp %>% left_join(o, by = c('DPTO_CCDGO' = 'Dep')) %>%
  filter(DPTO_CCDGO != '88') %>%
  select(DPTO_CCDGO, Depto, geometry, Ano) %>%
  st_as_sf() %>%
  ggplot() +
  geom_sf(aes(fill = Ano)) + 
  labs(title = 'Pobreza monetaria 2018', x = 'Longitud',
       y = 'Latitud', fill = 'IPM') + 
  geom_sf_text(aes(label= str_to_title(Depto)),col="black",
               fontface="bold", size=3,fun.geometry=function(x) sf::st_centroid(x)) +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold')) + 
   scale_fill_gradientn(colours = brewer.pal(n = 5, name = "Blues"), 
                        n.breaks=5)

#Gráfica relación pobreza puntaje
pmP11 <- as.data.frame(cbind(data1$Media[-c(25:32)], pobreza$Ano[-c(25:32)]))
colnames(pmP11) <- c('Puntaje', 'Pobreza')
ggplot(data = pmP11, aes(x = Puntaje, y = Pobreza)) +
  labs(x = 'Puntaje Saber 11', y = 'Pobreza monetaria', title = 'Pobreza Monetaria vs puntaje prueba Saber 11') +
  geom_point(color = 'red', shape = 16, size = 2) + 
  scale_x_continuous(breaks = seq(from = 200, to = 270, by = 10)) + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 15),
        axis.title.x = element_text(color = 'blue', hjust = 0.5, face = 'bold'),
        axis.title.y = element_text(color = 'blue', hjust = 0.5, face = 'bold'))

#Gráfica relación CNS y puntaje
left_join(data2, EstEdu, by = c('Depmun3' = 'CÓDIGO_MUNICIPIO')) %>%
  select(Media, COBERTURA_NETA_SECUNDARIA) %>%
  as.data.frame() %>%
  ggplot() + geom_point(aes(x = Media, y = COBERTURA_NETA_SECUNDARIA),
                        color = 'red', shape = 16) +
  labs(x = 'Puntaje Saber 11', y = 'Cobertura neta secundaria',
       title = 'Cobertura neta secundaria vs Puntaje Saber 11') + 
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold', size = 15),
        axis.title.x = element_text(color = 'blue', hjust = 0.5, face = 'bold'),
        axis.title.y = element_text(color = 'blue', hjust = 0.5, face = 'bold'))

#----Pregunta 10----

#Cluster de los municipios
xiMun <- matrix(NA, nrow = B, ncol = 1112)
set.seed(20)
for (i in 1:B){
  Clustermun = kmeans(as.numeric(MC5$MC[i, c(1:1112)]), 8)
  xiMun[i, ] <- Clustermun$cluster
}

#Matriz incidencia de los municipios
AMun <- matrix(NA, nrow = d, ncol = d)
for (i in 1:d) {
  for (j in 1:d) {
    # Contar cuántas veces los departamentos i y j pertenecen al mismo cluster en todas las iteraciones
    total <- sum(xiMun[,i] == xiMun[,j])
    AMun[i,j] <- total/B
  }
}
rMun <- order(colMeans(MC5$MC[,1:1112]), decreasing = TRUE)
AMunOrder <- AMun[rMun,rMun]
corrplot(corr = AMunOrder, is.corr = FALSE, addgrid.col = NA, method = "color", tl.pos = "n")

#Mapa de los cluster
set.seed(20)
Cluster8 <- kmeans(colMeans(MC5$MC[,1:1112]), centers = 8)
data2$Grupo <- as.factor(Cluster8$cluster)
centrosMun <- Cluster8$centers
data2$etiquetas <- paste('Grupo', data2$Grupo,':',round(centrosMun[data2$Grupo], 0))

data2 %>% right_join(shpMun, by = c('Depmun3' = 'MPIO_CDPMP')) %>%
  filter(DPTO_CCDGO != '88') %>%
  select(Depmun3, Media, Grupo, geometry, etiquetas) %>%
  st_as_sf() %>% 
  ggplot() +
  geom_sf(aes(fill = etiquetas)) + 
  scale_fill_manual(values =  c("#FF0000", "#FF7F00", "#FFFF00", "#7FFF00", "#00FF00", "#00FFFF", "#0000FF", "#8B00FF")) + 
  labs(title = 'Agrupamiento de los Municipios', x = 'Longitud', y = 'Latitud',
       fill = '') + 
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        legend.position = "bottom")
  


#----Pregunta 11----
{
  # Encuentra los códigos de municipio que están en EstEdu pero no en data2
  codigos_faltantes <- EstEdu$CÓDIGO_MUNICIPIO[!(EstEdu$CÓDIGO_MUNICIPIO %in% data2$Depmun3)]
  codigos_faltantes2 <- data2$Depmun3[!(data2$Depmun3 %in% EstEdu$CÓDIGO_MUNICIPIO)]
  #data2$mediaspost <- as.vector(colMeans(MC5$MC[1:1112]))
  # Encuentra los códigos de municipio comunes
  codigos_comunes <- intersect(data2$Depmun3, EstEdu$CÓDIGO_MUNICIPIO)
  length(codigos_comunes) #solo aparecen 1110 en ambas bases de datos
  # Filtra solo los municipios comunes en data2 y EstEdu
  data_combinada <- data2[data2$Depmun3 %in% codigos_comunes, ]
  data_combinada <- inner_join(data_combinada, EstEdu, by = c("Depmun3" = "CÓDIGO_MUNICIPIO"))
  data_combinada <- data_combinada[order(data_combinada$Depmun3, decreasing = FALSE),]
  
  
  # Crear una matriz para almacenar los resultados
  resultado <- matrix(NA, nrow = 10000, ncol = 3)
  preds <- matrix(NA, nrow = 10000, ncol = 2)
  
  # Definir el bucle de Monte Carlo
  for (i in 1:10000) {
    # Realizar una regresión lineal
    modelo <- lm(data_combinada$COBERTURA_NETA_SECUNDARIA ~ as.numeric(MC5$MC[i,c(1:581,583:1097,1099:1112)]))
    
    # Almacenar los resultados en la matriz
    resultado[i,] <- c(modelo$coefficients, adjR2(modelo))
    beta_0 <- modelo$coefficients[1]
    beta_1 <- modelo$coefficients[2]
    pred1 <- beta_0+beta_1*MC5$MC$zeta8_12[i]
    pred2 <- beta_0+beta_1*MC5$MC$zeta3_29[i]
    preds[i,] <- c(pred1, pred2)
  }
  
  # medias posteriores de los municipios faltantes
  meanMUNf <- c(colMeans(preds))
  IC_infMUNF <- c(quantile(preds[1], 0.025), quantile(preds[2], 0.025))
  IC_supMUN <- c(quantile(preds[1], 0.975), quantile(preds[2], 0.975))
  xtable::xtable(data.frame(meanMUNf, IC_infMUNF, IC_supMUN))
}
#----Pregunta 12----
PPP <- as.data.frame(matrix(NA, nrow = 1112, ncol = 6))
colnames(PPP) <- c('Media', 'Mediana', 'DE', 'Min', 'Max', 'RIC') 
for (i in 1:1112){
  YS <- NULL
  EST <- matrix(NA, nrow = 10000, ncol = 6)
  for (j in 1:10000){
    YS <- rnorm(n = data2$njk[i], mean = MC5$MC[j,i], sd = sqrt(MC5$MC$Kappa2[j]))
    EST[j,] <- c(mean(YS), median(YS), sd(YS), min(YS), max(YS), IQR(YS))
  }
  PPP[i,] <- c(mean(EST[,1] < data2$Media[i]), mean(EST[,2] < data2$Mediana[i]),
               mean(EST[,3] < sqrt(data2$S2[i])), mean(EST[,4] < data2$Min[i]),
               mean(EST[,5] < data2$Max[i]), mean(EST[,6] < data2$RIC[i]))
}

ggplot(melt(PPP), aes(x = variable, y = value)) + 
  geom_jitter(color = 'gray',width = 0.2, height = 0, size = 3, alpha = 0.3
  ) + 
  geom_boxplot(fill = 'black', alpha = 0.3, outlier.colour = 'black',
               outlier.size = 1, outlier.alpha = 0.3) + 
  labs(x = '', y = 'PPP') + 
  stat_boxplot(geom = "errorbar", width = 0.15) + 
  theme_minimal() + 
  theme( axis.title.x = element_text(face = 'bold'),
         axis.title.y = element_text(face = 'bold'),
         axis.text.x = element_text(face = 'bold', size = 12, color = 'black'),
         axis.text.y = element_text(face = 'bold'))







