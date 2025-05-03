#Limpio entorno de trabajo
rm(list = ls())
#Especifico directorio de trabajo
setwd('C:/Users/USUARIO/Documents/Universidad/2023-2S/Estadística Bayesiana/Caso 1')
#librarias a usar
library(readr)
library(ggplot2)
library(ggExtra)
library(gridExtra)
#Conjunto de datos
verizon <- read_delim('Verizon.csv', delim = ',', col_names = TRUE,
                      col_types = cols('d', 'c'))

#----Análisis Bayesiano----
# Inciso 1
  {  
#Particion
Y1 <- subset(verizon, Group == 'ILEC')$Time
Y2 <- subset(verizon, Group == 'CLEC')$Time

#medias del tiempo de reparación Grupo 1 - ILEC
bar.y1 <- mean(Y1)
#medias del tiempo de reparación Grupo 2 - CLEC
bar.y2 <- mean(Y2)
#Desv. estandar del tiempo de reparación Grupo 1 - ILEC
sd.y1 <- sd(Y1)
#Desv. estandar del tiempo de reparación Grupo 1 - ILEC
sd.y2 <- sd(Y2)

#Hiper parametros
ak <- 3
bk <- 17

#Tamaño grupos
n1 <- length(Y1)
n2 <- length(Y2)

#Estadistica suficiente
s1 <- sum(Y1)
s2 <- sum(Y2)

#Parametros posterior - grupo 1
a1p <- ak + n1
b1p <- bk + s1 
#Prametros posterior - grupo 2
a2p <- ak + n2
b2p <- bk + s2

#Previa y posterior
set.seed(79)
curve(1/dgamma(x, ak, bk), lwd = 2, col = 'violet',
      main = 'Distrbución posterior', xlab = expression(lambda),
      ylab = expression(paste('p(',lambda,'|y)',)), axes = FALSE)
axis(1)
axis(2)
set.seed(79)
curve(1/dgamma(x, a1p, b1p), lwd = 2, col = 'yellow', add = TRUE)
set.seed(79)
curve(1/dgamma(x, a2p, b2p), lwd = 2, col = 'green', add = TRUE)
legend(x = 'topright', lwd = c(2,2,2), lty = c(1,1,1),
       legend = c('Previa', 'ILEC', 'CLEC'), col = c('violet', 'yellow','green'),
       bty = 'n')

#Generación
set.seed(79)
lamda1 <- 1/rgamma(n = 100000, a1p, b1p)
set.seed(79)
lamda2 <- 1/rgamma(n = 100000, a2p, b2p)
eta <- data.frame(eta = lamda1 - lamda2)
hist(lamda1, freq = FALSE)
hist(lamda2, freq = FALSE)
hist(eta$eta, freq = FALSE)

#Distribución de heta
par(bg = "#f7f7f7")
hist(eta$eta, main = expression(paste('Distribución de ', eta)),
     xlab = expression(eta), ylab = expression(paste('p(',eta,'|',lambda[k],')')),
     freq = FALSE, col = 'gray', border = 'blue', breaks = 30)
lines(density(eta$eta), lwd = 2, col = 'red')


#Estimación puntual de heta
heta.mean <- mean(eta$eta)
heta.cv <- abs(sd(eta$eta)/heta.mean)
IC <- quantile(eta$eta, c(0.025, 0.975)) 
IC_inf <- as.numeric(IC[1])
IC_sup <- as.numeric(IC[2])

estimaciones <- data.frame(Media = heta.mean, CV = heta.cv, ICinf = IC_inf, ICsup = IC_sup)

#Representación visual de los estimadores
hist(eta$eta, main = expression(paste('Distribución de ', eta)),
     xlab = expression(eta), ylab = expression(paste('p(',eta,'|',lambda[k],')')),
     freq = FALSE, col = 'gray', border = 'gray', breaks = 30, axes = FALSE)
lines(density(eta$eta), type = 'l', lwd = 2, col = 'red',
     xlab = expression(eta), ylab = expression(paste('p(',eta,'|',lambda[k],')')),
     main =  expression(paste('Distribución de ', eta)), font.main = 2,
     axes = FALSE, add = TRUE)
axis(1)
axis(2)
abline(v = c(IC_inf, IC_sup, heta.mean), lty = c(2,2,1), col = c('green', 'green', 'blue'), lwd = 2)
legend(x = -27.5, y = 0.1, legend = c('Posterior', 'IC 95%', 'Media'), lty = c(1,2,1),
       lwd = c(2,2,2), col = c('red','green','blue'), bty = 'n')
  }
# Inciso 2
  {
    # Previa 1
    {
    ak1 <- 3
    bk1 <- 17
    
    #Estadisticas a priori
    set.seed(79)
    hetaPrev1 <- (1/rgamma(10000, ak1, bk1)) - (1/rgamma(10000, ak1, bk1))
    meanPrev.eta1 <- mean(hetaPrev1)
    cvPrev.eta1 <- sqrt(var(hetaPrev1))/meanPrev.eta1
    
    #Posterior
    set.seed(79)
    etaPost1 <- (1/rgamma(10000, ak1 + n1, bk1 + s1)) - (1/rgamma(10000, ak1 + n2, bk1 + s2))
    
    #Estadisticas posterior
    meanPost.eta1 <- mean(etaPost1)
    cvPost.eta1 <- abs(sqrt(var(etaPost1))/meanPost.eta1)
    IC1 <- quantile(etaPost1, c(0.025, 0.975))
    IC1_inf <- as.numeric(IC1[1])
    IC1_sup <- as.numeric(IC1[2])
    
    #Estadisticas a priori
    meanPrev.eta2 <- bk1/(ak1-1)
    cvPrev.eta2 <- 1/(ak1-2)
    }
    # Previa 2
    {
    ak2 <- 2
    bk2 <- 8.5
    
    #Estadisticas a priori
    meanPrev.eta2 <- bk2/(ak2-1)
    cvPrev.eta2 <- 1/(ak2-2)
    
    #Posterior
    set.seed(79)
    etaPost2 <- (1/rgamma(10000, ak2 + n1, bk2 + s1)) - (1/rgamma(10000, ak2 + n2, bk2 + s2))
    
    #Estadisticas posterior
    meanPost.eta2 <- mean(etaPost2)
    cvPost.eta2 <- abs(sqrt(var(etaPost2))/meanPost.eta2)
    IC2 <- quantile(etaPost2, c(0.025, 0.975))
    IC2_inf <- as.numeric(IC2[1])
    IC2_sup <- as.numeric(IC2[2])
    }
    # Previa 3
    {
    ak3 <- 3
    b1.3 <- 16.8
    b2.3 <- 33
    
    #Estadísticas a priori
    set.seed(79)
    hetaPrev3 <- (1/rgamma(10000, ak3, b1.3)) - (1/rgamma(10000, ak3, b2.3))
    meanPrev.eta3 <- mean(hetaPrev3)
    cvPrev.eta3 <- abs(sqrt(var(hetaPrev3))/meanPrev.eta3)
    
    #Posterior
    set.seed(79)
    etaPost3 <- (1/rgamma(10000, ak3 + n1, b1.3 + s1)) - (1/rgamma(10000, ak3 + n2, b2.3 + s2))
    
    #Estadisticas posterior
    meanPost.eta3 <- mean(etaPost3)
    cvPost.eta3 <- abs(sqrt(var(etaPost3))/meanPost.eta3)
    IC3 <- quantile(etaPost3, c(0.025, 0.975))
    IC3_inf <- as.numeric(IC3[1])
    IC3_sup <- as.numeric(IC3[2])
    }
    # Previa 4
    {
    ak4 <- 2
    b1.4 <- 8.4
    b2.4 <- 16.5
    
    #Distribución previa
    set.seed(79)
    etaPrev4 <- (1/rgamma(10000, ak4, b1.4)) - (1/rgamma(10000, ak4, b2.4))
    #Estadísticas
    meanPrev.eta4 <- mean(etaPrev4)
    cvPrev.eta4 <- abs(sqrt(var(etaPrev4))/meanPrev.eta4)
    
    #Distribución posterior
    set.seed(79)
    etaPost4 <- (1/rgamma(10000, ak4 + n1, b1.4 + s1)) - (1/rgamma(10000, ak4 + n2, b2.4 + s2))
    #Estadísticas
    meanPost.eta4 <- mean(etaPost4)
    cvPost.eta4 <- abs(sqrt(var(etaPost4))/meanPost.eta4)
    IC4 <- quantile(etaPost4, c(0.025, 0.975))
    IC4_inf <- as.numeric(IC4[1])
    IC4_sup <- as.numeric(IC4[2])
    }
    #Gráficas
    {
    par(mfrow = c(2,2), bg = "#f7f7f7")
    #Grafica posterior 1
    hist(etaPost1, main = expression(paste(a['k'],'= 3, ', b['k'],'= 17')),
           xlab = expression(eta), ylab = expression(paste('p(',eta,'|',lambda[k],')')),
           freq = FALSE, col = 'gray', border = 'gray', breaks = 30, axes = FALSE, font.main = 2)
    lines(density(etaPost1), type = 'l', lwd = 2, col = 'red',
         xlab = expression(eta), ylab = expression(paste('p(',eta,'|',lambda[k],')')),
         main =  expression(paste(a['k'],'= 3, ', b['k'],'= 17')), font.main = 2)
    axis(1)
    axis(2)
    abline(v = c(IC1_inf, IC1_sup, meanPost.eta1), lty = c(2,2,1), col = c('green', 'green', 'blue'), lwd = 2)
    legend('topleft', legend = c('Posterior', 'IC 95%', 'Media'), lty = c(1,2,1),
           lwd = c(2,2,2), col = c('red','green','blue'), bty = 'n', cex = 0.8)
    #Grafica posterior 2
    hist(etaPost2, main = expression(paste(a['k'],'= 2, ', b['k'],'= 8.5')),
         xlab = expression(eta), ylab = expression(paste('p(',eta,'|',lambda[k],')')),
         freq = FALSE, col = 'gray', border = 'gray', breaks = 30, axes = FALSE, font.main = 2)
    lines(density(etaPost2), type = 'l', lwd = 2, col = 'red',
         xlab = expression(eta), ylab = expression(paste('p(',eta,'|',lambda[k],')')),
         main =  expression(paste(a['k'],'= 2, ', b['k'],'= 8.5 '
         )), font.main = 2)
    axis(1)
    axis(2)
    abline(v = c(IC2_inf, IC2_sup, meanPost.eta2), lty = c(2,2,1), col = c('green', 'green', 'blue'), lwd = 2)
    legend('topleft', legend = c('Posterior', 'IC 95%', 'Media'), lty = c(1,2,1),
           lwd = c(2,2,2), col = c('red','green','blue'), bty = 'n', cex = 0.8)
    #Grafica posterior 3
    hist(etaPost3, main = expression(paste(a['k'],'= 3, ', b['1'],'= 16.8, ',
                                           b['2'],'= 33')),
         xlab = expression(eta), ylab = expression(paste('p(',eta,'|',lambda[k],')')),
         freq = FALSE, col = 'gray', border = 'gray', breaks = 30, axes = FALSE, font.main = 2)
    lines(density(etaPost3), type = 'l', lwd = 2, col = 'red',
         xlab = expression(eta), ylab = expression(paste('p(',eta,'|',lambda[k],')')),
         main =  expression(paste(a['k'],'= 3, ', b['1'],'= 16.8, ',
                                  b['2'],'= 33')), font.main = 2)
    axis(1)
    axis(2)
    abline(v = c(IC3_inf, IC3_sup, meanPost.eta3), lty = c(2,2,1), col = c('green', 'green', 'blue'), lwd = 2)
    legend('topleft', legend = c('Posterior', 'IC 95%', 'Media'), lty = c(1,2,1),
           lwd = c(2,2,2), col = c('red','green','blue'), bty = 'n', cex = 0.8)
        
    #Gráfica posterior 4
    hist(etaPost4, main = expression(paste(a['k'],'= 2, ', b['1'],'= 8.4, ',
                                           b['2'],'= 16.5')),
         xlab = expression(eta), ylab = expression(paste('p(',eta,'|',lambda[k],')')),
         freq = FALSE, col = 'gray', border = 'gray', breaks = 30, axes = FALSE, font.main = 2)
    lines(density(etaPost4), type = 'l', lwd = 2, col = 'red',
         xlab = expression(eta), ylab = expression(paste('p(',eta,'|',lambda[k],')')),
         main = expression(paste(a['k'],'= 2, ', b['1'],'= 8.4, ',
                                 b['2'],'= 16.5')), font.main = 2,
         axes = FALSE)
    axis(1)
    axis(2)
    abline(v = c(IC4_inf, IC4_sup, meanPost.eta4), lty = c(2,2,1), col = c('green', 'green', 'blue'), lwd = 2)
    legend('topleft', legend = c('Posterior', 'IC 95%', 'Media'), lty = c(1,2,1),
           lwd = c(2,2,2), col = c('red','green','blue'), bty = 'n', cex = 0.8)
    par(mfrow = c(1,1))
    }
    #Tablas
    EstPost2 <- data.frame(
      Previas = c('Prev. 1', 'Prev. 2','Prev. 3','Prev. 4'),
      Medias = c(meanPost.eta1, meanPost.eta2, meanPost.eta3, meanPost.eta4),
      CV = c(cvPost.eta1, cvPost.eta2, cvPost.eta3, cvPost.eta4),
      'IC Inf.' = c(IC1_inf, IC2_inf, IC3_inf, IC4_inf),
      'IC Sup.' = c(IC1_sup, IC2_sup, IC3_sup, IC4_sup)
    )
  }
# Inciso 3
{
  # Grupo 1- ILEC
  B <- 10000
  sd1 <- rep(0,B)
  mean1 <- rep(0,B)
  for (i in 1:B){
    set.seed(79 + i)
    l1 <- 1/rgamma(n = 1, ak + n1, bk + s1)
    set.seed(79 + i)
    y1 <- rexp(n = n1, rate = 1/l1)
    sd1[i] <- sd(y1)
    mean1[i] <- mean(y1)
  }
  # Grupo 2 - CLEC
  sd2 <- rep(0,B)
  mean2 <- rep(0,B)
  for (i in 1:B){
    set.seed(79 + i)
    l2 <- 1/rgamma(n = 1, ak + n2, bk + s2)
    set.seed(79 + i)
    y2 <- rexp(n = n2, rate = 1/l2)
    sd2[i] <- sd(y2)
    mean2[i] <- mean(y2)
  }
  #Valor p predictivo posterior
  {
    pppMean1 <- mean(mean1<bar.y1)
    pppMean2 <- mean(mean2<bar.y2)
    pppsd1 <- mean(sd1<sd.y1)
    pppsd2 <- mean(sd2<sd.y2)
  }
  #Graficos
  {
  par(mfrow = c(2, 2), bg = "#f7f7f7")
  hist(mean1, freq = FALSE, main= 'Ajuste para la media - ILEC',
       xlab = expression(bar(X)['1']), ylab = expression('p('* bar(x)['1'] * bold('|y') * ')'),
       breaks = 30, col = 'gray', border = 'gray')
  lines(density(mean1), lwd = 2, col = 'red')
  abline(v = c(bar.y1, quantile(mean1, c(0.025, 0.975))), lty = c(1, 2, 2), 
         col = c('blue', 'yellow', 'yellow'), lwd = c(2, 3, 3))
  legend(x = 'topright', y = 'center',legend = c('Densidad', 'IC 95%', 'Est. Obser.'),
         lty = c(1,2,1), lwd = c(2,3,2), col = c('red', 'yellow', 'blue'),
         bty = 'n', cex = 1)
  
  hist(mean2, freq = FALSE, main= 'Ajuste para la media - CLEC',
       xlab = expression(bar(X)['2']), ylab = expression('p('* bar(x)['2'] * bold('|y') * ')'),
       breaks = 30,col = 'gray', border = 'gray')
  lines(density(mean2), lwd = 2, col = 'red')
  abline(v = c(bar.y2, quantile(mean2, c(0.025, 0.975))), lty = c(1, 2, 2), 
         col = c('blue', 'yellow', 'yellow'), lwd = c(2, 3, 3))
  legend(x = 'topright', y = 'center',legend = c('Densidad', 'IC 95%', 'Est. Obser.'),
         lty = c(1,2,1), lwd = c(2,3,2), col = c('red', 'yellow', 'blue'),
         bty = 'n', cex = 1)
  
  hist(sd1, freq = FALSE, main= 'Ajuste para el Desv. Estandar - ILEC',
       xlab = expression(s['1']), ylab = expression('p('* s['1'] * bold('|y') * ')'),
       breaks = 30,col = 'gray', border = 'gray')
  lines(density(sd1), lwd = 2, col = 'red')
  abline(v = c(sd.y1, quantile(sd1, c(0.025, 0.975))), lty = c(1, 2, 2), 
         col = c('blue', 'yellow', 'yellow'), lwd = c(2, 3, 3))  
  legend(x = 'topright',legend = c('Densidad', 'IC 95%', 'Est. Obser.'),
         lty = c(1,2,1), lwd = c(2,3,2), col = c('red', 'yellow', 'blue'),
         bty = 'n', cex = 1)
  
  hist(sd2, freq = FALSE, main= 'Ajuste para el Desv. Estandar - CLEC',
       xlab = expression(s['2']), ylab = expression('p('* s['2'] * bold('|y') * ')'),
       breaks = 30,col = 'gray', border = 'gray')
  lines(density(sd2), lwd = 2, col = 'red')
  abline(v = c(sd.y2, quantile(sd2, c(0.025, 0.975))), lty = c(1, 2, 2), 
         col = c('blue', 'yellow', 'yellow'), lwd = c(2, 3, 3))
  legend(x = 'topright',legend = c('Densidad', 'IC 95%', 'Est. Obser.'),
         lty = c(1,2,1), lwd = c(2,3,2), col = c('red', 'yellow', 'blue'),
         bty = 'n')
  par(mfrow = c(1, 1))
  
  #Dispersogarama
  windows(width = 800, height = 500)
  opar <- par(no.readonly = TRUE)
  par(mfrow = c(1, 2), bg = "#f7f7f7")
  par(mar = c(10, 4, 4, 2))
  
  # Grupo 1
  plot(mean1, sd1, ylim = c(7, 15), col = 'gray', main = 'Grupo 1', ylab = 'Desv. Est.', xlab = 'Media')
  points(x = bar.y1, y = sd.y1, col = 'blue', pch = 17, lwd = 5, cex = 2)
  abline(v = quantile(mean1, c(0.025, 0.975)), h = quantile(sd1, c(0.025, 0.975)), lty = c(2, 2, 2, 2), col = 'red')
  
  # Grupo 2
  plot(mean2, sd2, col = 'gray', main = 'Grupo 2', ylab = 'Desv. Est.', xlab = 'Media')
  points(x = bar.y2, y = sd.y2, col = 'blue', pch = 17, lwd = 3, cex = 2)
  abline(v = quantile(mean2, c(0.025, 0.975)), h = quantile(sd2, c(0.025, 0.975)), lty = c(2, 2, 2, 2), col = 'red')
  
  # Restaurar la configuración original de par
  par(opar)
  
  # Agregar leyenda debajo de los gráficos
  legend("bottom", legend = c("Valor Observado", "IC al 95%"), col = c("blue", "red"), 
         horiz = TRUE, pch = c(17, NA), lty = c(NA, 2), lwd = c(NA, 1), bty = "n", xpd = TRUE, inset = c(-12, 0))
  }
  #Dispergroma con histograma conjunto
  # Crea un dataframe de ejemplo para simular tus datos
  G1 <- data.frame(mean1, sd1)
  G2 <- data.frame(mean2, sd2)
  
  # Gráfico 1: mean1 vs sd1
  cor1 <- data.frame(Media = bar.y1, SD = sd.y1)
  plot1 <- ggplot(G1, aes(x = mean1, y = sd1)) +
    geom_point() + labs(x = 'Media', y = 'Desv. Est.', title = 'Grupo 1 - ILEC') +
    theme(plot.title = element_text(hjust = 0.5, face = 'bold')) + 
    geom_vline(xintercept = quantile(mean1, c(0.025, 0.975)), linetype = c('dashed', 'dashed'),
               color = c('green', 'green'), size = c(1,1)) + 
    geom_hline(yintercept = quantile(sd1, c(0.025, 0.975)), linetype = c('dashed', 'dashed'),
               color = c('green', 'green'), size = c(1,1)) + 
    geom_point(data = cor1, aes(x = Media, y = SD), color = "red", shape = 17, size = 3)
  
  
  # Agrega histogramas marginales
  ScatterHist1 <- ggMarginal(plot1, type = "histogram", 
                             xparams = list(fill = "#43799F", col = 'gray'),
                             yparams = list(fill = "#F49335", col = 'gray'))
  
  # Gráfico 2: mean2 vs sd2
  cor2 <- data.frame(Media = bar.y2, SD = sd.y2)
  plot2 <- ggplot(G2, aes(x = mean2, y = sd2)) +
    geom_point() + labs(x = 'Media', y = 'Desv. Est.', title = 'Grupo 2 - CLEC') +
    theme(plot.title = element_text(hjust = 0.5, face = 'bold')) + 
    geom_vline(xintercept = quantile(mean2, c(0.025, 0.975)), linetype = c('dashed', 'dashed'),
               color = c('green', 'green'), size = c(1,1)) + 
    geom_hline(yintercept = quantile(sd2, c(0.025, 0.975)), linetype = c('dashed', 'dashed'),
               color = c('green', 'green'), size = c(1,1)) + 
    geom_point(data = cor2, aes(x = Media, y = SD), color = "red", shape = 17, size = 3)
  
  
  # Agrega histogramas marginales
  ScatterHist2 <- ggMarginal(plot2, type = "histogram", 
                             xparams = list(fill = "#43799F", col = 'gray'),
                             yparams = list(fill = "#F49335", col = 'gray'))
  grid.arrange(ScatterHist1, ScatterHist2, ncol = 2)
  
  #Tablas
  {
  BondAjus <- data.frame('Grupo' = c('ILEC', 'CLEC'),
    'SD Obs.' = c(sd.y1, sd.y2),
    'Media Obs.' = c(bar.y1, bar.y2),
    'PPP Media' = c(pppMean1, pppMean2),
    'PPP Desv. Est.' = c(pppsd1, pppsd2))
  }
}

#---Análisis Frecuentista----
#Normalidas asintotica
{
MLE <- function(meanY1, meanY2, n1, n2){
  meanEta <- meanY1 - meanY2
  sd <- sqrt(((meanY1)^2/n1) + ((meanY2)^2/n2))
  cvEta <- abs(sd/meanEta)
  IC <- c(meanEta - (1.96*sd), meanEta +( 1.96*sd))
  return(list(meanEta = meanEta, cv = cvEta, IC = IC))
}
}
#Boostrap parametrico
{
bostP <- function(B, mean.y1, mean.y2, n1, n2){
  eta.hat <- c()
  for (i in 1:B){
    set.seed(79 + i)
    y1 <- mean(rexp(n1, rate = 1/mean.y1))
    set.seed(79 + i)
    y2 <- mean(rexp(n2,rate = 1/mean.y2))
    eta.hat[i] <- y1 - y2
  }
  meanEtaP <- mean(eta.hat)
  cvEtaP  <- abs(sd(eta.hat)/mean(eta.hat))
  IC_P  <- as.numeric(quantile(x = eta.hat, probs = c(0.025, 0.975)))
  return(list(meanEta = meanEtaP, cvEta = cvEtaP, IC = IC_P))
}
}
#Boostrap no parametrico
{
bostNP <- function(B, sample1, sample2, n1, n2){
  eta.hat <- c()
  for (i in 1:B) {
    set.seed(79 + i)
    y1 <- sample(x = sample1, size = n1, replace = T)
    set.seed(79 + i)
    y2 <- sample(x = sample2, size = n2, replace = T)
    eta.hat[i] <- mean(y1) - mean(y2)
  }
  meanEtaNP <- mean(eta.hat)
  cvEtaNP  <- abs(sd(eta.hat)/mean(eta.hat))
  IC_NP  <- as.numeric(quantile(x = eta.hat, probs = c(0.025, 0.975)))
  return(list(meanEta = meanEtaNP, cvEta = cvEtaNP, IC = IC_NP))
}
}
# Tabla
{
Bp <- bostP(10000, mean.y1 = bar.y1, mean.y2 = bar.y2,
              n1 = n1, n2 = n2)
Na <- MLE(meanY1 = bar.y1, meanY2 = bar.y2, n1 = n1, n2 = n2)
Bnp <- bostNP(10000, sample1 = Y1, sample2 = Y2,
            n1 = n1, n2 = n2)

Est <- data.frame(Metodo = c('Bayes','Norm. Asint.', 'Boostrap P', 'Boostrap NP'), 
           Media = c(heta.mean, Na$meanEta, Bp$meanEta, Bnp$meanEta), 
           CV = c(heta.cv, Na$cv, Bp$cvEta, Bnp$cvEta),
           IC.Inferior = c(IC_inf, Na$IC[1], Bp$IC[1], Bnp$IC[1]), 
           IC.Superior = c(IC_sup, Na$IC[2], Bp$IC[2], Bnp$IC[2]))

}
# Grafica
{
  Est$B <- as.factor(Est$Metodo)
  ggplot(data = Est, aes(x = B, y = Media)) + 
    geom_errorbar(aes(ymin = IC.Inferior, ymax = IC.Superior), size = 1) +
    geom_point(col = 'blue', size = 3) + 
    labs(title = 'Intervalo de Confianza por método', x = 'Método') +
    theme_classic() + 
    theme(plot.title = element_text(hjust = 0.5),
          panel.background = element_rect(fill = "#f7f7f7"))
   
}
#Falta presentación gráficas

#----Simulacion----

#Escenarios - método Bayesiano
{
  B <- 100000
  n <- c(10, 20, 50, 100)
  propBayes <- c()
  eta.true <- bar.y1 - bar.y2
  for (j in 1:length(n)){
    etasBayes <-c()
    for (i in 1:B){
      set.seed(79 + i)
      y1 <- rexp(n = n[j], rate = 1/bar.y1)
      set.seed(79 + i)
      y2 <- rexp(n = n[j], rate = 1/bar.y2)
      set.seed(79 + i)
      l1 <- 1/rgamma(n[j], 3 + n[j],
                     17 + sum(y1))
      set.seed(79 + i)
      l2 <- 1/rgamma(n[j], 3 + n[j],
                     17 + sum(y2))
      etasBayes[i] <- mean(l1 - l2)
    }
    ICBayes <- quantile(etasBayes, c(0.025, 0.975))
    propBayes[j] <- mean(eta.true >= ICBayes[1] & eta.true <= ICBayes[2])
  }
  propBayes
}

#Escenarios - Normalidad Asint.
{
  propMLE <- c()
  for (j in 1:length(n)){
    etasMLE <-c()
    for (i in 1:B){
      #set.seed(79 + i)
      y1 <- rexp(n = n[j], rate = 1/bar.y1)
      #set.seed(79 + i)
      y2 <- rexp(n = n[j], rate = 1/bar.y2)
      etasMLE[i] <- mean(y1) - mean(y2)
    }
    sdMLE <- (bar.y1^2/n1) + (bar.y2^2/n2)
    ICMLE <- c(eta.true - 1.96*sdMLE, eta.true + 1.96*sdMLE)
    propMLE[j] <- mean(eta.true >= ICMLE[1] & eta.true <= ICMLE[2])
  }
  propMLE
}
#Escenarios - Boostrop No Par.
{
  propNP <- c()
  for (j in 1:length(n)){
    etasNP <- c()
    for (i in 1:B) {
      set.seed(79 + i)
      y1 <- sample(rexp(n = n[j], 1/bar.y1), size = n[j], replace = T)
      set.seed(79 + i)
      y2 <- sample(rexp(n = n[j], 1/bar.y2), size = n[j], replace = T)
      etasNP[i] <- mean(y1) - mean(y2)
    }
    ICNP  <- quantile(etasNP, c(0.025, 0.975))
    propNP[j] <- mean(eta.true >= ICNP[1] & etasNP <= ICNP[2])
  }
}
#Escenarios - Boostrap Param.
propP <- c()
for (j in 1:length(n)){
  etasP <- c()
  for (i in 1:B){
    set.seed(79 + i)
    y1 <- mean(rexp(n[j], rate = 1/bar.y1))
    set.seed(79 + i)
    y2 <- mean(rexp(n[j], rate = 1/bar.y2))
    
    etasP[i] <- y1 - y2
  }
  ICP <- quantile(etasP, c(0.025,0.975))
  propP[j] <- mean(eta.true >= ICP[1] & eta.true <= ICP[2])
}
#Tabla
EstPar3 <- data.frame(
  'Muestra (n)' = n,
  Bayes = propBayes,
  'Norm. Asint' = propMLE,
  'Boostrap Par.' = propP,
  'Boostrap No Par.' = propNP
)


