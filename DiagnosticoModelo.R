# Código para obtener los gráficos del diagnóstico del modelo
# Este código es una modificación del código de Carolina. 
rm(list=ls()) 
library(pscl)
library(magrittr)
library(dplyr)
library(tidyverse)
library(rstan)
# Cargar las estimaciones del modelo
load("C:/Users/User/Documents/TrabajoDeGradoEstadistica/fit0_L1.Rda")
# Gráfico del estadístico R sombrero
windows()
stan_rhat(stan.fit, bins=60, fill="white") + 
  xlim(c(0.9998,1.0009)) +
  xlab(expression(paste("Estadística  ", hat(R)))) +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))
# Gráfico del tamaño efectivo de muestra sobre el tamaño
# de muestra
windows()
stan_ess(stan.fit, bins=60, fill="white") + 
  xlim(c(0.8,1.11)) + 
  xlab("Tamaño efectivo de muestra/Tamaño de muestra") +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))
# Gráfico del error estándar de Monte Carlo sobre la desviación
# estándar posterior
windows()
stan_mcse(stan.fit, bins=60, fill="white") + 
  xlim(c(0.0083,0.011)) + 
  xlab("Error estándar Monte Carlo/Desviación estándar posterior") +
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))
# Gráfico de la convergencia del modelo (para ver si la cadena es estacionaria)
windows()
traceplot(stan.fit, par = "lp__", colour="black") + 
  theme (legend.position = "none",
         axis.text.x = element_text(colour="black", face="bold", size=13),
         axis.text.y = element_text(colour="black", face="bold", size=13),
         axis.title = element_text(face="bold", colour="black", size=rel(1.5))) + 
  ylab("Log-verosimilitud posterior") +
  xlab("iteraciones")
# Gráfico del coeficiente de variación de las estimaciones
est <- summary(stan.fit)$summary
se_mean <- as.vector(est[1:414,2])
mean <- abs(as.vector(est[1:414,1]))
cv <- round((se_mean/mean)*100,2)
cv[cv>25]
windows()
cv1 <- data.frame(cv)
colnames(cv1) <- c("cv")
ggplot(data=cv1, aes(x=cv)) + 
  geom_histogram(breaks=seq(0, 13, by = 0.5), 
                 col="black", 
                 fill="white") + 
  labs(x="Coeficiente de variación de las estimaciones (%)", y="Número de parámetros") + 
  xlim(c(0,13)) +
  theme_classic() +
  scale_x_continuous(breaks=seq(0, 13, 2))+
  scale_y_continuous(breaks=seq(0, 600, 100))+
  theme(axis.text.x = element_text(colour="black", face="bold", size=13),
        axis.text.y = element_text(colour="black", face="bold", size=13),
        axis.title = element_text(face="bold", colour="black", size=rel(1.5)))