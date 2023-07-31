### MODELO BAYESIANO ###
# Este código es una modificación del código desarrollado
# por Carolina Luque para el senado del periodo 2010-2014. 
rm(list=ls()) 
# Librerias necesarias
library(readxl)
library(readr)
library(dplyr)
library(magrittr)
library(tidyverse)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
# Cargar los datos, recuerde que estos datos fueron construidos en 
# el script llamado PreparacionDatos
s2006_votes <- read_excel("C:/Users/User/Documents/Congreso2006-2010/s2006_votes.xlsx")
# Número de legisladores y listas
(N <- max(s2006_votes$id_legis))                           
(M <- max(s2006_votes$id_list)) 
# Ubicación donde se guardará las estimaciones de los puntos ideales 
# producidas por el modelo
path = "C:/Users/User/Documents/Congreso2006-2010"
file.names <- dir(path, pattern =".stan",full.names = TRUE)
mod_ideal_point <- stan_model("ideal_point_0.stan")
mod_ideal_point
# Se anclan dos senadores de orillas políticas opuestas. 
# Por un lado se ancla a Carlos Cárdenas del PU, es decir, 
# se fija su punto ideal como 1. El otro senador fijado es 
# Guillermo Jaramillo del PDA, el cual se ancla en -1.
# Los demás senadores tienen punto ideal inicial igual a 1 si 
# pertenecen a la coalición 
xi <-
  s2006_votes %>%
  select(legislador,id_legis, grupo)%>%
  distinct(legislador,id_legis, grupo)%>%
  arrange(id_legis)%>%
  mutate( xi = if_else(legislador == "Carlos Cardenas Ortiz:PU", 1,
                       if_else(legislador == "Guillermo Alfonso Jaramillo Martinez:PDA", -1, NA_real_)),
          init = if_else(grupo == "coalicion", 1,-1))
# configuración de los datos para el modelo
legislators_data <-
  within(list(), {
    y <- as.integer(s2006_votes$voto)                   
    y_idx_leg <- as.integer(s2006_votes$id_legis)       
    y_idx_vote <- as.integer(s2006_votes$id_list)       
    Y_obs <- length(y)                              
    N <- max(s2006_votes$id_legis)                      
    M <- max(s2006_votes$id_list)                       
    # priors
    mu_loc <- 0                                                                              
    mu_scale <- 5                                        
    alpha_loc <- 0                                        
    alpha_scale <- 5                                      
    beta_loc <- 0                                        
    beta_scale <- 1
    N_xi_obs <- sum(!is.na(xi$xi))                      
    idx_xi_obs <- which(!is.na(xi$xi))                  
    xi_obs <- xi$xi[!is.na(xi$xi)]                    
    N_xi_param <- sum(is.na(xi$xi))                     
    idx_xi_param <- which(is.na(xi$xi))                 
  })
legislators_init <- list(                               
  list(xi_param = xi$init[is.na(xi$xi)])
)
# Se corre el modelo con 16000 datos de calentamiento
# Se hacen 80000 iteraciones, el refresh de 8000 hace 
# que cada 8000 iteraciones se muestre el progreso alcanzado
ini <- Sys.time()
stan.fit <- sampling(mod_ideal_point, 
                     data = legislators_data,
                     chains = 1, 
                     iter = 80000,
                     warmup = 16000, 
                     thin = 5,
                     init = legislators_init,
                     refresh = 8000,
                     seed=12345)
end <- Sys.time()
(time <- end - ini)
# Se almacenan los resultados del modelo en la ubicación establecida
save(stan.fit, file = "C:/Users/User/Documents/Congreso2006-2010/fit0_L1.Rda")