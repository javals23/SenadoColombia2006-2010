# Gráficos de los puntos ideales de los senadores junto con sus intervalos
# de credibilidad
# Distinguibles por su grupo político, por su partido
# y por su vinculación o no vinculación con la parapolítica
# Este código también es una modificación del código de Carolina
# el cual ella elaboró para el senado 2010-2014
# Librerias necesarias
# rm(list=ls()) 
library(pscl)
library(magrittr)
library(dplyr)
library(tidyverse)
library(rstan)
library(readxl)
# Cargar las estimaciones del modelo
load("C:/Users/User/Documents/TrabajoDeGradoEstadistica/fit0_L1.Rda")
est <- summary(stan.fit)$summary
# Cargar la base de datos
s2006_votes <- read_excel("C:/Users/User/Documents/TrabajoDeGradoEstadistica/s2006_votes.xlsx")
#############################################################################################
# puntos ideales estimados
# el rango 143:286 se obtiene al correr est[grep("xi", row.names(est)),1]
# Se puede ver que los 144 puntos ideales estimados aparecen en ese rango.
# Si lo hace para otro senado, debe hacer el proceso de buscar el rango 
# adecuado
BetaM <- est[grep("xi", row.names(est)),1][143:286]
# intervalos de credibilidad 
BetaQ <- est[grep("xi", row.names(est)),c(4,8)][143:286,]
# puntos ideales estimados y sus intervalos de credibilidad
Beta <- as.data.frame(cbind(BetaM, BetaQ))
# Al dataframe Beta se le debe añadir el legislador, grupo, id_legis, partido y si está o no relacionado
# con la parapolítica
leg_group <- s2006_votes %>% dplyr::select(legislador, grupo,id_legis,partido,parapolitica0)%>% 
  distinct(legislador, grupo, id_legis,partido,parapolitica0) %>% 
  arrange(id_legis)
Beta$legislador <- leg_group$legislador
Beta$group <- leg_group$grupo
Beta$id_legis<-leg_group$id_legis
Beta$partido<-leg_group$partido
Beta$parapolitica0<-as.character(leg_group$parapolitica0)
Beta <- Beta %>% rename(legislator=legislador)
row.names(Beta) <- NULL
colnames(Beta)[1:3] <- c("Mean", "Lower", "Upper")
Beta <- Beta[order(Beta$Mean),]

# Puntos ideales en el espacio políitico resaltados por 
# grupo político
windows()
Y <- seq(from=1, to=length(Beta$Mean), by=1)
ggplot(Beta, aes(x=Mean, y=Y)) +
  geom_point(aes(colour=group), shape=19, size=1.5) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper,colour = group),height = 0) +
  geom_text(aes(x = Upper, label = legislator, colour = group), 
            size = 1.5, 
            hjust = -.05) +
  scale_colour_manual(NULL, values = c("red", "chartreuse4", "blue", "goldenrod2"),
                      labels = c("Coalición", "Independientes", "Minorías", "Oposición")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        aspect.ratio = 0.7,
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = 1,colour = "grey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = .4),
        axis.text.x = element_text(colour="black", face="bold", size=8),
        legend.text = element_text (color = "black", size = 9))+
  scale_x_continuous(limits = c(-2.6,3.2)) 

# Puntos ideales en el espacio político resaltados por si están o no
# vinculados con el escándalo de la parapolítica
windows()
Y <- seq(from=1, to=length(Beta$Mean), by=1)
ggplot(Beta, aes(x=Mean, y=Y)) +
  geom_point(aes(colour=parapolitica0), shape=19, size=1.5) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper,colour = parapolitica0),height = 0) +
  geom_text(aes(x = Upper, label = legislator, colour = parapolitica0), 
            size = 1.5, 
            hjust = -.05) +
  scale_colour_manual(NULL, values = c("gray", "black"),
                      labels = c("No involucrado", "Involucrado")) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_blank(),
        aspect.ratio = 0.7,
        legend.position = "bottom",
        legend.direction = "horizontal",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = 1,colour = "grey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = .4),
        axis.text.x = element_text(colour="black", face="bold", size=8),
        legend.text = element_text (color = "black", size = 9))+
  scale_x_continuous(limits = c(-2.6,3.2)) 


# puntos ideales agrupados por partido político
windows()
YP <- round(Beta$Mean,2)
Beta$Party_o <- factor(Beta$partido, levels=c("PU","PC","AEC","CC","CD","CV","CR","AICO","ASI","MIRA","PL","PDA"))
ggplot(Beta, aes(x=Mean, y=YP)) +
  geom_point(size = 1.5, aes(colour = Party_o)) +
  geom_errorbarh(aes(xmin = Lower, xmax = Upper, colour = Party_o), height = 0) +
  facet_grid(Party_o ~ .,scales = "free_y") +
  scale_colour_manual("Partido",values = c("red","red","red","red","red",
                                           "red","red","blue","blue","chartreuse4","goldenrod2","goldenrod2"),
                      labels=c("Social de Unidad Nacional (PU)","Conservador (PC)","Alas Equipo Colombia (AEC)","Convergencia Ciudadana (CC)",
                               "Colombia Democrática (CD)","Colombia Viva (CV)","Cambio Radical (CR)","AICO","ASI","MIRA","Liberal (PL)","Polo Democrático (PDA)"
                      )) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        #axis.title = element_blank(), 
        legend.position = "right",
        legend.direction = "vertical",
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(linetype = 1,colour = "grey"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA, size = .4),
        axis.text.x = element_text(colour="black", face="bold", size=8),
        legend.text = element_text (color = "black", size = 9),
        legend.title = element_text (color = "black", size = 10, face="bold")) + 
  ylab(NULL) +
  xlab(NULL)
