rm(list=ls())
######### Senado de la República 2006-2010#########
# En este script se encuentra el código junto con los 
# pasos que se siguieron para armar el conjunto de datos
# necesarios para correr el modelo
# librerias necesarias para crear la base de datos
# para el modelo 
library(readxl)
library(dplyr)
library(stringi)
library(stringr)
library(tidyverse)
library(writexl)
# Cargar los datos de las votaciones en el congreso
VotSenado <- read_excel("C:/Users/User/Documents/TrabajoDeGradoEstadistica/DataCV.xlsx",sheet = "Votaciones")
# Cambiar la fecha de radicación a date  
VotSenado$fecha_radicacion <- as.Date(VotSenado$fecha_radicacion)
# Cargar la lista de congresistas del periodo de interés
# Nota: En el excel llamado listacong encontrará una hoja llamada final,
# esta hoja fue hecha a mano mediante la busqueda en diferentes fuentes 
# de la lista de Senadores del periodo de interés. Si usted planea
# replicar este trabajo para otro periodo entonces deberá hacer esta 
# lista por su cuenta.
listacong<-read_excel("C:/Users/User/Documents/TrabajoDeGradoEstadistica/Congresistas1998-2014.xlsx",sheet = "final")
# Eliminar tildes y las letras ñ del nombre completo de cada Senador
listacong$completo<-stri_trans_general(str = listacong$completo,id = "Latin-ASCII")
# Reducir espacios múltiples entre palabras a espacios de longitud uno 
listacong$completo=str_squish(listacong$completo)
# Crear una copia de la lista de Senadores del periodo de interés
A<-listacong$completo
# Nota: La lista A fue sacada de una lista general de congresistas, pero es posible que
# los nombres de los Senadores en las votaciones aparezca diferente y por tal razón
# es necesario obtener una segunda lista de Senadores a partir de la tabla de votaciones
# para así intersectarla con la lista A y verificar que los Senadores aparezcan con los 
# mismos nombres
VotSenado$congresista = stri_trans_general(str = VotSenado$congresista,id = "Latin-ASCII")
VotSenado$congresista=str_squish(VotSenado$congresista)
B<-unique(VotSenado$congresista)
# Se intersectan ambas listas.
C<-intersect(A,B)
# Se Hace la diferencia entre la lista A y la lista C
# para ver si hay alguien en la lista A que no esté en la lista B
setdiff(A,C)
# Nota: al correr el anterior comando le sale que no hay diferencia, pero al correrlo por primera
# vez si había diferencia.Emma Claudia Castellanos aparecía en la lista de congresistas como
# Claudia Yadira Ines Rodriguez de Castellanos, por lo que tocó cambiarle el nombre. 
# Este paso fue muy importante por lo que tengalo muy presente si desea replicar este trabajo en otro
# periodo legislativo.
# Sacar las id de votación 
idvot<-unique(VotSenado$id_votacion)
# Sacar el número de votaciones a partir del conteo
# de las id de votación
nvot<-length(idvot)
# Sacar las votaciones del periodo de interés
votint<-c()
tamano<-c()
for (i in 1:nvot) {
  B<-VotSenado[VotSenado$id_votacion==idvot[i],]$congresista
  coinc<-length(intersect(A,B))
  if(coinc>30 && length(B)-coinc==0){
    votint=c(votint,idvot[i])
  }
}
# Nota: En el  código anterior para cada votación se extrae la lista de congresistas 
# que participaron en esta. Luego se intersecta con la lista de senadores del
# periodo de interés y si se cumple que hay al menos 30 coincidencias entre ambas
# listas y que la lista de senadores de la votación está contenida en la lista de 
# senadores del periodo de interés entonces se guarda la id de esa votación.
# El valor de 30 es porque se quieren las votaciones plenarias. Las comisiones a lo 
# sumo tienen 20 senadores.
# Obtener las votaciones del periodo de interés
votaciones<-VotSenado[VotSenado$id_votacion %in% votint,]
vota<-votaciones[,c("congresista","id_congresista","id_votacion","voto")]
# Nota 1: De las columnas presentes solo son de interés por el momento las 4 que 
# se seleccionan en el dataframe llamado vota.
# Nota 2: El senador Jorge de Jesús Castro Pacheco no asistió
# a ninguna plenaria, por esa razón se saca de la lista
vota<-vota[vota$congresista != "Jorge de Jesus Castro Pacheco", ]  
# Nota: Los senadores Alvaro Alfonso Garcia Romero y Jairo Enrique Merlano Fernandez  
# son sacados de la lista al eliminar datos faltantes
vota<-vota[vota$congresista != "Alvaro Alfonso Garcia Romero", ] 
vota<-vota[vota$congresista != "Jairo Enrique Merlano Fernandez", ] 
# Crear un nuevo dataframe llamado s2006_vot y adicionalmente
# crear una columna llamada id_list, la cual es un id para cada lista
# de votación.
s2006_vot<-c()
k<-length(votint)
for(i in 1:k){
  t<-dim(vota[vota$id_votacion==votint[i],])[1]
  votai<-cbind(vota[vota$id_votacion==votint[i],],rep(i,t))
  s2006_vot<-rbind(s2006_vot,votai)
}
colnames(s2006_vot)[5] <- 'id_list'
# Crear un nuevo dataframe llamado s2006_votes y adicionalmente
# crear una columna llamada id_legis, la cual es un id para cada senador.
s2006_votes<-c()
senadores<-vota[vota$id_votacion==votint[1],]$congresista
l<-length(senadores)
for(i in 1:l){
  t<-dim(s2006_vot[s2006_vot$congresista==senadores[i],])[1]
  votai<-cbind(s2006_vot[s2006_vot$congresista==senadores[i],],rep(i,t))
  s2006_votes<-rbind(s2006_votes,votai)
}
colnames(s2006_votes)[6] <- 'id_legis'
# Cambiar los datos de la columna voto y luego eliminar
# datos faltantes. 
s2006_votes$voto[s2006_votes$voto=="Si"]<-1
s2006_votes$voto[s2006_votes$voto=="No"]<-0
s2006_votes$voto[s2006_votes$voto=="No aplica"]<-NA
s2006_votes$voto[s2006_votes$voto=="No asistio"]<-NA
s2006_votes$voto[s2006_votes$voto=="Se abstuvo"]<-NA
s2006_votes<-s2006_votes[!is.na(s2006_votes$voto), ]
# Cambiar los nombres de los partidos por sus siglas
listacong$partido<-as.factor(listacong$partido)
levels(listacong$partido)<-c("AEC","ASI","AICO","CR","CD","CV","PC","CC","PL","MIRA","PU","PDA")
# Definir la columna partido en s2006_votes
a<-dim(s2006_votes)[1]
partido<-c()
for (i in 1:a) {
  b<-which(listacong$completo==s2006_votes$congresista[i])
  partido<-c(partido,as.character(listacong$partido[b]))
}
s2006_votes$partido<-partido
# Combinar la columna congresista con la columna
# partido para crear la columna legislador
s2006_votes$legislador <- paste(s2006_votes$congresista,s2006_votes$partido,sep=":")
# Definir los grupos políticos
pcoalicion<-c("CR","CD","CV","PC","CC","AEC","PU")
poposicion<-c("PL","PDA")
pindependiente<-c("MIRA")
pminoria<-c("ASI","AICO")
# Crear la columna grupo
s2006_votes<- s2006_votes %>% 
  mutate(grupo = 
           ifelse(s2006_votes$partido %in% pcoalicion,
                  "coalicion",
                  ifelse(s2006_votes$partido %in% poposicion,
                         "oposicion",
                         ifelse(s2006_votes$partido %in% pindependiente,
                                "independiente",
                                ifelse(s2006_votes$partido %in% pminoria,
                                       "minoria","ERROR")))))
# El dataframe llamado listacong contiene una columna llamada parapolítica
# la cual es 1 si el senador estuvo o está involucrado con el escándalo de 
# la parapolítica.
# Crear la columna legislador en listacong para luego unir este 
# con s2006_votes 
listacong$completo <- paste(listacong$completo,listacong$partido,sep=":")
listacong<-listacong[,c(4,19)]
colnames(listacong)[1] <- 'legislador'
# Retener las columnas legislador, id_legis, grupo, id_votacion
# voto, id_list y partido en el dataframe s2006_votes
s2006_votes<-s2006_votes[,c(8,6,9,3,4,5,7)]
# Cambiar el tipo de dato de la columna voto a numérico
s2006_votes$voto<-as.numeric(s2006_votes$voto)
# Combinar los dataframe s2006_votes y listacong
s2006_votes<-merge(s2006_votes,listacong,by='legislador',all.x = TRUE)
# Crear un archivo excel con los datos necesarios para el modelo
write_xlsx(s2006_votes,"C:/Users/User/Documents/Congreso2006-2010/s2006_votes.xlsx")