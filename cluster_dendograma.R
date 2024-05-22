library(readxl)
library(dplyr)
library(tidyverse)
library(cluster)
library(factoextra)
library(NbClust)
library(tidyr)
library(ggplot2)

df= read.csv("C:/Users/sebastianzapata/Downloads/indice_paises.csv"
             , header = TRUE, sep = ";", quote = "\"",
             dec = ",", fill = TRUE, comment.char = "")
View(df)

row.names(df) = df[,1]
df = df[,-1]

#dfinverted <- t(df) #PARA TRANSPORNER

dfT <- scale(df) #Escalar los datos. Sirve para normalizar los datos

#Calcular la matriz de distancia

m.distancia <- get_dist(dfT, method = "euclidean")
fviz_dist(m.distancia, gradient = list(low="red",
                    mid ="yellow", high = "green")) 

# Calcular el número óptimo de clusters

fviz_nbclust(dfT, kmeans, method = "wss") # método 1
fviz_nbclust(dfT, kmeans, method = "silhouette")  # método 2
fviz_nbclust(dfT, kmeans, method = "gap_stat")  # método 3

#Calcular con múltiples métodos el número óptimo de clusters

resumencluster<-NbClust(dfT, distance="euclidean", min.nc=2, 
                max.nc=10, method="kmeans",index = "all") #Este código 
# corre más de 20 métodos al mismo tiempo y proporciona el resultado 
#del número más apropiado de clusters para tomar

k1 <- kmeans(dfT, centers = 2, nstart = 10) # Establecer el número de clusters
# En este caso, 2 es el más apropiado según los métodos

fviz_cluster(k1, data = dfT) # Gráficar el número de clusters en 2D

km1<-hcut(dfT, k =2, stand = TRUE) # Base del dendrograma
fviz_dend(km1,rect=TRUE, cex=0.5, k_colors =c("red","green")) # Graficar el dendrograma

