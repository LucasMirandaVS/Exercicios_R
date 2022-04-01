## Instalando os pacotes

library(stats)
library(ggplot2)
library(dplyr)
library(ggfortify)

## Carregando a base de dados
View(iris)

mydata = select(iris, c(1,2,3,4))

## Função que calcula o númerod e k-médias
wssplot <- function(data, nc = 15, seed = 1234)
{
  wss <- nrow((data) -1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers = i)$withinss)}
  plot(1:nc, wss, type = "b", xlab = "Número de clusters",
       ylab = "Soma dos quadrados entre grupos")
}

wssplot(mydata)
# o número ótimo de clusters é 2

## Aplicando o kmeans 

kmedias <- kmeans(mydata, 2)

## Plotando os clusters

autoplot(kmedias, mydata, frame = TRUE)
# os grupos são distitnos e não se encontram. A clusterização foi um sucesso

## Agora olhando os centros dos clusters:
kmedias$centers

# os centros dos clusters tem valores diferentes e não se intercalam.

## Agora aplicando cluster no mtcars

dataf <- (mtcars)

wssplot(dataf)

# A funão apontou 2 como o npuero ótim de clusters

km <- kmeans(dataf, 2)

## Avaliando o plot e os centros
autoplot(km, dataf, frame = TRUE)
km$centers

# os centros dos clusters tem valores diferentes e não se intercalam.

