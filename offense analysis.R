## Fazendo análise das jogadas ofensivas

# Pra manipular os dados
library(tidyverse)
library(lubridate)
# Pra visualizar os dados
library(ggplot2)
library(scales)

## Importando os dados
df_og <- read.csv("C:/Users/Lucas/Desktop/plots e scrpits offense analytics/Offense Analytics - Geral - GERAL.csv")

## Limpeza e manipulação preliminares
# Renomeando as colunas e vendo o tipo dos dados
Offense.Analytics <-  df_og

colnames(Offense.Analytics) <- c('data', 'situacao', 'jogada',
                                 'ajuste', 'QB', 'solo', 'center', 
                                 'slot', 'aberto', 'resultado', 
                                 'keyplayer', 'jardas', 'cometarios') 
summary(Offense.Analytics)

# Removendo as duas ultimas linhas com informação faltando
Offense.Analytics<- Offense.Analytics[-seq(nrow(Offense.Analytics),
                                           nrow(Offense.Analytics)-1),]

## Agora começando a visualizar

# Boxplots
#####
# Jardas por key player
jarda_kp = boxplot(data = Offense.Analytics, 
                   jardas ~ keyplayer,
                   main = "Jardas ganhas por key player", 
                   ylab="Key Player", 
                   xlab="Jardas", 
                   horizontal = TRUE,
                   notch = FALSE,
                   col = 'orange')
# Jardas por jogada
jarda_jogada = boxplot(data = Offense.Analytics, 
                       jardas ~ jogada,
                       main = 'Jardas ganhas por jogada',
                       ylab="Jogada", 
                       xlab="Jardas", 
                       horizontal = T,
                       notch = F,
                       col = "blue")
# Jardas por QB
jarda_qb = boxplot(data = Offense.Analytics, 
                   jardas ~ QB,
                   main = "Jardas conquistadas por cada Quarterback",
                   ylab = 'Quarterback', 
                   xlab = 'Jardas', 
                   horizontal = T,
                   notch = F,
                   col = 'red')
# Jardas por Solo
jarda_solo = boxplot(data = Offense.Analytics, 
                     jardas ~ solo,
                     main = "Jardas por Solo",
                     ylab = 'Solo', 
                     xlab = 'Jardas', 
                     horizontal = T,
                     notch = F,
                     col = 'grey')
# Jardas por center
jarda_center = boxplot(data = Offense.Analytics, 
                       jardas ~ center,
                       main = 'Jardas obtidas por center',
                       ylab = 'Center', 
                       xlab = 'Jardas', 
                       horizontal = T,
                       notch = F,
                       col = 'black')
# Jardas por Slot
jarda_slot = boxplot(data = Offense.Analytics, 
                     jardas ~ slot,
                     main = 'Jardas por slot',
                     ylab = 'Slot',
                     xlab = 'Jardas', 
                     horizontal = T,
                     notch = F,
                     col = 'pink')
#####
# Barplots