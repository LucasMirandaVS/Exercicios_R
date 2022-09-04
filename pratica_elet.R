setwd("~/")

#Código da série SGST do BACEN: 1403


elet_1<- read.csv2("elet.csv", header=FALSE, sep=";", dec=".")
head(elet_1)

elet_1<- ts(elet_1, start = c(1979, 1), freq = 12)

elet_1<- elet_1[, -1]

elet<- window(elet_1, start = c(2007, 1), freq= 12)

library(forecast)
library(ggplot2)

###

l_elet <- log(elet)

l_elet %>% ggtsdisplay(xlab="Year", main="Log(ELET)")


##


l_elet %>% diff(lag=12) %>%
  ggtsdisplay(xlab="Year", main="Seasonally differenced Log(ELET)")

####################################################

mod1 <- Arima(l_elet, order=c(2,1,0), 
                 seasonal = list(order = c(0, 1, 1), period = 12))

checkresiduals(mod1)

mod2 <- Arima(l_elet, order=c(2,1,0), 
              seasonal = list(order = c(1, 1, 1), period = 12))
checkresiduals(mod2)

mod3 <- Arima(l_elet, order=c(4,1,1), 
              seasonal = list(order = c(0, 1, 1), period = 12))
checkresiduals(mod3)

mod4 = auto.arima(l_elet)

checkresiduals(mod4)
summary(mod4)

library(texreg)

texreg(list(mod1, mod2, mod3, mod4))

####

er1 = checkresiduals(mod1, plot= FALSE, lag = 12)
er2 = checkresiduals(mod2, plot= FALSE, lag = 12)
er3 = checkresiduals(mod3, plot= FALSE, lag = 12)
er4 = checkresiduals(mod4, plot= FALSE, lag = 12)

modelo1 = round(c(er1$statistic, er1$p.value), digits = 5)
modelo2 = round(c(er2$statistic, er2$p.value), digits = 5)
modelo3 = round(c(er3$statistic, er3$p.value), digits = 5)
modelo4 = round(c(er4$statistic, er4$p.value), digits = 5)

table = rbind(modelo1, modelo2, modelo3, modelo4)

colnames(table) = c("Ljung-Box test", "p-value") 

table

t(table)

table2  = t(table)

colnames(table2) = c("Modelo 1", "Modelo 2", "Modelo 3", "Modelo 4") 

table2

library(stargazer)

stargazer(table2, header = FALSE, summary = FALSE, type = 'text',
          title = "Testes de Autocorrelação dos Resíduos em 12")


########################################

## Previsão mod3 e mod 4 #####

NROW(l_elet)


#Conforme vimos acima, temos uma amostra de 142 observações, 
#se separarmos de 2007.1 a 2014.8, vamos ter quantas observações restantes?

NROW(window(l_elet, end = c(2014, 8)))

142 - 92  #50

#Então eu posso fazer 50 previsões e comparar com minhas observações reais:
#Criando os vetores onde vão entrar as minhas previsões:

prev_fit1<- numeric(50)
prev_fit2<- numeric(50)

##Agora faço um comando de loop para calcular a previsão um passa a frente
#e mando substituir no vetor que eu criei acima:


for(i in 1:50){
  mod_fit1 = Arima(l_elet[1:(92+i-1)], order=c(4,1,1), 
                   seasonal = list(order = c(0, 1, 1), period = 12), include.mean = TRUE)
  prev_fit1[i]<- forecast(mod_fit1, h= 1)$mean
}


for(i in 1:50){
  mod_fit2 = Arima(l_elet[1:(91+i)], order=c(0,1,2), 
                   seasonal = list(order = c(0, 1, 1), period = 12), include.mean = TRUE)
  prev_fit2[i]<- forecast(mod_fit2, h= 1)$mean
}


par(mfrow = c(1,2), mar = c(2,3,4,2))
plot(exp(prev_fit1), main = "Previsão do Modelo 3", type = "p", pch = 19,
     ylim = c(10, 13))
lines(elet[93:142], lwd = 2)

legend('topright', inset = 0.01, ncol = 1, col = c("black", "black"), 
       lwd = 2, pch = c(19, NA), lty = c(NA, 1), legend = c('Previsto', 'Real'), 
       cex=0.9)

#####

plot(exp(prev_fit2), main = "Previsão do Modelo 4", type = "p", pch = 19,
     ylim = c(10, 13))
lines(elet[93:142], lwd = 2)

legend('topright', inset = 0.01, ncol = 1, col = c("black", "black"), 
       lwd = 2, pch = c(19, NA), lty = c(NA, 1), legend = c('Previsto', 'Real'), 
       cex=0.9)

####

#Ajuste Geral do Modelo
Ajuste_Modelo3 =  accuracy(mod_fit1)
Ajuste_Modelo4 = accuracy(mod_fit2)

table3 = rbind(Ajuste_Modelo3, Ajuste_Modelo4)
table4 = t(table3)

colnames(table4) = c("Modelo 3", "Modelo 4")

table5 = t(table4)

stargazer(table5, header = FALSE, summary = FALSE, type = 'text',
          title = "Medidas de Erro para as Previsões do Modelo Geral")


#Ajuste das Previsões :
Ajuste_Modelo3 = accuracy(exp(prev_fit1), elet[93:142])
Ajuste_Modelo4 = accuracy(exp(prev_fit2), elet[93:142])


table3 = rbind(Ajuste_Modelo3, Ajuste_Modelo4)
table4 = t(table3)

colnames(table4) = c("Modelo 3", "Modelo 4")

table5 = t(table4)

#Para o RMarkdown, se tiver com sinal negativo dá erro, aí tem que ajustar 
#pelo latex

stargazer(table5, header = FALSE, summary = FALSE, type = 'text',
          title = "Medidas de Erro para as Previsões de 50 Valores Previstos")




########################################
