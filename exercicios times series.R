# Lista 3 de econometria. 
## Discente : Lucas Miranda Vilela Santos

#Parte 1
#CAP. 2 ENDERS, EXERCÍCIO 9 
# 9) a)
getwd()
setwd("C:/RFundamentos/econometria2/Enders_Data Sets")
getwd()

library(sas7bdat)
data<-  read.sas7bdat("sim_2.sas7bdat")
head(data)
data<- data[,-1]
head(data)

Y1 = data[,"Y1"]  # gerado por yt = 0.7yt-1 + e_t Processo AR(1)
Y2 = data[,"Y2"]  # gerado por yt = -0.7yt-1 + et - 0.7et-1 Processo ARMA(1,1)

#Definindo um modelo ARMA(1,1)

library(forecast)
mod2_2 = Arima(Y2, order = c(1, 0, 1), include.mean = FALSE, method = "CSS")

#Plotando o ACF do ARMA (1,1)

par(mfrow = c(1,1), mar = c(4,4,4,4))  
acf1 = acf(Y2, main="Panel (c): ACF for the ARMA(1, 1) process")
acf1

# Ao analisar o grágico gerado pelo modelo ARMA (1,1), 
#ele parece se comportar como um processo estacionário

#Modificando o Box.test para que ele desconte os graus de liberdade
#e vamos chama-lo de Box.test2:


Box.test2<- function (x, lag = 1, type = c("Box-Pierce", "Ljung-Box"), fitdf = 0) 
{
  if (NCOL(x) > 1) 
    stop("x is not a vector or univariate time series")
  DNAME <- deparse(substitute(x))
  type <- match.arg(type)
  cor <- acf(x, lag.max = lag, plot = FALSE, na.action = na.pass)
  n <- sum(!is.na(x))
  PARAMETER <- c(df = lag - fitdf)
  obs <- cor$acf[2:(lag + 1)]
  if (type == "Box-Pierce") {
    METHOD <- "Box-Pierce test"
    STATISTIC <- n * sum(obs^2)
    PVAL <- 1 - pchisq(STATISTIC, lag - fitdf)
  }
  else {
    METHOD <- "Box-Ljung test"
    STATISTIC <- n * (n + 2) * sum(1/(seq.int(n - 1, n - 
                                                lag) - fitdf) * obs^2)
    PVAL <- 1 - pchisq(STATISTIC, lag - fitdf)
  }
  names(STATISTIC) <- "Q-squared"
  structure(list(statistic = STATISTIC, parameter = PARAMETER, 
                 p.value = PVAL, method = METHOD, data.name = DNAME), 
            class = "htest")
}

# 9) b)

#Definindo um objeto AR(1)
mod2_1 = Arima(Y2, order = c(1, 0, 0), include.mean = FALSE, method = "CSS")
summary(mod2_1)
SRR<- sum(mod2_1$residuals^2 )
SRR
K = nrow(as.matrix(mod2_1$coef))
Box.test2(mod2_1$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod2_1$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod2_1$residuals, lag = 24,type = "Ljung-Box", fitdf = K)


#Definindo um outro processo ARMA(1,1)
mod2_2 = Arima(Y2, order = c(1, 0, 1), include.mean = FALSE, method = "CSS")

summary(mod2_2)
SRR<- sum(mod2_2$residuals^2 )
SRR

K = nrow(as.matrix(mod2_2$coef))

Box.test2(mod2_2$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod2_2$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod2_2$residuals, lag = 24,type = "Ljung-Box", fitdf = K)


#Agora vamos definir um processo AR(2):
mod2_3 = Arima(Y2, order = c(2, 0, 0), include.mean = FALSE, method = "CSS")
summary(mod2_3)
SRR<- sum(mod2_3$residuals^2 )
SRR
K = nrow(as.matrix(mod2_3$coef))
Box.test2(mod2_3$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod2_3$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod2_3$residuals, lag = 24,type = "Ljung-Box", fitdf = K)

PVAL <- 1 - pchisq(22.59, 24-2)
PVAL 

#Aqui temos um resultado que se aproxima com o da tabela 2.3 do capítulo 2
# do Ender. Dessa forma está confirmado o resultado pedido no enunciado


# 9) c) 
#Aqui a resposta começa por definir um processo MA(2)
ma(Y1, order=2, centre = TRUE)
mod3_2= arima(Y2, order = c(0, 0, 2), include.mean = FALSE, method = "CSS")
summary(mod3_2)
SRR<- sum(mod3_2$residuals^2 )
SRR
K = nrow(as.matrix(mod3_2$coef))
Box.test2(mod3_2$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod3_2$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod3_2$residuals, lag = 24,type = "Ljung-Box", fitdf = K)

#Resultados do teste Ljung-Box:
# oito defasagens : 29.08, com significancia de 0.000%; 
# dezesseis defasagens: 38.27, com significancia de 0.000%
# vinte e quatro defasagens : 39.68, com significancia de 0.1175%
# Esses resultados indicam que deve-se rejeitar H0 que Q(8)= 0; Q(16)=0; Q(24)=0; 
# Há portanto a existencia de autocorrelação serial, e este não é um modelo apropriado

# 9) d)
# Ao comparar os modelos ARMA (1,1) e MA(2), é possível constatar a significancia de ambos 
# os coeficientes. Entretanto, o teste de Ljung-Box para as defasagens do ARMA(1,1) não indicaram
# a presença de autocorrelação serial. No modelo MA(2) esses testes indicaram autocorrelação nos testes.
# Com essa evidencia podemos afirmar que o ARMA (1,1) é mais apropriado.
.

# Exercício 10)
Y3 = data[,"Y3"] #gerado por yt = 0.7yt-1 - 0.49yt-2 + e_t Processo AR(2)

#10) a)

# Começamos definindo um processo AR(2)
mod4_1 = Arima(Y3, order = c(2, 0, 0), include.mean = FALSE, method = "CSS")
summary(mod4_1)
acf2 = acf(Y3, main="Panel (c): ACF for the AR(2) process")
pacf2 = pacf(Y3, main="Panel (d): PACF for the AR(2) process")

par(mfrow = c(1,1), mar = c(4,4,4,4))  
acf2
pacf2

SRR<- sum(mod4_1$residuals^2 )
SRR
K = nrow(as.matrix(mod4_1$coef))
Box.test2(mod4_1$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod4_1$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod4_1$residuals, lag = 24,type = "Ljung-Box", fitdf = K)

# Para chegar a resposta, comparei a amostra da seção 7 do capítulo 2 e os resultados do ACF e PCF
# Teste Ljung-Box: 8 defasagens = 9.78, com significancia de 13,4%;
# Com esse resultado, não podemos rejeitar o H0 do teste em que Q(8) = 0
# Para as defasagens de Q(16) e Q(24) foi detectada autocorrelção serial.Portanto esse modelo não é apropriado
#Esse resultado não é o mesmo do livro.


# 10) b)

#Começamos defininco um processo AR(1)
mod4_2 = Arima(Y3, order = c(1, 0, 0), include.mean = FALSE, method = "CSS")
summary(mod4_2)
SRR<- sum(mod4_2$residuals^2 )
SRR
K = nrow(as.matrix(mod4_2$coef))
Box.test2(mod4_2$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod4_2$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod4_2$residuals, lag = 24,type = "Ljung-Box", fitdf = K)


#Aqui faremos um Teste CUSUM, como na linha 399 da Aula_Pratica3_ARMA

y_break = read.csv2("y_break.csv", sep=";", dec='.')
head(y_break)
y_break <- y_break[,-1]

#par(mfrow= c(2,2))  
y_break<- ts(y_break, start = 1)

library(dynlm)
mod1<- dynlm(y_break~lag(y_break,-1))

summary(mod1)

library(strucchange)

prev_one <- recresid(mod1)
sigma <- sqrt(var(prev_one))

#Teste CUSUM


process <- cumsum(c(0, prev_one))/(sigma)
#Upper 5%:
N<- length(process)

Up5<- NA

T<- length(y_break)
size <- 10 #quantos valores entram...
n <- size

for(i in n:N){
  Up5[i]<- 0.948*((T-n)^0.5 +2*(i - 10)*(T - n)^(-0.5))
}

####
#Lower 5%:
Lw5<- NA

T<- length(y_break)
n <- size

for(i in n:N){
  Lw5[i]<- - 0.948*((T-n)^0.5 +2*(i - 10)*(T - n)^(-0.5))
}

#####


plot(process, type= "l", ylim = c(-40, 50),
     ylab = "", lwd = 2, main = "Panel (c): The CUSUM Test")
lines(Up5, type="l", lty =2, lwd = 2)
lines(Lw5, type="l", lty =3)
abline(h=0, lty=1)
legend('topright',
       ncol=3, bty ="n",  col=c('black','black', 'black'), lty=c(1,2,3), lwd=c(1,2,1),
       legend=c('CUSUMS', 'Upper 5%', "Lower 5%"))




par(mfrow= c(1,1))


# Fizemos o teste de CUSUM para identificar as quebras na série a partir do
# comportamento dos resíduos. No gráfico (c), vemos que os resíduos começam
# a divergir quando chegam no valor 100. Do 100 em diante, temos que os resíduos excedem
# para fora do intervalo de confiança(entre 100 e 150), aqui está clara a quebra na série
# Os ressíduos do Ljung-Box indicam a existecna de uma autocorrelação significativa nos resíduos
# Para Q(8), Q(16) e Q(24) são 0 para todos os três. Esse modelo não é apropriado

# 10) c) 
#A resposta começa definindo um processo ARMA (1,1)
mod4_3 = Arima(Y3, order = c(1, 0, 1), include.mean = FALSE, method = "CSS")
summary(mod4_3)
SRR<- sum(mod4_3$residuals^2 )
SRR

#verificando acf e pacf (deu igual do modelo AR(2))

acf3 = acf(Y3, main="Panel (c): ACF for the ARMA(1,1) process")
pacf3 = pacf(Y3, main="Panel (d): PACF for the ARMA(1,1) process")

par(mfrow = c(1,1), mar = c(4,4,4,4))  
acf3
pacf3


#Produziu o mesmo resultado que o modelo AR (2).

#Box.test2

Box.test2(mod4_3$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod4_3$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod4_3$residuals, lag = 24,type = "Ljung-Box", fitdf = K)

#### os Ljung-Box Q indicam autocorrelação significativa nos 
#### resíduos. Os níveis de significância de Q (8), Q (16) e Q (24) são 
#### 0 para todos os três. Logo, o modelo se mostra inadequado.

# 10) d) 
# Começamos definindo o objeto AR(2)

mod4_4 = Arima(Y3, order = c(2, 0, 0), include.mean = FALSE, method = "CSS")
summary(mod4_4)
SRR<- sum(mod4_4$residuals^2 )
SRR
Box.test2(mod4_4$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod4_4$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod4_4$residuals, lag = 24,type = "Ljung-Box", fitdf = K)

# Portanto está confirmado o resultado na pagina 74 do enders

# 11)
quart = read.csv2("quarterly.csv", sep=";", dec='.')
head(quart)
tail(quart)
quart = ts(quart, start = c(1960, 1), end = c(2012, 4), freq = 4)

# 11) a) 

#Primeiramente, definimos um AR(||1,3||)
ppi = quart[ , "Finished"]
dly = diff(log(ppi))
plot(dly, type  = "l")

library(forecast)

#Agora, definimos um processo AR(1,3)
mod5_1 = Arima(dly, order = c(1, 0, 0), include.mean = TRUE,
               seasonal = list(order = c(1, 0, 0), period = 3))
summary(mod5_1)

# Agora  definimos um AR(3)
mod5_2 = Arima(dly, order = c(3, 0, 0), include.mean = TRUE)
summary(mod5_2)

# Seguimos definindo um processo ARMA(1,1)
mod5_3 = Arima(dly, order = c(1, 0, 1), include.mean = TRUE)
summary(mod5_3)

# Segundo o AIC e BIC, temos que o modelo AR(||1, 3||), (mod5_1) é o 
# que apresenta o melhor ajustamento por ser o que apresenta menores

# 11) b)

quart = read.csv2("quarterly.csv", sep=";", dec='.')
head(quart)
tail(quart)
quart = ts(quart, start = c(1960, 1), end = c(2012, 4), freq = 4)
ppi = quart[ , "Finished"]
dly = diff(log(ppi))


n <- sum(!is.na(dly))
n # nos dá 211 observações

# Ao descontar as 3 observações do AR (3), temos 208 observações restantes
# Para fins de realizar o exercício, foi escolhido o número de 55 observações
# Com isso, pudemos testar a capacidade dos modelos e tecer as previsões

#Aqui foi replicado o exemplo do livro:
dly_base = window(dly, start = c(1960, 1), end = c(2000, 2), freq = 4)
n <- sum(!is.na(dly_base)) 
n #161 - 3, teremos até 158 observações

#Agora podemos definir o precosso AR(1,3)
mod1_dly= Arima(dly_base, order = c(1, 0, 0), include.mean = TRUE,
                seasonal = list(order = c(1, 0, 0), period = 3))
summary(mod1_dly)

# Agora podemos modelar alguns passos a frente:

# A amostra tem 211 observações. Ao separarmos de 1960.1 a 2000.2, 
# Vemos o numero d eobservações restantes

NROW(dly)
NROW(dly_base)
211 - 161  #50

#Portanto, é possível fazer 50 previsões e comparar com as observações, para isso
# criamos vetores para as previsões

prev_mod1<- numeric(50) #AR(1,3)
prev_mod2<- numeric(50) #ARMA (1,1)

# Agora, usamos um laço /for para calcular a previsão e substituimos no vetor criado
#mod 1, considerando um AR(1,3):

mod1_dly= Arima(dly_base, order = c(1, 0, 0), include.mean = TRUE,
                seasonal = list(order = c(1, 0, 0), period = 3))
summary(mod1_dly)


for(i in 1:50){
  mod1_dly = Arima(dly[1:(162+i-1)], order = c(1, 0, 0), include.mean = TRUE,
                   seasonal = list(order = c(1, 0, 0), period = 3))
  prev_mod1[i]<- forecast(mod1_dly, h= 1)$mean
}

par(mfrow = c(2,1), mar = c(2,3,4,2))
plot(prev_mod1, main = "Previsão do Modelo AR(1,3)", type = "p", pch = 19, 
     ylim = c(-0.05,0.05))
lines(dly[163:212], lwd = 2)


legend('topright', inset = 0.01, ncol = 2, col = c("black", "black"), 
       lwd = 2, pch = c(19, NA), lty = c(NA, 1), legend = c('Previsto', 'Real'), 
       cex=0.9)

mean(prev_mod1)
var(prev_mod1)

summary(lm(dly[163:212]~prev_mod1))

e1 = (prev_mod1 - dly[163:212])
mean(e1)
var(e1)

# Agora definimos um ARMA (1,1)
for(i in 1:50){
  mod2_dly = Arima(dly[1:(162+i-1)], order = c(1, 0, 1), include.mean = TRUE)
  prev_mod2[i]<- forecast(mod2_dly, h= 1)$mean
}

plot(prev_mod2, main = "Previsão do Modelo ARMA(1,1)", type = "p", pch = 19, 
     ylim = c(-0.05,0.05))
lines(dly[163:212], lwd = 2)


legend('topright', inset = 0.01, ncol = 2, col = c("black", "black"), 
       lwd = 2, pch = c(19, NA), lty = c(NA, 1), legend = c('Previsto', 'Real'), 
       cex=0.9)

mean(prev_mod2)
var(prev_mod2)

summary(lm(dly[163:212]~prev_mod2))

e2 = (prev_mod2 - dly[163:212])
mean(e2)
var(e2)

# Podemos constatar analisando o gráfico gerado que as previsões estaõ bem aproximadas

cor(prev_mod1, prev_mod2)

#Aqui analisamos os resíduos das previsões FORECAST ERROR and FORECAST ERROR VARIANCE
error.mod1 = (prev_mod1 - dly[163:212])
error.mod2 = (prev_mod2 - dly[163:212])
var(error.mod1)
var(error.mod2)

#Ajuste das Previsões :
Ajuste_Modelo1 = accuracy((prev_mod1), dly[163:212])
Ajuste_Modelo1
Ajuste_Modelo2 = accuracy((prev_mod2), dly[163:212])
Ajuste_Modelo2

# Podemos dizer com base nos resíduos que o modelo AR (1,3) é o mais apropriado

# 11) c) 

# Em virtude do comportamento dos modelos, não há a necessidade de usar o teste DIEBOLD-MARIANO

# 11) d)

# Começamos essa resposta definindo um processo AR(5)
mod5_4 = Arima(dly, order = c(5, 0, 0), include.mean = TRUE)
summary(mod5_4)

# Em seguida, definimos um processo ARMA(2,1)
mod5_5 = Arima(dly, order = c(2, 0, 1), include.mean = TRUE)
summary(mod5_5)

# De acordo com os AIC e BIC dos modelos, o processo  (||AR(1,3)||) é mais apropriado.

# EXERCÍCIO 12

# 12 a) e b)
#deixa aqui de stand by log_curr = log(quart[, "Curr"])

c1 = ((quart[,  "Curr"]))
plot (c1)

par(mar=c(5,4,4,5)+.1)
plot(diff(log(c1))*100, main = "The Level and Growth Rate of c1",
     ylab = "Rate of Growth (%)", xlim=c(1960, 2012),
     ylim = c(-3, 8),
     lwd = 2)
par(new = T)
plot(c1, ylim=c(0,2500), xlim=c(1960, 2012), xlab='', 
     ylab='', xaxt = 'n', yaxt='n', col='black', 
     type = "l", lty =2, lwd = 2)
axis(4)
mtext('Billions of $',side=4,line=3) ##line=3, é qual próximo ela está do eixo direito
legend('bottomright', ncol=2, bty ="n",  col=c('black','black'), lty=c(1,2), lwd=c(1,2),
       legend=c('Rate of growth', 'c1 in billions'))


c1 = window(c1, start = c(1962, 3), end = c(2008, 2), freq= 4)

acf_c1 <- acf(diff(log(c1)), lag.max = 22)
pacf_c1 <- pacf(diff(log(c1)), lag.max = 22)

# Para fazer os gráficos parecidos com os do Enders:

#Transformando os acf em matrizes:
acf_c1_t<- matrix(acf_c1$acf, ncol=1)
pacf_c1_t<- matrix(pacf_c1$acf, ncol=1)

m =matrix(1, nrow=1)
pacf_c1_t<- rbind(m, pacf_c1_t)

#Juntamos na mesma matriz:
auto <- cbind(acf_c1_t, pacf_c1_t)

#Transpondo para fazer o gráfico

auto_t <- t(auto)

### Estimando o intervalo de confiança:

dp = 2*1/(acf_c1$n.used)^(1/2)

#Colocar os gráficos juntos
#par(mfrow = c(2,1), mar = c(2, 4, 3, 2)) 


barplot(auto_t, beside=T, main="Panel a: C1 Growth", ylim = c(-0.4, 1),
        names.arg = rep(0:22) , cex.names = 0.8, xaxs = "i",
        ylab=" ",legend.text=c("Autocorrelations","PACF"),
        args.legend=list(x = "topright", bty="n", ncol = 2))

abline(h = 0)
grid(nx=NA, ny=NULL)

#Para as grades ficarem abaixo do gráfico, para melhorar a estética dele
#repita o comando e adicione add = TRUE:

barplot(auto_t, beside=T, main="Panel a: C1 Growth", ylim = c(-0.4, 1),
        names.arg = rep(0:22) , cex.names = 0.8, xaxs = "i",
        ylab=" ",legend.text=c("Autocorrelations","PACF"),
        args.legend=list(x = "topright", bty="n", ncol = 2), add=TRUE)
abline( h = dp, lty = 2)
abline( h = - dp, lty = 2)

## Repetir todo o procedimento para a primeira diferença sazonal:

#Obter ACF e PACF
#Obter ACF e PACF

# criar a série de moeda em log e em primeira diferença
dc = diff(log(c1))

#lag:
l = 4

acf_c1 <- acf(diff(dc, lag = l), plot = FALSE)
pacf_c1 <- pacf(diff(dc, lag = l, lag.max = 22), plot =FALSE)

#Transformar em matrizes:
acf_c1_t<- matrix(acf_c1$acf, ncol=1)
pacf_c1_t<- matrix(pacf_c1$acf, ncol=1)

#Compatibilizar as saídas das duas matrizes ACF e PACF
m =matrix(1, nrow=1)
pacf_c1_t<- rbind(m, pacf_c1_t)

#Juntar na mesma matriz:
auto <- cbind(acf_c1_t, pacf_c1_t)

#Transpor para fazer o gráfico

auto_t <- t(auto)

#Estimando o intervalo de confiança
dp = 2*1/(acf_c1$n.used)^(1/2)


barplot(auto_t, beside=T, main="Panel b: Seasonally Differenced c1 Growth", 
        ylim = c(-0.5, 1),
        names.arg = rep(0:22), cex.names = 0.8, xaxs = "i",
        ylab=" ",legend.text=c("Autocorrelations","PACF"),
        args.legend=list(x = "topright", bty="n", ncol = 2))

abline(h = 0)
grid(nx=NA, ny=NULL)
abline( h = dp, lty = 2)
abline( h = - dp, lty = 2)

#grid(NA, 8, lwd = 2) # grid only in y-direction


barplot(auto_t, beside=T, main="Panel b: Seasonally Differenced c1 Growth", 
        ylim = c(-0.5, 1),
        names.arg = rep(0:22) , cex.names = 0.8, xaxs = "i",
        ylab=" ",legend.text=c("Autocorrelations","PACF"),
        args.legend=list(x = "topright", bty="n", ncol = 2), add=TRUE)
abline( h = dp, lty = 2)
abline( h = - dp, lty = 2)

## para voltar o gráfico ao normal:
par(mfrow = c(1,1))

# Através do padrão sazonal, vemos um padrão misto. De acordo com os critérios do livro,
# Um Ma puro precisa que o ACF corte o eixo 0 e para o Ar puro, há um decaimento do ACF, no caso 
# do processo analisado, esse decaimento não é geométrico. Como não parece haver um padrão
# na oscilação do PACF em que os primeiros sete valores alternam no sinal. Contudo, sua queda
# é um sinal de um coeficiente positivo para o MA, o que não ocorre no MA observado, isto é mais uma evidencia
# que não estamos tratando um processo MA puro. Possivelmente o melhor modelo é o misto.
# No conjunto analisado, nenhum conjunto de dados corresponderá as supisições necessárias
# para a metodologia Box-Jenkins, o que dificulta a realuzação desses testes. Como nem sempre está claro quais
# características amostrais de fato estão no processo, é possível incorrer no prblema de sobreajuste

# 12) c) ### 

#Model 1
library(forecast)

mod6_c1 = Arima(log(c1), order = c(1, 1, 0),
                seasonal = list(order = c(0, 1, 1), period = 4), include.mean = TRUE)

mod6_c1

checkresiduals(mod6_c1, lag = 4, plot = FALSE)
# Faz:
#pchisq(Q, df)
#df = s (defasagem) - p - q 

pval <- 1-pchisq(1.4592, 2)
pval

#
checkresiduals(mod6_c1, lag = 8, plot = FALSE)
checkresiduals(mod6_c1, lag = 12, plot = FALSE)

summary(mod6_c1)

#Modelo 2
mod7_c1 = Arima(log(c1), order = c(0, 1, 1),
                seasonal = list(order = c(0, 1, 1), period = 4), include.mean = TRUE)
summary(mod7_c1)

checkresiduals(mod7_c1, lag = 4, plot = FALSE)
checkresiduals(mod7_c1, lag = 8, plot = FALSE)
checkresiduals(mod7_c1, lag = 12, plot = FALSE)

# Os critérios AIC e BIC, assim como o Ljung-Box indicaram que o modelo ARMA(1, 1, 0)(0, 1, 1) é mais apropriado,
# Em relação a significancia, ambos os modelos apresentaram parametros significantes testes Ljung-Box não indicaram
# autocorrelação serial dos erros.

# 13) a)
## i) ##

inp = quart[ , "IndProd"]
dinp = diff(log(inp))
plot(dinp, type  = "l")

library(forecast)

#AR(1)
mod8_1 = Arima(dinp, order = c(1, 0, 0), include.mean = TRUE,
               seasonal = list(order = c(0, 0, 0), period = 0))
summary(mod8_1)
SRR<- sum(mod8_1$residuals^2 )
SRR
Box.test2(mod8_1$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod8_1$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod8_1$residuals, lag = 24,type = "Ljung-Box", fitdf = K)

## ii)

# AR(||8||)
mod8_2 = Arima(dinp, order = c(1, 0, 0), include.mean = TRUE,
               seasonal = list(order = c(1, 0, 0), period = 8))
summary(mod8_2)
SRR<- sum(mod8_2$residuals^2 )
SRR
Box.test2(mod8_2$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod8_2$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod8_2$residuals, lag = 24,type = "Ljung-Box", fitdf = K)

# De acordo com o AIC e BIC, vemos que o AR (|8|) tem valores menores. O Ljung-Box
# evidencia que ao não rejeitar  a hipótese nula de que Q(8)=0, este modelo não apresenta 
# autocorrelação. Ao analisar o  Q(16) e o Q(24), vemos que ambos modelos apresentam
# autocorrelação serial. Logo, o modelo AR(||8||) é o mais apropriado, apesar de ainda apresentar problemas.

#13) b) 
un = quart [,"Unemp"]

# i) 

plot (un, main = "Unemployment rate time path ", type = "l", pch = 19)
par(mfrow = c(1,1), mar = c(4,4,4,4))  
acf4 = acf(un, main="ACF for the unemployment rate")

# O gráfico indica que não se trata de uma série estacionária, pois o processo não
# parece ser constante ao lingo do tempo. Logo, não é razoavel supor que a serie é 
# covariancia estacionária. O gráfico da ACF mostra que a série ultrapassa o limite de
# 5% do interalo de confiança, o que indica que a distribuição dos errso não é uma normal.
# Temos motivos para se preocupar com esta série

# ii)  
library(forecast)
#AR(2)

mod9_1 = Arima(un, order = c(2, 0, 0), include.mean = TRUE, method = "CSS")
summary (mod9_1)
SRR<- sum(mod9_1$residuals^2 )
SRR

# O modelo AR(2) apresenta melhores coeficientes , com ar1: 1.6460; ar2: -0.6827; intercepto: 6.1601.
# Esses valores diferem dos encontrados no livro. Vale ressaltar que foi preciso ignorar a 
# detecção da não estacionaridade e a  necessidade de tirar a primeira diferença.

# iii) 

# Através dos coeficientes vemos que as raízes não estão dentro do círculo unitário. 
# Os coeficientes estimados (a1 = 1.6460 e a2 = -0.6827)
# É preciso tirar a primeira diferença e verificar se os problema da não estacionaridade foi corrigido

# iv) 

dun = diff(log(un))
plot(dun, type  = "l")

#AR (1)
mod9_2 = Arima(dun, order = c(1, 0, 0), include.mean = TRUE, method = "CSS")
summary (mod9_2)
SRR<- sum(mod9_2$residuals^2 )
SRR

# Retirando a primeira diferença do modelo AR(1), vemos que o intercepto é significativo
# Os resíduos tambem apresentaram um valor menor em relação ao AR (2). A inspeção visual indicou
# que se trata e um processo estacionário

# 13) c)
inf = quart[ , "CPI"]
dinf = diff(log(inf))
#?diff
plot(dinf, type  = "l")
par(mfrow = c(1,1), mar = c(4,4,4,4))  

## i) ##
acf5 = acf(dinf, main="ACF for the first difference of CPI")
pacf4 = pacf(dinf, main="PACF for the first difference of CPI")

#AR(1)
mod9_3 = Arima(dinf, order = c(1, 0, 0), include.mean = TRUE, method = "CSS")
SRR<- sum(mod9_3$residuals^2 )
SRR
Box.test2(mod9_3$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod9_3$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod9_3$residuals, lag = 24,type = "Ljung-Box", fitdf = K)

#A análise do grafico, assim como a análise do AIC e BIC e Ljung-Box mostram que
# o modelo não é estacionário nem possui autocorrelação serial. Portamto, é necessário
# tirar a segunda diferença para verificar se o problema foi resolvido.

# ii) 
d2inf = diff(log(inf), differences=2)
plot(d2inf, type  = "l")

#MA(1)
mod9_4 = Arima(d2inf, order = c(0, 0, 1), include.mean = TRUE,
               seasonal = list(order = c(1, 0, 0), period = 2))
summary(mod9_4)
SRR<- sum(mod9_4$residuals^2 )
SRR
K = nrow(as.matrix(mod9_4$coef))
Box.test2(mod9_4$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod9_4$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod9_4$residuals, lag = 24,type = "Ljung-Box", fitdf = K)

#AR(1)
mod9_5 = Arima(d2inf, order = c(1, 0, 0), include.mean = TRUE,
               seasonal = list(order = c(1, 0, 0), period = 2))
summary(mod9_5)
SRR<- sum(mod9_5$residuals^2 )
SRR
K = nrow(as.matrix(mod9_5$coef))
Box.test2(mod9_5$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod9_5$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod9_5$residuals, lag = 24,type = "Ljung-Box", fitdf = K)

#AR(2)
mod9_5b = Arima(d2inf, order = c(2, 0, 0), include.mean = TRUE,
                seasonal = list(order = c(1, 0, 0), period = 2))
summary(mod9_5b)
SRR<- sum(mod9_5b$residuals^2 )
SRR
K = nrow(as.matrix(mod9_4$coef))
Box.test2(mod9_5b$residuals, lag = 8,type = "Ljung-Box", fitdf = K)
Box.test2(mod9_5b$residuals, lag = 16,type = "Ljung-Box", fitdf = K)
Box.test2(mod9_5b$residuals, lag = 24,type = "Ljung-Box", fitdf = K)

# O AIC indicou que o MA(1) é mais apropriado que o AR(1). O teste Ljung-Box do MA(1)
# mostrou que há autocorrelação nesse processo paa 8 e 24 defasagens. Para o modelo AR (1)
# temos autocorrelçaõ em todas as defasagens. Ja no modelo AR(2), temos resultados melhores em 
# todos os testes.

# iii)

# Por apresentarem autocorrelação para 8 e 24 defasagens,os modelos MA(1) e AR(1) não apresentam as 
# melhores propriedades de previsão. Em relação ao modelo AR (1) , temos autocorrelção para todas as
# defasagens. Como demonstrado na c) ii), o modelo AR(2) parece ser o mais apropriado

# iv) 

quart = read.csv2("quarterly.csv", sep=";", dec='.')
head(quart)
tail(quart)
quart = ts(quart, start = c(1960, 1), end = c(2012, 4), freq = 4)
inf = quart[ , "CPI"]
dinf = diff(log(inf))

#####################

n <- sum(!is.na(dinf)) #calcula o tamanho da amostra excluindo os NA's
n #211 observações

#Amostra possui 211 observações, menos 3 por conta dos AR(2), 209. 
#Vamos utilizar uma quantidade razoável, de 156,
#para testar a capacidade de cada modelo AR(2) => mod_dinf 
#em realizar previsões:


#Para replicar o exemplo do gráfico do livro:
dinf_base = window(dinf, start = c(1960, 1), end = c(2000, 2), freq = 4)
n <- sum(!is.na(dinf_base)) #calcula o tamanho da amostra excluindo os NA's
n #161 - 3, teremos até 158 observações

#AR(2)
mod9_dinf= Arima(dinf_base, order = c(2, 0, 0), include.mean = TRUE,
                seasonal = list(order = c(1, 0, 0), period = 12))
summary(mod9_dinf)

## Modelando vários passos a frente:

#Conforme vimos acima, temos uma amostra de 211 observações, separamos de 1960.1 a 2000.2:

NROW(dly)
NROW(dly_base)
211 - 161  #50
# Assim é ossivel fazer 50 previsões e comparar com as observações reais

prev_mod9<- numeric(50) #AR(2)

##Agora fazemos um laço for para calcular a previsão um passa a frente
#e mando substituir no vetor que eu criei acima:
#mod 9, considerando um AR(2):

mod9_dinf= Arima(dinf_base, order = c(2, 0, 0), include.mean = TRUE,
                seasonal = list(order = c(1, 0, 0), period = 12))
summary(mod9_dinf)


for(i in 1:50){
  mod9_dinf = Arima(dinf[1:(162+i-1)], order = c(2, 0, 0), include.mean = TRUE,
                   seasonal = list(order = c(1, 0, 0), period = 12))
  prev_mod9[i]<- forecast(mod9_dinf, h= 1)$mean
}

mean(prev_mod9)
var(prev_mod9)

summary(lm(dinf[163:212]~prev_mod9))

e9 = (prev_mod9 - dinf[163:212])
mean(e9)
var(e9)


#Ajuste das Previsões :
Ajuste_Modelo9 = accuracy((prev_mod9), dinf[163:212])
Ajuste_Modelo9

# Ao fazer o modelo com as previsões de 1 a 12 passos a frente de série cpi,
# vemos que ele é superior aos estimados na questão ii), via análise dos critérios AIC e BIC.

# 15)

quart = read.csv2("quarterly.csv", sep=";", dec='.')
head(quart)
tail(quart)
quart = ts(quart, start = c(1960, 1), end = c(2012, 4), freq = 4)

# a)

m1 = ((quart[,  "M1NSA"]))

plot(m1)

#Replicando o gráfico do Livro:

par(mar=c(5,4,4,5)+.1)
plot(diff(log(m1))*100, main = "The Level and Growth Rate of M1",
     ylab = "Rate of Growth (%)", xlim=c(1960, 2012),
     ylim = c(-3, 8))
par(new = T)
plot(m1, ylim=c(0,2500), xlim=c(1960, 2012), xlab='', 
     ylab='', xaxt = 'n', yaxt='n', col='black', 
     type = "l", lty =2, lwd = 2)
axis(4)
mtext('Billions of $',side=4,line=3) ##line=3, é qual próximo ela está do eixo direito
legend('bottomright', ncol=2, bty ="n",  col=c('black','black'), lty=c(1,2), lwd=c(1,2),
       legend=c('Rate of growth', 'M1 in billions'))


## Replicar os resultados das estimativas do livro:

m1 = window(m1, start = c(1962, 3), end = c(2008, 2), freq= 4)

#Obter ACF e PACF
acf_m1 <- acf(diff(log(m1)), lag.max = 22)
pacf_m1 <- pacf(diff(log(m1)), lag.max = 22)


## Fazer os gráficos como os do livro texto:


#Transformar em matrizes:
acf_m1_t<- matrix(acf_m1$acf, ncol=1)
pacf_m1_t<- matrix(pacf_m1$acf, ncol=1)

#Compatibilizar as saídas das duas matrizes ACF e PACF
m =matrix(1, nrow=1)
pacf_m1_t<- rbind(m, pacf_m1_t)

#Juntar na mesma matriz:
auto <- cbind(acf_m1_t, pacf_m1_t)

#Transpor para fazer o gráfico

auto_t <- t(auto)

### Estimando o intervalo de confiança:

dp = 2*1/(acf_m1$n.used)^(1/2)


#Rode este comando caso vc queira colocar os gráficos juntos
par(mfrow = c(2,1), mar = c(2, 4, 3, 2)) 


barplot(auto_t, beside=T, main="Panel a: M1 Growth", ylim = c(-0.4, 1),
        names.arg = rep(0:22) , cex.names = 0.8, xaxs = "i",
        ylab=" ",legend.text=c("Autocorrelations","PACF"),
        args.legend=list(x = "topright", bty="n", ncol = 2))

abline(h = 0)
grid(nx=NA, ny=NULL)

#Caso vc queira que as grades fiquem abaixo do gráfico, para melhorar
# a estética do gráfico, repita o o comando e adicione add = TRUE:
barplot(auto_t, beside=T, main="Panel a: M1 Growth", ylim = c(-0.4, 1),
        names.arg = rep(0:22) , cex.names = 0.8, xaxs = "i",
        ylab=" ",legend.text=c("Autocorrelations","PACF"),
        args.legend=list(x = "topright", bty="n", ncol = 2), add=TRUE)
abline( h = dp, lty = 2)
abline( h = - dp, lty = 2)


######

## Repetir todo o procedimento para a primeira diferença sazonal:

#Obter ACF e PACF
#Obter ACF e PACF

# criar a série de moeda em log e em primeira diferença
dm = diff(log(m1))

#lag:
l = 4

acf_m1 <- acf(diff(dm, lag = l), plot = FALSE)
pacf_m1 <- pacf(diff(dm, lag = l, lag.max = 22), plot =FALSE)

#Transformar em matrizes:
acf_m1_t<- matrix(acf_m1$acf, ncol=1)
pacf_m1_t<- matrix(pacf_m1$acf, ncol=1)

#Compatibilizar as saídas das duas matrizes ACF e PACF
m =matrix(1, nrow=1)
pacf_m1_t<- rbind(m, pacf_m1_t)

#Juntar na mesma matriz:
auto <- cbind(acf_m1_t, pacf_m1_t)

#Transpor para fazer o gráfico

auto_t <- t(auto)

par(mfrow = c(1,1))

barplot(auto_t, beside=T, main="Panel b: Seasonally Differenced M1 Growth", 
        ylim = c(-0.5, 1),
        names.arg = rep(0:22) , cex.names = 0.8, xaxs = "i",
        ylab=" ",legend.text=c("Autocorrelations","PACF"),
        args.legend=list(x = "topright", bty="n", ncol = 2))

abline(h = 0)
grid(nx=NA, ny=NULL)
abline( h = dp, lty = 2)
abline( h = - dp, lty = 2)
#grid(NA, 8, lwd = 2) # grid only in y-direction


#Caso vc queira que as grades fiquem abaixo do gráfico, para melhorar
# a estética do gráfico, repita o o comando e adicione add = TRUE:
barplot(auto_t, beside=T, main="Panel b: Seasonally Differenced M1 Growth", 
        ylim = c(-0.5, 1),
        names.arg = rep(0:22) , cex.names = 0.8, xaxs = "i",
        ylab=" ",legend.text=c("Autocorrelations","PACF"),
        args.legend=list(x = "topright", bty="n", ncol = 2), add=TRUE)
abline( h = dp, lty = 2)
abline( h = - dp, lty = 2)


## para voltar o gráfico ao normal:
par(mfrow = c(1,1))


## Foi rodado em Primeira Diferença, isto é indicado pelo número 1 no "ARIMA"

#Model 1: AR(1) with Seasonal MA
library(forecast)

mod1_m1 = Arima(log(m1), order = c(1, 1, 0),
                seasonal = list(order = c(0, 1, 1), period = 4), include.mean = TRUE)

mod1_m1

c1 <- c(NA,0, 0, 0, NA)
ARMA14 <- Arima(log(m1),order=c(1,1,4),fixed=c1, include.mean = TRUE)
ARMA14

#Ljung-Box test
#Ho: Ausência de Autocorrelação:

checkresiduals(mod1_m1, lag = 4, plot = FALSE)
# Faz:
#pchisq(Q, df)
#df = s (defasagem) - p - q 

pval <- 1-pchisq(1.4592, 2)
pval

#
checkresiduals(mod1_m1, lag = 8, plot = FALSE)
checkresiduals(mod1_m1, lag = 12, plot = FALSE)

summary(mod1_m1)

#Model 2: Multiplicative Autoregressive
mod2_m1 = Arima(log(m1), order = c(1, 1, 0),
                seasonal = list(order = c(1, 1, 0), period = 4), include.mean = TRUE)

summary(mod2_m1)

#Model 3: Multiplicative Moving Average
mod3_m1 = Arima(log(m1), order = c(0, 1, 1),
                seasonal = list(order = c(0, 1, 1), period = 4), include.mean = TRUE)
summary(mod3_m1)


## Ao que parece o primeiro modelo é o mais adequado, possui melhor ajuste
## Previsão 11 passoa a frente:
m1_prev<- forecast(mod1_m1, h = 11)

m1_base<- window(m1, start = c(2000, 1), freq=4)

plot(m1_base, xlim = c(2000, 2011), ylim = c(1050, 1500))
lines(exp(m1_prev$mean), type = "l", lty =2, lwd = 2)

# b) 

# O AR(1) sazonal com MA(1) apresentou valores menores, e dessa forma é mais apropriado

# c) 
m2 = ((quart[,  "M2NSA"]))
par(mfrow = c(1,1), mar = c(4,4,4,4))  
acf6 = acf(m2, main="ACF for m2")
acf6

# O resultado do ACF diz que devemos retirar a primeira diferença, tendo em vista que o m2 não é estacionário

# d) 

quart = read.csv2("quarterly.csv", sep=";", dec='.')
head(quart)
tail(quart)
quart = ts(quart, start = c(1960, 1), end = c(2012, 4), freq = 4)
m2 = (quart[,  "M2NSA"])
dm2 = diff(log(m2))
plot(dm2)

n <- sum(!is.na(dm2)) #calcula o tamanho da amostra excluindo os NA's
n #211 observações

#Para replicar o exemplo do gráfico do livro:
dm2_base = window(dm2, start = c(1962, 3), end = c(2002,3), freq = 4)
n <- sum(!is.na(dm2_base)) #calcula o tamanho da amostra excluindo os NA's
n #161 - 4, teremos até 157 observações

#AR(1,4) seasonal MA

mod1_dm2= Arima(dm2_base, order = c(1, 0, 0), include.mean = TRUE,
                seasonal = list(order = c(0, 0, 1), period = 4))
summary(mod1_dm2)

## Modelando vários passoa a frente:

#Conforme vimos acima, temos uma amostra de 211 observações, 
#se separarmos de 1960.1 a 2000.2
NROW(dm2)
NROW(dm2_base)
211 - 161  #50

#Então eu posso fazer 50 previsões e comparar com minhas observações reais:

prev_mod3<- numeric(50) #AR(1,4) seasonal MA
prev_mod4<- numeric(50) #MA(1,4) seasonal AR
prev_mod5<- numeric(50) #MA(1,4) seasonal MA

#mod1, considerando um AR(1,4) seasonal MA:

mod1_dm2= Arima(dm2_base, order = c(1, 0, 0), include.mean = TRUE,
                seasonal = list(order = c(0, 0, 1), period = 4))
summary(mod1_dm2)


for(i in 1:50){
  mod1_dm2 = Arima(dm2[1:(162+i-1)], order = c(1, 0, 0), include.mean = TRUE,
                   seasonal = list(order = c(0, 0, 1), period = 4))
  prev_mod3[i]<- forecast(mod1_dm2, h= 1)$mean
}

par(mfrow = c(2,1), mar = c(2,3,4,2))
plot(prev_mod3, main = "Previsão do Modelo AR(1,4)", type = "p", pch = 19, 
     ylim = c(-0.05,0.05))
lines(dm2[163:212], lwd = 2)


legend('topright', inset = 0.01, ncol = 2, col = c("black", "black"), 
       lwd = 2, pch = c(19, NA), lty = c(NA, 1), legend = c('Previsto', 'Real'), 
       cex=0.9)

mean(prev_mod3)
var(prev_mod3)

summary(lm(dm2[163:212]~prev_mod3))

e3 = (prev_mod3 - dm2[163:212])
mean(e3)
var(e3)

#MA(1,4) seasonal AR
for(i in 1:50){
  mod2_m2 = Arima(dm2[1:(162+i-1)], order = c(0, 0, 1), include.mean = TRUE)
  prev_mod4[i]<- forecast(mod2_m2, h= 1)$mean
}


plot(prev_mod4, main = "Previsão do Modelo MA(1,4)", type = "p", pch = 19, 
     ylim = c(-0.05,0.05))
lines(dm2[163:212], lwd = 2)


legend('topright', inset = 0.01, ncol = 2, col = c("black", "black"), 
       lwd = 2, pch = c(19, NA), lty = c(NA, 1), legend = c('Previsto', 'Real'), 
       cex=0.9)

mean(prev_mod4)
var(prev_mod4)

summary(lm(dm2[163:212]~prev_mod4))

e4 = (prev_mod4 - dm2[163:212])
mean(e4)
var(e4)

#MA(1,4) seasonal MA

for(i in 1:50){
  mod3_m2 = Arima(dm2[1:(162+i-1)], order = c(0, 0, 1), include.mean = TRUE)
  prev_mod5[i]<- forecast(mod3_m2, h= 1)$mean
}

plot(prev_mod5, main = "Previsão do Modelo MA(1,4)", type = "p", pch = 19, 
     ylim = c(-0.05,0.05))
lines(dm2[163:212], lwd = 2)


legend('topright', inset = 0.01, ncol = 2, col = c("black", "black"), 
       lwd = 2, pch = c(19, NA), lty = c(NA, 1), legend = c('Previsto', 'Real'), 
       cex=0.9)

mean(prev_mod5)
var(prev_mod5)

summary(lm(dm2[163:212]~prev_mod5))

e5 = (prev_mod5 - dm2[163:212])
mean(e5)
var(e5)

# A análise gráfica mostrou que as previsões são bem aproximadas.
cor(prev_mod1, prev_mod2)

### FORECAST ERROR and FORECAST ERROR VARIANCE
error.mod1 = (prev_mod1 - dly[163:212])
error.mod2 = (prev_mod2 - dly[163:212])
var(error.mod1)
var(error.mod2)

#Ajuste das Previsões :
Ajuste_Modelo1 = accuracy((prev_mod1), dly[163:212])
Ajuste_Modelo1
Ajuste_Modelo2 = accuracy((prev_mod2), dly[163:212])
Ajuste_Modelo2

# QUESTÃO 16

# a)

# Ha indícios que existe uma quebra estrutural em t = 100. Se o  for maior ou igual
# a 101, a dummy tem valor 1.5, o que reslta num salto para 2,5. O coeficiente AR(1)
# de 0.5 para 0.65

# b) e c) 
y_break = read.csv2("y_break.csv", sep=";", dec='.')
head(y_break)
y_break <- y_break[,-1]


par(mfrow= c(2,2))

plot(y_break, type = "l", lwd = 2,
     main = "Panel (a): The series",
     ylab = "")

y_break<- ts(y_break, start = 1)


library(dynlm)

mod1<- dynlm(y_break~lag(y_break,-1))

summary(mod1)

#Testando a estabilidade do Intercepto:

size <- 10 #quantos valore entram...

beta <- matrix(NA, ncol = 2, nrow = nrow(data.frame(y_break))-size)
sd <- matrix(NA, ncol = 2, nrow = nrow(data.frame(y_break))-size)

colnames(beta) <- c('intercep', 'coef')
colnames(sd) <- c('intercep', 'coef')


for (i in 1:nrow(sd)){
  quebra <- dynlm(y_break~lag(y_break,-1), end = start(y_break) + c(0, size + i - 1))
  
  beta[i,] <- c(coef(summary(quebra))[1,1], coef(summary(quebra))[2,1])
  
  sd[i,] <- c(coef(summary(quebra))[1,2], coef(summary(quebra))[2,2])
  
}


plot(beta[,1], type= "l", ylim = c(-2, 7), lwd = 2,
     ylab = "", main = "Panel (b): Intercept")
lines(beta[,1] + sd[,1]*2, type="l", lty =2, lwd = 2)
lines(beta[,1] - sd[,1]*2, type="l", lty =3)
abline(h=0, lty=1)
legend('topright',
       ncol=3, bty ="n",  col=c('black','black', 'black'), lty=c(1,2,3), lwd=c(1,2,1),
       legend=c('Intercept', '+ 2stds', "- 2stds"))

### AR(1)
plot(beta[,2], type= "l", ylim = c(-1.5, 2), lwd = 2,
     ylab = "", main = "Panel (c): AR(1) coeficient")
lines(beta[,2] + sd[,2]*2, type="l", lty =2, lwd = 2)
lines(beta[,2] - sd[,2]*2, type="l", lty =3)
abline(h=0, lty=1)
legend('topright',
       ncol=3, bty ="n",  col=c('black','black', 'black'), lty=c(1,2,3), lwd=c(1,2,1),
       legend=c('AR(1)', '+ 2stds', "- 2stds"))


################ Teste CUSUM:

mod1<- dynlm(y_break~lag(y_break,-1))

summary(mod1)


library(strucchange)
#?recresid
#A generic function for computing the recursive residuals 
#(standardized one step prediction errors) of a linear regression model.

prev_one <- recresid(mod1)
sigma <- sqrt(var(prev_one))

#Teste CUSUM


process <- cumsum(c(0, prev_one))/(sigma)
#Upper 5%:
N<- length(process)

Up5<- NA

T<- length(y_break)
n <- size

for(i in n:N){
  Up5[i]<- 0.948*((T-n)^0.5 +2*(i - 10)*(T - n)^(-0.5))
}

####
#Lower 5%:
Lw5<- NA

T<- length(y_break)
n <- size

for(i in n:N){
  Lw5[i]<- - 0.948*((T-n)^0.5 +2*(i - 10)*(T - n)^(-0.5))
}

#####


plot(process, type= "l", ylim = c(-40, 50),
     ylab = "", lwd = 2, main = "Panel (d): The CUSUM Test")
lines(Up5, type="l", lty =2, lwd = 2)
lines(Lw5, type="l", lty =3)
abline(h=0, lty=1)
legend('topright',
       ncol=3, bty ="n",  col=c('black','black', 'black'), lty=c(1,2,3), lwd=c(1,2,1),
       legend=c('CUSUMS', 'Upper 5%', "Lower 5%"))

par(mfrow= c(1,1))

length(y_break)

D0 = matrix(rep(0, 100))
D1 = matrix(rep(1, 50))
D = rbind(D0, D1)
length(D)

D = ts(D, start = 1, end = 150)

mod2<- dynlm(y_break~lag(y_break,-1) + D)

summary(mod2) 


mod3<- dynlm(y_break~lag(y_break,-1) + D + D*lag(y_break, -1))

summary(mod3) # In this particular case, the dummy variable indicate that there is
#a break but do not measure the size of the break very well.
#(#Note: the actual break in the intercept is + 1.5 and the actual break in
#the AR(1) coefficient is 0.15)

#### Os resultados conferem com os do livro.

### d) e e) ###

size <- 10

beta <- matrix(NA, ncol = 3, nrow = nrow(data.frame(y_break))-size)
sd <- matrix(NA, ncol = 3, nrow = nrow(data.frame(y_break))-size)

colnames(beta) <- c('intercep', 'coef1', 'coef2')
colnames(sd) <- c('intercep', 'coef1', 'coef2')


for (i in 1:nrow(sd)){
  quebra <- dynlm(y_break~lag(y_break,-1) + lag(y_break,-2), end = start(y_break) + c(0, size + i - 1))
  
  beta[i,] <- c(coef(summary(quebra))[1,1], coef(summary(quebra))[2,1], coef(summary(quebra))[3,1])
  
  sd[i,] <- c(coef(summary(quebra))[1,2], coef(summary(quebra))[2,2],  coef(summary(quebra))[3,2])
  
}

#Verifique que:
coef(summary(quebra))

### AR(2)
plot(beta[,3], type= "l", ylim = c(-1.5, 2), lwd = 2,
     ylab = "", main = "Panel (e): AR(2) coeficient")
lines(beta[,3] + sd[,3]*2, type="l", lty =2, lwd = 2)
lines(beta[,3] - sd[,3]*2, type="l", lty =3)
abline(h=0, lty=1)
legend('topright',
       ncol=3, bty ="n",  col=c('black','black', 'black'), lty=c(1,2,3), lwd=c(1,2,1),
       legend=c('AR(2)', '+ 2stds', "- 2stds"))

################ Teste CUSUM:

mod2<- dynlm(y_break~lag(y_break,-1) + lag(y_break,-2))

summary(mod2)


library(strucchange)
#?recresid
#A generic function for computing the recursive residuals 
#(standardized one step prediction errors) of a linear regression model.

prev_two <- recresid(mod2)
sigma <- sqrt(var(prev_two))

#Teste CUSUM


process2 <- cumsum(c(0, prev_two))/(sigma)
#Upper 5%:
N<- length(process)

Up5<- NA

T<- length(y_break)
n <- size

for(i in n:N){
  Up5[i]<- 0.948*((T-n)^0.5 +2*(i - 10)*(T - n)^(-0.5))
}

####
#Lower 5%:
Lw5<- NA

T<- length(y_break)
n <- size

for(i in n:N){
  Lw5[i]<- - 0.948*((T-n)^0.5 +2*(i - 10)*(T - n)^(-0.5))
}

#####


plot(process, type= "l", ylim = c(-40, 50),
     ylab = "", lwd = 2, main = "Panel (f): The CUSUM Test")
lines(Up5, type="l", lty =2, lwd = 2)
lines(Lw5, type="l", lty =3)
abline(h=0, lty=1)
legend('topright',
       ncol=3, bty ="n",  col=c('black','black', 'black'), lty=c(1,2,3), lwd=c(1,2,1),
       legend=c('CUSUMS', 'Upper 5%', "Lower 5%"))

# O AR(2) tem uma trajetória mais confinada ao intervalo de confiança, em relação ao AR(1), 
#de acordo com o valor do teste de CUSUM

# EXERCÍCIO 2 LISTA

## A) ##

#CAP. 4 ENDERS
#EXERCÍCIO 10

# a) 

quart = read.csv2("quarterly.csv", sep=";", dec='.')
head(quart)
tail(quart)

library(urca)

inp = quart[ , "IndProd"]
loginp = log(inp)

ers.test = ur.ers(loginp,lag=1,type="DF-GLS",model="trend")

ers.test

# Aqui, não há a necessidade de rejeitar a hipótese nula de estacionariedade
# portato, é uma série estacionária.

# b) 

un = quart [,"Unemp"]

ers.test = ur.ers(un,lag=8,type="DF-GLS",model="trend")

ers.test

# Não rejeitamos H0, indicando a não estacionariedade da série.

# c)

un = quart [,"Unemp"]

ers.test = ur.ers(un,lag=1,type="DF-GLS",model="trend")

ers.test

# Rejeitamos a hipótese nula de não estacionariedade. Logo, a série com uma defasagem
# é de fato estacionária


## B) ##
data1 = read.csv2("Panel.csv", sep=";", dec='.')
head(data1)

#Países Japan e UK
#Testes ADF, ERS, KPSS, Zivot e Andrews




