# Imports
library(quantmod)
library(ggplot2)
library(forecast)
library(tseries)
library(rugarch)
library(prophet)
library(tsfknn)

# Pegando os dados
getSymbols("^GSPC",src="yahoo",from="2015-01-01",to = "2022-06-04")

date = index(GSPC)
date = as.Date(date)
head(GSPC)

# Visualizando os dados
chartSeries(GSPC,TA = NULL)
chartSeries(GSPC,TA=c(addVo(),addBBands(),addMACD())) # com bandas de bolinger e MACD

#ADF TEST 
print(adf.test(GSPC$GSPC.Close))
# p-valor =< 0.05 = série estacionária
# p-valor > 0.05 = série tem raiz unitária

#Plot ACF and PACF
par(mfrow = c(1, 2))
acf(GSPC$GSPC.Close)
pacf(GSPC$GSPC.Close)
par(mfrow = c(1, 1))
#dev.off()

## Applicando auto.arima() 
modelfit <-auto.arima(GSPC$GSPC.Close, lambda = "auto")
summary(modelfit)

# Diagnostico dos residuos
plot(resid(modelfit),ylab="Residuals",main="Resíduos (Arima(2,1,0)) vs. Tempo")

# Histogram of Residuals & Normality Assumption
hist(resid(modelfit),freq=F,ylim=c(0,9500),main="Histograma dos Resíduos")
e=resid(modelfit)
curve(dnorm(x, mean=mean(e), sd=sd(e)), add=TRUE, col="darkred")

# Diagnostics for Arima
tsdiag(modelfit)

# Box test for lag=2
Box.test(modelfit$residuals, lag= 2, type="Ljung-Box")

# generalized box test
Box.test(modelfit$residuals, type="Ljung-Box")

# Plot do gráfico
plot(as.ts(GSPC$GSPC.Close))
lines(modelfit$fitted,col="red")

# Prevendo os próximos 30 dias
plot(forecast(modelfit,h=30))

# Propriedades da previsão
price_forecast <- forecast(modelfit,h=30)
plot(price_forecast)
head(price_forecast$mean)

# Dividing the data into train & test sets , Applying the model
N = length (GSPC$GSPC.Close)
n = 0.8*N
train = GSPC$GSPC.Close[1:n, ]
test = GSPC$GSPC.Close[(n+1):N,]
trainarimafit <- auto.arima(train$GSPC.Close ,lambda= "auto")
summary(trainarimafit)
predlen= length(test)
trainarima_fit <- forecast(trainarimafit, h= predlen)

#Plotting mean predicted  values vs real data
meanvalues<- as.vector(trainarima_fit$mean)
precios <- as.vector(test$GSPC.Close)
plot(meanvalues, type = "l",col="red")
lines(precios, type = "l")