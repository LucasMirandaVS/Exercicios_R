## Dividindo um dataframe de acordo com as datas
# Importando o df
# Coluna de datas deve estar em formato POSICXT
library(readxl)
library(data.table)
library(tidyverse)
library(tidyr)

# Importando os dados:
mensal_defl <- read_excel("C:/Users/Lucas/Desktop/tab.diss.anual/Nova pasta/mensal_defl.xlsx")

reduzido <- select(mensal_defl, 1,2,8,9,10,11) 
colnames(reduzido) <- c('mes', 'uf', 'pec', 'pep', 'pgc', 'pgp')

# Dividindo por estado
UF <- split(reduzido, reduzido$uf)$`MS`
#
# Definindo as datas que eu quero que sejam as separadoras
N = as.Date('2011-01-01') 
O = as.Date('2012-01-01')
P = as.Date('2013-01-01')
Q = as.Date('2014-01-01')
R = as.Date('2015-01-01')
S = as.Date('2016-01-01')
U = as.Date('2017-01-01')
V = as.Date('2018-01-01')
X = as.Date('2019-01-01')

# Dividindo os estados por datas
ano2010 <- filter(UF, mes < N)
ano2011 <- filter(UF, mes >= N, mes < O)
ano2012 <- filter(UF, mes >= O, mes < P)
ano2013 <- filter(UF, mes >= P, mes < Q)
ano2014 <- filter(UF, mes >= Q, mes < R)
ano2015 <- filter(UF, mes >= R, mes < S)
ano2016 <- filter(UF, mes >= S, mes < U)
ano2017 <- filter(UF, mes >= U, mes < V)
ano2018 <- filter(UF, mes >= V, mes < X)
ano2019 <- filter(UF, mes >= X)

# Calculando as  médias anuais 
media2010 <- as.data.frame(colMeans(ano2010[, c(-1, -2)]))
media2011 <- as.data.frame(colMeans(ano2011[, c(-1, -2)]))
media2012 <- as.data.frame(colMeans(ano2012[, c(-1, -2)]))
media2013 <- as.data.frame(colMeans(ano2013[, c(-1, -2)]))
media2014 <- as.data.frame(colMeans(ano2014[, c(-1, -2)]))
media2015 <- as.data.frame(colMeans(ano2015[, c(-1, -2)]))
media2016 <- as.data.frame(colMeans(ano2016[, c(-1, -2)]))
media2017 <- as.data.frame(colMeans(ano2017[, c(-1, -2)]))
media2018 <- as.data.frame(colMeans(ano2018[, c(-1, -2)]))
media2019 <- as.data.frame(colMeans(ano2019[, c(-1, -2)]))

# Unindo os valores anuais e exportando
medjunt <- cbind(media2010,media2011, media2012, media2013, media2014,
              media2015, media2016, media2017, media2018, media2019)

colnames(medjunt) <- c('2010', '2011', '2012', '2013', '2014',
                    '2015', '2016', '2017', '2018', '2019')

row.names(medjunt) <- c('pec', 'pep', 'pgc', 'pgp')

medjunt$var <- row.names(medjunt)

#
fwrite(medjunt, 'C:/Users/Lucas/Desktop/tab.diss.anual/ufs/magrossoSul.csv')
