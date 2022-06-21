# Analisando o painel
library(plm)
library(data.table)
library(dplyr)

P.ANUAL <- read.csv("C:/Users/Lucas/Desktop/pesquisa/P.ANUAL.csv")

P.ANUAL$Y<-as.numeric(gsub(",",'.', P.ANUAL$Y))
summary(P.ANUAL)

# Agora, o painel:
data_panel<- pdata.frame(P.ANUAL, index=c('UF',"ANO")) %>%
  arrange(UF)

summary(data_panel)

#verificando se o painel e balanceado
pdim(data_panel)

#verificando a variacao das variaveis no tempo e individuos
pvar(data_panel)

#######################################################
### ESTIMADOR DE EFEITOS FIXOS E PRIMEIRA DIFERENCA ###
#######################################################
modelo1 <- plm(log(QE)~log(PEC)+
                 log(PGC)+
                 log(TxF)+
                 log(Y)+
                 factor(ANO)+
                 log(PEC)*D1+
                 log(PGC)*D1+
                 log(PEC)*D2+
                 log(PGC)*D2,
               data = data_panel,
               model = 'within')
summary(modelo1)
#######################################
### ESTIMADOR DE EFEITOS ALEATORIOS ###
#######################################
modelo1_1 <- plm(log(QE)~log(PEC)+
                   log(PGC)+
                   log(TxF)+
                   log(Y)+
                   factor(ANO)+
                   log(PEC)*D1+
                   log(PGC)*D1+
                   log(PEC)*D2+
                   log(PGC)*D2,
                 data = data_panel,
                 model = 'random')
summary(modelo1_1)

# Testes estatísticos
# chow - heterogeneidade entre os estados
# hausman - indica efeito fixo ou variável
# wooldridge - autocorrelação
# Wald - heteroscedasticidade
########################
### TESTE DE HAUSMAN ###
########################
#H0: O modelo de efeitos aleatorios e consistente
phtest(modelo1,modelo1_1)	#indicou o efeito fixo

#####################
### TESTE DE CHOW ###
#####################
form <- log(QE) ~ log(PEC) + log(PGC) + log(TxF) + log(Y) + log(PEC) * 
  D1 + log(PGC) * D1 + log(PEC) * D2 + log(PGC) * D2

pooltest(form, data = P.ANUAL, model = "within")

# indicou o modelo OLS com efeito fixo

###########################
### TESTE DE WOOLDRIDGE ###
###########################
pwartest(modelo1) # O teste indicou a presença de autocorrelação

#####################
### TESTE DE WALD ###
#####################
pwaldtest(modelo1) #indicou a presença de heteroscedasticidade

#########
# Corrigindo o problema com os erros padrão de Driscoll-Kraay
lmtest::coeftest(modelo1,
                 vcov. = function(x) vcovSCC(x, type = "HC0"))

# Esses são os resultados que devem ser interpretados

#####
# Otimizando o modelo
# Aplicando uma coluna dummy para cada estado produtor
# SP
P.ANUAL$dsp <- 0
P.ANUAL$dsp[P.ANUAL$UF == 'SP'] <- 1
# PR
P.ANUAL$dpr <- 0
P.ANUAL$dpr[P.ANUAL$UF == 'PR'] <- 1
# MT
P.ANUAL$dmt <- 0
P.ANUAL$dmt[P.ANUAL$UF == 'MT'] <- 1
# MG
P.ANUAL$dmg <- 0
P.ANUAL$dmg[P.ANUAL$UF == 'MG'] <- 1
# GO
P.ANUAL$dgo <- 0
P.ANUAL$dgo[P.ANUAL$UF == 'GO'] <- 1
# PE
P.ANUAL$dpe <- 0
P.ANUAL$dpe[P.ANUAL$UF == 'PE'] <- 1
# MS
P.ANUAL$dms <- 0
P.ANUAL$dms[P.ANUAL$UF == 'MS'] <- 1

painel <- pdata.frame(P.ANUAL, index=c('UF',"ANO")) %>%
  arrange(UF)

# agora vou refazer as estimações
##estimador de efeitos fixos (versão completa)
modelo2 <- plm(log(QE)~
                 log(PEC)+log(PGC)+log(TxF)+log(Y)+factor(ANO)+
                            log(PEC)*D1+log(PGC)*D1+
                            log(PEC)*dsp+ log(PGC)*dsp+
                            log(PEC)*dpr+ log(PGC)*dpr+
                            log(PEC)*dmt+ log(PGC)*dmt+
                            log(PEC)*dms+ log(PGC)*dms+
                            log(PEC)*dmg+ log(PGC)*dmg+
                            log(PEC)*dgo+ log(PGC)*dgo+
                            log(PEC)*dpe+log(PGC)*dpe,
                          data = painel, model = 'within')
summary(modelo2) 

# Coeficientes com erros padrão driscol-kraay

lmtest::coeftest(modelo2, vcov. = function(x) vcovSCC(x, type = "HC0"))
# Esses são os resultados que devem ser interpretados

# Nonparametric robust covariance matrix estimators a la Driscoll and Kraay 
matrizDK2 <- vcovSCC(modelo2, 
                     type = "HC0",
                     cluster = "time",
                     maxlag = NULL,
                     inner = "cluster")