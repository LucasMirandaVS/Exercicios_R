# Montando as séries de preço do painel da minha dissertação

library(plm)
library(tidyverse)
library(tidyr)
library(plyr)
library(readxl)
library(data.table)


# Primeiro, vou pegar os valores de 2013 a 2021
dataset2013_2021 <- read.csv("C:/Users/Lucas/Desktop/tab.diss.anual/Nova pasta/mensal-estados-desde-jan2013 (2).csv", header = TRUE)

# Agora pegando os valores para o etanol e gasolina
pe2013_2021 <- dataset2013_2021 %>%
  select(1,2,4,7,13) %>%
  filter(PRODUTO %in% c("ETANOL HIDRATADO")) %>%
  select(-PRODUTO) %>%
  arrange(ESTADO) %>%
  mutate(ESTADO = recode(ESTADO, "ACRE" = "AC", "ALAGOAS" = "AL", "AMAPA" = "AP",
                         "AMAZONAS" = "AM", "BAHIA" = "BA", "CEARA" = "CE",
                         "DISTRITO FEDERAL" = "DF", "ESPIRITO SANTO" = "ES", "GOIAS" = "GO",
                         "MARANHAO" = "MA", "MATO GROSSO" = "MT", "MATO GROSSO DO SUL" = "MS",
                         "MINAS GERAIS" = "MG", "PARA" = "PA", "PARANA" = "PR",
                         "PARAIBA" = "PB", "PERNAMBUCO" = "PE", "PIAUI" = "PI",
                         "RIO DE JANEIRO" = "RJ", "RIO GRANDE DO SUL" = "RS", "RIO GRANDE DO NORTE" = "RN",
                         "RONDONIA" = "RO", "RORAIMA" = "RR", "SANTA CATARINA" = "SC",
                         "SAO PAULO" = "SP", "SERGIPE" = "SE", "TOCANTINS" = "TO")) 
names(pe2013_2021)[names(pe2013_2021) == 'PREÇO MÉDIO REVENDA'] <- 'PEC'
names(pe2013_2021)[names(pe2013_2021) == 'PREÇO MÉDIO DISTRIBUIÇÃO'] <- 'PEP'

fwrite(pe2013_2021, "C:/Users/Lucas/Desktop/tab.diss.anual/Nova pasta/eta13.csv")

PG_2013_2021 <- dataset2013_2021 %>%
  select(1,2,4,7,13) %>%
  filter(PRODUTO %in% c('GASOLINA COMUM')) %>%
  select(-PRODUTO) %>%
  arrange(ESTADO) %>%
  mutate(ESTADO = recode(ESTADO, "ACRE" = "AC", "ALAGOAS" = "AL", "AMAPA" = "AP",
                         "AMAZONAS" = "AM", "BAHIA" = "BA", "CEARA" = "CE",
                         "DISTRITO FEDERAL" = "DF", "ESPIRITO SANTO" = "ES", "GOIAS" = "GO",
                         "MARANHAO" = "MA", "MATO GROSSO" = "MT", "MATO GROSSO DO SUL" = "MS",
                         "MINAS GERAIS" = "MG", "PARA" = "PA", "PARANA" = "PR",
                         "PARAIBA" = "PB", "PERNAMBUCO" = "PE", "PIAUI" = "PI",
                         "RIO DE JANEIRO" = "RJ", "RIO GRANDE DO SUL" = "RS", "RIO GRANDE DO NORTE" = "RN",
                         "RONDONIA" = "RO", "RORAIMA" = "RR", "SANTA CATARINA" = "SC",
                         "SAO PAULO" = "SP", "SERGIPE" = "SE", "TOCANTINS" = "TO")) 
names(PG_2013_2021)[names(PG_2013_2021) == 'PREÇO MÉDIO REVENDA'] <- 'PGC'
names(PG_2013_2021)[names(PG_2013_2021) == 'PREÇO MÉDIO DISTRIBUIÇÃO'] <- 'PGP'

fwrite(PG_2013_2021, "C:/Users/Lucas/Desktop/tab.diss.anual/Nova pasta/gas13.csv")

# Achei os preços de distribuição de janeiro de 2010 a dezembro de 2012
dataset2001_2012 <- read.csv("C:/Users/Lucas/Desktop/tab.diss.anual/Nova pasta/mensal-estados-2001-a-2012.csv", header = TRUE)

#Agora pegando os valores pra gasolina e etanol
PE2001_2012 <- dataset2001_2012 %>%
  select(1,2,4,7,13) %>%
  filter(PRODUTO %in% c("ETANOL HIDRATADO")) %>%
  select(-PRODUTO) %>%
  arrange(ESTADO) %>%
  mutate(ESTADO = recode(ESTADO, "ACRE" = "AC", "ALAGOAS" = "AL", "AMAPA" = "AP",
                         "AMAZONAS" = "AM", "BAHIA" = "BA", "CEARA" = "CE",
                         "DISTRITO FEDERAL" = "DF", "ESPIRITO SANTO" = "ES", "GOIAS" = "GO",
                         "MARANHAO" = "MA", "MATO GROSSO" = "MT", "MATO GROSSO DO SUL" = "MS",
                         "MINAS GERAIS" = "MG", "PARA" = "PA", "PARANA" = "PR",
                         "PARAIBA" = "PB", "PERNAMBUCO" = "PE", "PIAUI" = "PI",
                         "RIO DE JANEIRO" = "RJ", "RIO GRANDE DO SUL" = "RS", "RIO GRANDE DO NORTE" = "RN",
                         "RONDONIA" = "RO", "RORAIMA" = "RR", "SANTA CATARINA" = "SC",
                         "SAO PAULO" = "SP", "SERGIPE" = "SE", "TOCANTINS" = "TO")) 
names(PE2001_2012)[names(PE2001_2012) == 'PRECO MÉDIO REVENDA'] <- 'PEC'
names(PE2001_2012)[names(PE2001_2012) == 'PRECO MÉDIO DISTRIBUIÇÃO'] <- 'PEP'

fwrite(PE2001_2012, "C:/Users/Lucas/Desktop/tab.diss.anual/Nova pasta/eta01.csv")


PG_2001_2012 <- dataset2001_2012 %>%
  select(1,2,4,7,13) %>%
  filter(PRODUTO %in% c('GASOLINA COMUM')) %>%
  select(-PRODUTO) %>%
  arrange(ESTADO) %>%
  mutate(ESTADO = recode(ESTADO, "ACRE" = "AC", "ALAGOAS" = "AL", "AMAPA" = "AP",
                         "AMAZONAS" = "AM", "BAHIA" = "BA", "CEARA" = "CE",
                         "DISTRITO FEDERAL" = "DF", "ESPIRITO SANTO" = "ES", "GOIAS" = "GO",
                         "MARANHAO" = "MA", "MATO GROSSO" = "MT", "MATO GROSSO DO SUL" = "MS",
                         "MINAS GERAIS" = "MG", "PARA" = "PA", "PARANA" = "PR",
                         "PARAIBA" = "PB", "PERNAMBUCO" = "PE", "PIAUI" = "PI",
                         "RIO DE JANEIRO" = "RJ", "RIO GRANDE DO SUL" = "RS", "RIO GRANDE DO NORTE" = "RN",
                         "RONDONIA" = "RO", "RORAIMA" = "RR", "SANTA CATARINA" = "SC",
                         "SAO PAULO" = "SP", "SERGIPE" = "SE", "TOCANTINS" = "TO")) 
names(PG_2001_2012)[names(PG_2001_2012) == 'PREÇO MÉDIO REVENDA'] <- 'PGC'
names(PG_2001_2012)[names(PG_2001_2012) == 'PREÇO MÉDIO DISTRIBUIÇÃO'] <- 'PGP'

fwrite(PG_2001_2012, "C:/Users/Lucas/Desktop/tab.diss.anual/Nova pasta/gas01.csv")

