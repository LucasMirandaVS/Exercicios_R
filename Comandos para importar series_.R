################### Importando dados diretos de sites

library(Quandl)
library(dygraphs)

################# base de dados BCB  - pacote "Quandl"
####              gerenciador de séries do BACEN


# Coletar o dado do IPCA. Observe que adicionamos BCB/ ao código da série temporal. Sempre usaremos BCB/ 
# para coletar dados do BACEN por meio do Quandl. Ele tem o significado de determinar de qual banco de 
# dados o Quandl deve buscar pela série que o número definido. Como padrão o Quandl coletará os dados na
# periodicidade divulgada pelo BACEN.
ipca = Quandl('BCB/433')
# Coletar a mesma informação para um período específico
ipca = Quandl('BCB/433', start_date = "1996-01-01", end_date = "2017-12-31")
# Coletar definindo apenas a data inicial 
ipca = Quandl('BCB/433', start_date = "1996-01-01")
# Coletar definindo a periodicidade de interesse
# Opções: daily, weekly, monthly, quarterly, annual
ipca = Quandl("BCB/433", collapse = "quarterly", start_date = "1996-01-01")
# Coletar fazendo alterações nos dados. Transformações nos dados permitidas pelo Quandl:
# - diff: z[t] = y[t] - y[t-1] (diferença)
# - rdiff: z[t] = (y[t] - y[t-1]) / y[t-1] (diferença %)
# - rdiff_from: z[t] = (y[latest] - y[t]) / y[t] (incremento % em relação à última observação)
# - cumul:  z[t] = y[0] + y[1] + . + y[t] (soma acumulativa)
# - normalize: z[t] = y[t] ÷ y[0] * 100 (série iniciar em 100)
ipca = Quandl("BCB/433", transform = "diff", start_date = "1996-01-01")
# Coletar definido o tipo de dado que queremos no R
# - ts: série temporal
# - zoo: objeto zoo 
# - xts: no formato xts
# Detalhes sobre a diferença entre os tipos no link abaixo
# https://stackoverflow.com/questions/33714660/what-is-the-difference-the-zoo-object-and-ts-object-in-r
ipca = Quandl("BCB/433", start_date = "1996-01-01", type = "xts")
# Alterar o nome da coluna do objeto para IPCA
colnames(ipca)="IPCA"
############## salvando como série temporal
ipca2 = Quandl("BCB/433", start_date = "1996-01-01", type = "ts")
# Visualizar os dados usando o pacote dygraphs. Mais detalhes em
# https://rstudio.github.io/dygraphs/
dygraphs::dygraph(ipca2, main = "Índice Nacional de Preços ao Consumidor-Amplo (IPCA)") %>% dyRangeSelector()





########################## dados financeiros - yahoo e google finanças e FRED: Federal Reserve Bank of St. Louis - pacote quantmod" 
############## link para compreender as funcionalidades e exemplos    https://www.quantmod.com/examples/data/

library(quantmod)
suppressMessages(require(PerformanceAnalytics))

############ Acessar o site do Yahoo Finance (https://finance.yahoo.com/), escolher uma ação de interesse e seu código. Por exemplo:
############ a ação da Vale negociada na BM&F BOVESPA que tem o código VALE3.SA. Atenção para o caso de ações negociadas em várias bolsas. Ao pesquisar pelo nome da empresa aparecerá a bolsa na qual ela está sendo negociada e você deve escolher para a bolsa que quer coletar os dados.


# Coletar os dados da VALE3.SA do Yahoo Finance. Temos as seguintes opções:
# - google: Google Finance
# - FRED: Federal Reserve Bank of St. Louis
# A opção auto.assign define se os dados devem ser incorporados no R com o nome
# do symbol ou um nome específico (auto.assign = FALSE). No nosso caso, optamos
# pelo nome vale.
vale = quantmod::getSymbols("VALE3.SA", src = "yahoo", auto.assign = FALSE)
# Coletar os dados para um período específico
vale = quantmod::getSymbols("VALE3.SA", src = "yahoo", auto.assign = FALSE, from = '2015-01-01', to = '2016-12-31')
# Coletar os dados de uma data específica até a última observação disponível sobre a ação
vale = quantmod::getSymbols("VALE3.SA", src = "yahoo", auto.assign = FALSE, from = '2018-01-01')
# Coletar definido o tipo de dado que queremos no R
# - ts: série temporal
# - zoo: objeto zoo 
# - xts: no formato xts
vale = quantmod::getSymbols("VALE3.SA", src = "yahoo", auto.assign = FALSE, from = '2017-01-01', return.class = 'xts')
# Formato da saída
knitr::kable(head(vale), align = "c")

## Open: O preço de abertura nas datas especificadas
## High: O preço da alta nas datas especificadas
## Low: O preço da baixa nas datas especificadas
## Close: O preço de fechamento nas datas especificadas
## Volume: O volume nas datas especificadas
## Adjusted: O preço de fechamento ajustado depois de aplicar distribuições de dividendos ou divisão da ação.


# Calcular o log-retorno diário usando o log(p_t) - log(p_t-1). 
# Outra opção é o retorno diário por meio da opção method = "discrete"
daily_return = PerformanceAnalytics::Return.calculate(vale$VALE3.SA.Close, method = "log")
# Alterar o nome da coluna do objeto para VALE3.SA
colnames(daily_return)="VALE3.SA"
# Visualizar os dados usando o pacote dygraphs. Mais detalhes em
dygraphs::dygraph(daily_return, main = "Retorno Diário da VALE3.SA") %>% dyRangeSelector()





