###############################################################################-
# Monitoria 4 - Econometria (MP) FGV-Rio - 2025
# Professor: Valdemar Pinho Neto
# Monitor: Vinícius Schuabb
# Macroeconometria
###############################################################################-
#------------------------------------------------------------------------------#
# Nome: Jader Brenny Santana
# ID na FGV: 253204033
#------------------------------------------------------------------------------#

# Preambulo para todo script:

rm(list = ls()) 
gc() 
getwd()

library(tidyverse)
library(ggplot2)
library(tseries)
library(forecast)

set.seed(1234)

#############################-
## IPEA Data - Análise Macro
#############################-


### Desomposição de uma série #####

# exercicio 1
# Item b)

# Do que se trata a série analisada? 
# O saldo refere-se a diferença entre o total de admissões e dispensa de empregados, sob o regime da Consolidação das Leis do Trabalho - CLT

dados_emp <- read.csv("ipeadata[24-03-2025-03-33].csv", sep = ";") # carregando os dados - Mude o nome do arquivo para o que você baixou do IpeaData
dados_emp$X <- NULL # veio uma coluna vazia, eliminando-a atribuindo valores nulos
colnames(dados_emp) <- c("Data", "saldo_emprego") # renomeando as colunas
dados_emp <- dados_emp[dados_emp$Data>=2000.01 & # selecionando apenas entre 2000.01 e 2019.12
                         dados_emp$Data<=2019.12,]

help(ts) #- atribuindo o caráter temporal para a análise de séries de tempo no R
saldo_emprego <- ts(dados_emp$saldo_emprego, frequency = 12, start = c(2000, 1), end=c(2019, 12))

# Quando se deu o mínimo (ano e mês)?
dados_emp[dados_emp$saldo_emprego==min(saldo_emprego),]

png("saldo_emprego_2000_2019.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
plot(saldo_emprego, main='Saldo de Empregos Formais no Brasil', xlab='', ylab='Empregos (saldo)')
dev.off() # Exporta o gráfico png para o diretório conforme o png()

# Item c)

m <- decompose(saldo_emprego)

png("decomposicao_saldo_emprego.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
plot(m) # grafico
dev.off() # Exporta o gráfico png para o diretório conforme o png()

# Quando se deu o mínimo da tendência (ano e mês)?

dados_emp$tendencia <- m$trend
dados_emp[!is.na(dados_emp$tendencia)&
            dados_emp$tendencia==min(m$trend, na.rm = T),]

# Item d) Responda você a partir das diferenças que observamos entre os itens b) e c).


### 
# exercicio 2

# Item a)

# Baixando junto os dados

dados <- read.csv("ipeadata[24-03-2025-04-00].csv", sep = ";") # carregando os dados# carregando os dados - Mude o nome do arquivo para o que você baixou do IpeaData
dados$X <- NULL # veio uma coluna vazia, eliminando-a atribuindo valores nulos
colnames(dados) <- c("Data","Cambio","Preco","Inflacao") # renomeando as colunas
dados <- dados[dados$Data>=2000.01 & dados$Data<=2023.12,] # selecionando apenas entre 2000.01 e 2023.12

dados$Cambio <- as.numeric(sub(",", ".", dados$Cambio, fixed = TRUE)) # substituindo o separador decimal de virgula para ponto
dados$Preco <- as.numeric(sub(",", ".", dados$Preco, fixed = TRUE)) # substituindo o separador decimal de virgula para ponto
dados$Inflacao <- as.numeric(sub(",", ".", dados$Inflacao, fixed = TRUE)) # substituindo o separador decimal de virgula para ponto

cambio <- dados[,c("Data","Cambio")]
precos <- dados[,c("Data","Preco")]
inflacao <- dados[,c("Data","Inflacao")]

colnames(cambio) <- c("Data", "Cambio")
colnames(precos) <- c("Data", "IPCA")
colnames(inflacao) <- c("Data", "Var_IPCA")

# Definindo como séries de tempo:

cambio <- ts(cambio$Cambio, frequency = 12, start = c(2000, 1), end=c(2023, 12))
precos <- ts(precos$IPCA, frequency = 12, start = c(2000, 1), end=c(2023, 12))
inflacao <- ts(inflacao$Var_IPCA, frequency = 12, start = c(2000, 1), end=c(2023, 12))

# Item b)

# Gerando os gráficos

png("cambio_2000_2023.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
plot(cambio,  main = "Taxa de Cambio", xlab="Ano", ylab="US$/R$") # grafico
dev.off() # Exporta o gráfico png para o diretório conforme o png()

png("precos_2000_2023.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
plot(precos, main = "IPCA - Indice", xlab='', ylab="Nivel de Preços") # grafico
dev.off() # Exporta o gráfico png para o diretório conforme o png()

png("inflacao_2000_2023.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
plot(inflacao, main = "Inflação - IPCA", xlab='', ylab="Variação dos de Preços") # grafico
dev.off() # Exporta o gráfico png para o diretório conforme o png()

png("dif_inflacao_2000_2023.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
plot(diff(precos), main  = "Série diferenciada - IPCA - Indice", xlab = "", ylab =  "") # grafico
dev.off() # Exporta o gráfico png para o diretório conforme o png()


# item c)

# Teste de estacionariedade de Dickey-Fuller

help(adf.test)

adf.test(cambio)
adf.test(precos)
adf.test(inflacao)
adf.test(diff(precos))

# item d) Quais tem características de uma série estacionária?

png("acf_cambio.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
acf(cambio, main = "Função de Auto-correlação da Taxa de Câmbio")
dev.off() # Exporta o gráfico png para o diretório conforme o png()

png("pacf_cambio.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
pacf(cambio, main = "Função de Auto-correlação Parcial da Taxa de Câmbio")
dev.off() # Exporta o gráfico png para o diretório conforme o png()

png("acf_precos.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
acf(precos, main = "Função de Auto-correlação do IPCA")
dev.off() # Exporta o gráfico png para o diretório conforme o png()

png("pacf_precos.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
pacf(precos, main = "Função de Auto-correlação Parcial do IPCA")
dev.off() # Exporta o gráfico png para o diretório conforme o png()

png("acf_inflacao.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
acf(inflacao, main = "Função de Auto-correlação da Inflação-IPCA")
dev.off() # Exporta o gráfico png para o diretório conforme o png()

png("pacf_inflacao.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
pacf(inflacao, main = "Função de Auto-correlação Parcial da Inflação-IPCA")
dev.off() # Exporta o gráfico png para o diretório conforme o png()

png("acf_diff_precos.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
acf(diff(precos), main = "Função de Auto-correlação da Differença-IPCA")
dev.off() # Exporta o gráfico png para o diretório conforme o png()

png("pacf_diff_precos.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
pacf(diff(precos), main = "Função de Auto-correlação Parcial da Differença-IPCA")
dev.off() # Exporta o gráfico png para o diretório conforme o png()


# item e)

### Previsão #### pacote forecast

precos.auto <- auto.arima(inflacao, max.p = 5, max.q = 5,
                          seasonal = F)
summary(precos.auto)

mod.arima <- auto.arima(inflacao, seasonal = FALSE)
summary(mod.arima)

arima_plot <- mod.arima |>
  forecast(h = 12)

png("arima_plot.png",width=8,height=6,units="in",res=250) # Forma de salvar gráfico via plot
plot(arima_plot, main = "Modelo ARIMA")
dev.off() # Exporta o gráfico png para o diretório conforme o png()

