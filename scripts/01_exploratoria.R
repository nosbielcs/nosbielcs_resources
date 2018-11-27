#Análise

#Carregando Pacotes
library(ggplot2)
library(magrittr)
library(dplyr)
library(forcats)
library(readr)
library(tidyr)

#Leitura dos Dados
library(readr)
dados <- read_delim("dados/opendata_aig_brazil.csv", 
                  "~", 
                  escape_double = FALSE, 
                  trim_ws = TRUE)

names(dados)
head(dados)
attach(dados)

#Total de Acidentes
ggplot(dados) + 
  geom_bar(data = dados, mapping = aes(dados$ocorrencia_classificacao)) +
  xlab("Classificação") +
  ylab("Total")


