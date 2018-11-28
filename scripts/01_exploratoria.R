#Análise

#Carregando Pacotes
library(tidyverse)

#Leitura dos Dados
library(readr)
dados <- read_delim("dados/opendata_aig_brazil.csv", 
                  "~", 
                  escape_double = FALSE, 
                  trim_ws = TRUE)

names(dados)
head(dados)
glimpse(dados)
attach(dados)

#OBS: 201602011017592 tem 5 FC
#Dataset com fatores apenas e 
#eliminando ocorrencias sem fatores
fatores <- dados %>%
  filter(quantidade_fatores_contribuintes > 0) %>%
  select(starts_with('fator_')) %>%
  mutate_all(~ if_else(.x == 'SIM', 1, 0))



#Total de Acidentes
ggplot(dados) + 
  geom_bar(data = dados, mapping = aes(dados$ocorrencia_classificacao)) +
  xlab("Classificação") +
  ylab("Total")

#Total por Tipo de Motor
oco %>%
  ggplot() + 
  geom_bar(data=oco, mapping = aes(oco$aeronave_tipo_motor)) +
  xlab('Tipo do Motor') +
  ylab('Total')

#Total por Quantidade de Motores
ggplot(data=oco, aes(oco$aeronave_quantidade_motores, 
                     oco$quantidade_fatalidades,
                     colour = oco$aeronave_equipamento)
) +
  geom_point()




