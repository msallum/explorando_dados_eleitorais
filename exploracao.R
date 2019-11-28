library(tidyverse)
library(stringr)
library(FactoMineR)

tse_18= read_csv('C:/users/josez/Desktop/Economia/Politica Brasil/explorando_dados_eleitorais/dados/dados_formatados/treino_tse_2018.csv')

tse_18 <- select(tse_18, -'VOTO ANULADO E APURADO EM SEPARADO')

tse_18_votos_1t <- tse_18 %>%
  filter(NR_TURNO == 1) %>%
  select('ALVARO FERNANDES DIAS':'VOTO NULO') %>%
  replace(is.na(.), 0)

pca = PCA(tse_18_votos_1t, scale.unit = TRUE, ncp=5)
print(pca)
