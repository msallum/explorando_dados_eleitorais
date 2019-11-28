library(tidyverse)
library(stringr)
library(FactoMineR)

tse_18= read_csv('C:/users/josez/Desktop/Economia/Politica Brasil/explorando_dados_eleitorais/dados/dados_formatados/treino_tse_2018.csv')

# Análise PCA. Objetivo: Averiguar se a political compass é legit
tse_18_votos_1t <- tse_18 %>%
  filter(NR_TURNO == 1) %>%
  select('ALVARO FERNANDES DIAS':'VOTO NULO') %>%
  replace(is.na(.), 0)

pca = PCA(tse_18_votos_1t)
print(pca)
summary(pca)
# O primeiro fator, que explica 72% da variação, tem muita cara de ser só o tamanho de cada zona eleitoral disfarçado
# O segundo, epxlicando 7%, tem cara de ter mais a ver com o objetivo
# Tentaremos filtrar o efeito tamanho pondo todas as variáveis em % da população total da zona
# Corremos riscos com multicolinearidade?

tse_18_votos_1t_pc <- tse_18 %>%
  filter(NR_TURNO == 1) %>%
  select(ELEITORES, 'ALVARO FERNANDES DIAS':'VOTO NULO') %>%
  replace(is.na(.), 0) %>%
  mutate_at(vars(-ELEITORES), funs(./ ELEITORES)) %>%
  select(-ELEITORES)
pca_pc <- PCA(tse_18_votos_1t_pc)
summary(pca)
# Resultados muito mais interessantes. Nosso palpite anterior provavelmente estava certo, dadas as mudanças observadas
# Agora, claramente o eixo Y define esquerda/direita. A interpretação do X é menos trivial. Uma primeira intuição
# Pode ser que indique "nível de antipetismo", mas é forçado.
# A informação do dataframe também ficou muito menos concentrada; onde antes apenas o tamanho de cada cidade
# respondia, provavelmente, por 72% da informação, agora o PC1, de interpretação incerta, responde por apenas 24.7%
# O PC2, suspeito de ser posição econômica, responde por 15.4%