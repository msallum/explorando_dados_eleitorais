library(tidyverse)
library(stringr)
library(FactoMineR)

tse_18= read_csv('C:/users/josez/Desktop/Economia/Politica Brasil/explorando_dados_eleitorais/dados/dados_formatados/treino_tse_2018.csv')

# Análise PCA. Objetivo: Averiguar se a political compass é legit
tse_18_votos_1t <- tse_18 %>%
  filter(NR_TURNO == 1) %>%
  select('ALVARO FERNANDES DIAS':'VOTO NULO') %>%
  replace(is.na(.), 0)

head(tse_18_votos_1t)
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
summary(pca_pc)
coords = pca_pc$var$coord



# Resultados muito mais interessantes. Nosso palpite anterior provavelmente estava certo, dadas as mudanças observadas
# Agora, claramente o eixo Y define esquerda/direita. O eixo X, por si, define muito claramente Nordeste vs Não Nordeste
# Interessantemente, "Brancos" está bem distantte de "Nulos" em ambos os eixos (ambos no quadrante não Nordeste de Esquerda)
# A informação do dataframe também ficou muito menos concentrada; onde antes apenas o tamanho de cada cidade
# respondia, provavelmente, por 72% da informação, agora o PC1, de interpretação incerta, responde por apenas 24.7%
# O PC2, suspeito de ser posição econômica, responde por 15.4%

summary_tse_18 <- summary(tse_18)
# População varia loucamente, vai ser problemático
# Curiosamente, apesar de a mínima, primeiro quartil e mediana de masc ser próxima à de fem, a média, máxima e 
# terceiro quartil são bem diferentes; a primeira é 2k maior em mas, as outras duas, 1k maior em fem.
# isso indica que algumas zonas são muito mais masculinamente concentradas; até a metade, andam juntos,
# mas daí em diante até quase o final, mulheres crescem mais rápidos, até que no final homens explodem. Porque?
# A idadde média estimada é consistente, com a mínima em 36.1, o primeiro quartil em 41.83, media e mediana proximas
# em 43.5 e 43.54, terceiro quartil em 45.18 e maxima em 55.46. Variavel super bem comportada que indica que o eleitorado
# É principalmente a geração de 70
# Para os candidatos, os dados são bisonhos.
# A média é constantemente muito diferente da mediana, e todos parecem ser muuuuuito top-heavy,
# com uma quantidade alta de distrito reportando 0 votos para o candidato.
# As exceções, e olhe lá, são Brancos, Nulos, Bolsonaro e Haddad


