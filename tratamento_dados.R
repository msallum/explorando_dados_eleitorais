library(tidyverse)
library(stringr)
set.seed(93)


# Baixe os dados
dir_perfil_elet_2018 <- 'C:/users/josez/Desktop/Economia/Politica Brasil/explorando_dados_eleitorais/dados/dados_brutos_tse/perfil_eleitorado/planilhas/perfil_eleitorado_2018.csv'
perfil_2018_raw <- read_csv2(dir_perfil_elet_2018, locale = locale(encoding = "latin1"))

idade_est <- function (x) {
  #  Auxiliar para estimar a idade média de cada grupo
  #  O TSE reparte em grupos de cinco anos para idade > 20, e de ano em ano para 16 => idade => 20
  ifelse(
    x>20, x + 2, ifelse(
      x < 0, NA, x
      )
    )
}
na_para_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}


perfil_2018 <- perfil_2018_raw %>%
  mutate(
    IDADE_EST_MIN = as.numeric(CD_FAIXA_ETARIA%/%100),
    IDADE_EST_MED = idade_est(IDADE_EST_MIN)
    ) %>%
  filter(SG_UF != "ZZ") %>%
  select(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, DS_GENERO, IDADE_EST_MED, QT_ELEITORES_PERFIL) %>%
  arrange(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, DS_GENERO, IDADE_EST_MED, QT_ELEITORES_PERFIL) %>%
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, DS_GENERO, IDADE_EST_MED) %>%
  summarise(ELEITORES = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%  # Consolida quantidade de eleitores
  spread(key = DS_GENERO, value = ELEITORES) %>%  # Cria colunas espec?ficas para genero
  rename(NAO_INFORMADO = `NÃO INFORMADO` ) %>%
  mutate(
    NAO_INFORMADO = na_para_zero(NAO_INFORMADO),
    MASCULINO = na_para_zero(MASCULINO),
    FEMININO = na_para_zero(FEMININO),
    ELEITORES = FEMININO + MASCULINO + NAO_INFORMADO
  ) %>% # Recompõe o total de eleitores
  mutate(IDADE_EST_MED = weighted.mean(IDADE_EST_MED, ELEITORES, na.rm = TRUE)) %>%
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, IDADE_EST_MED) %>%
  summarise(
    ELEITORES = sum(ELEITORES, na.rm = TRUE),
    FEMININO = sum(FEMININO, na.rm = TRUE),
    MASCULINO = sum(MASCULINO, na.rm = TRUE)
    )



dir_votos_2018 <- 'C:/users/josez/Desktop/Economia/Politica Brasil/explorando_dados_eleitorais/dados/dados_brutos_tse/votacao_presidente/planilhas/votacao_secao_2018_BR.csv'
votos_2018_raw <- read_csv2(dir_votos_2018, locale = locale(encoding = "latin1"))

votos_2018 <- votos_2018_raw %>%
  select(NR_TURNO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NM_VOTAVEL, QT_VOTOS) %>%
  arrange(NR_TURNO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NM_VOTAVEL) %>%
  group_by(NR_TURNO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NM_VOTAVEL) %>%
  summarise(VOTOS = sum(QT_VOTOS)) %>%
  spread(key = NM_VOTAVEL, value = VOTOS)  # Tidy, cada candidato como uma variavel

tse_2018 <- perfil_2018 %>%
  inner_join(votos_2018, by = NULL) %>%  # Dataframe geral unificando os dados do TSE
  replace(is.na(.), 0) %>%
  select(-'VOTO ANULADO E APURADO EM SEPARADO')


write_csv(tse_2018, path = './dados/dados_formatados/tse_2018.csv')

#samples: 0.6 para treino, 0.1 para confirmação, 0.3 para teste puro
cria_sample_ind <- function(data, pc){
  sample(seq_len(nrow(data)), size = floor(pc*nrow(data)))
}

teste_ind <- cria_sample_ind(tse_2018, 0.3)
teste_tse_2018 = tse_2018[teste_ind, ]
resto = tse_2018[-teste_ind, ]

confirm_ind = cria_sample_ind(resto, 1/7)
confirm_tse_2018 = resto[confirm_ind, ]
treino_tse_2018 = resto[-confirm_ind, ]

write_csv(treino_tse_2018, path = './dados/dados_formatados/treino_tse_2018.csv')
write_csv(teste_tse_2018, path = './dados/dados_formatados/teste_tse_2018.csv')
write_csv(confirm_tse_2018, path = './dados/dados_formatados/confirm_tse_2018.csv')


# TODO: Tratar os dataframes com outros dados em um df_info
# TODO: Unificar o tse_2 com o df_info
# TODO: Analisar primeiro só o tse_2018; depois, o brasil_2018