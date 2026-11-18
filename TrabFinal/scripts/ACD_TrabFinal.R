# ============================================
# 1. Configuração e Bibliotecas
# ============================================
library(tidyverse)
library(tidymodels)
library(themis)
library(summarytools)
library(arrow)
library(dplyr)

# Definição de semente para reprodutibilidade
set.seed(47) 

# ============================================
# 2. Leitura dos Dados
# ============================================
dados2 <- read.csv2("../dados/Tabela_Escola_2025_V2.csv", fileEncoding = "Windows-1252")

dados <- dados2 %>%
  dplyr::filter(!TP_SITUACAO_FUNCIONAMENTO %in% c(2,3)) %>%
  droplevels()
# ============================================
# 3. Criação da variável resposta
# ============================================
dados <- dados %>%
  mutate(
    vResposta = factor(
      case_when(
        CO_REDE == 1 ~ "Publica",
        CO_REDE == 2 ~ "Privada",
        TRUE ~ NA_character_
      ),
      levels = c("Privada", "Publica")
    )
  )
# ============================================
# 4. Remoção de variáveis
# ============================================

# Removendo variáveis não relevantes
dados$NU_ANO_CENSO <- NULL
dados$TP_SITUACAO_FUNCIONAMENTO <- NULL

# Removendo variáveis redundantes
dados$CO_REGIAO <- NULL
dados$SG_UF <- NULL
dados$CO_UF <- NULL
dados$CO_MUNICIPIO <- NULL
dados$CO_REGIAO_GEOG_INTERM <- NULL
dados$CO_REGIAO_GEOG_IMED <- NULL
dados$CO_DISTRITO <- NULL
dados$CO_REGIAO_GEOG_INTERM <- NULL
dados$CO_SUBDISTRITO <- NULL

# Removendo variáveis que causariam data leakage
dados$CO_LINGUA_INDIGENA_3 <- NULL
dados$CO_LINGUA_INDIGENA_2 <- NULL
dados$CO_IES_OFERTANTE <- NULL
dados$CO_SUBDISTRITO <- NULL
dados$NO_ENTIDADE <- NULL
dados$CO_ENTIDADE <- NULL
dados$TP_DEPENDENCIA <- NULL
dados$TP_CATEGORIA_ESCOLA_PRIVADA <- NULL
dados$CO_ORGAO_REGIONAL <- NULL
dados$DT_ANO_LETIVO_INICIO <- NULL
dados$DT_ANO_LETIVO_TERMINO <- NULL
dados$IN_VINCULO_SECRETARIA_EDUCACAO <- NULL
dados$IN_VINCULO_SEGURANCA_PUBLICA <- NULL
dados$IN_VINCULO_SECRETARIA_SAUDE <- NULL
dados$IN_VINCULO_OUTRO_ORGAO <- NULL
dados$IN_PODER_PUBLICO_PARCERIA <- NULL
dados$TP_PODER_PUBLICO_PARCERIA <- NULL

dados$IN_FORMA_CONT_TERMO_COLABORA <- NULL
dados$IN_FORMA_CONT_TERMO_FOMENTO <- NULL
dados$IN_FORMA_CONT_ACORDO_COOP <- NULL
dados$IN_FORMA_CONT_PRESTACAO_SERV <- NULL
dados$IN_FORMA_CONT_COOP_TEC_FIN <- NULL
dados$IN_FORMA_CONT_CONSORCIO_PUB <- NULL
dados$IN_FORMA_CONT_MU_ACORDO_COOP <- NULL
dados$IN_FORMA_CONT_MU_TERMO_COLAB <- NULL
dados$IN_FORMA_CONT_MU_CONSORCIO_PUB <- NULL
dados$IN_FORMA_CONT_MU_COOP_TEC_FIN <- NULL
dados$IN_FORMA_CONT_MU_PREST_SERV <- NULL
dados$IN_FORMA_CONT_MU_TERMO_FOMENTO <- NULL
dados$IN_FORMA_CONT_ES_TERMO_COLAB <- NULL
dados$IN_FORMA_CONT_ES_TERMO_FOMENTO <- NULL
dados$IN_FORMA_CONT_ES_ACORDO_COOP <- NULL
dados$IN_FORMA_CONT_ES_PREST_SERV <- NULL
dados$IN_FORMA_CONT_ES_COOP_TEC_FIN <- NULL
dados$IN_FORMA_CONT_ES_CONSORCIO_PUB <- NULL

dados$IN_MANT_ESCOLA_PRIVADA_EMP <- NULL
dados$IN_MANT_ESCOLA_PRIVADA_ONG <- NULL
dados$IN_MANT_ESCOLA_PRIVADA_OSCIP <- NULL
dados$IN_MANT_ESCOLA_PRIV_ONG_OSCIP <- NULL
dados$IN_MANT_ESCOLA_PRIVADA_SIND <- NULL
dados$IN_MANT_ESCOLA_PRIVADA_SIST_S <- NULL
dados$IN_MANT_ESCOLA_PRIVADA_S_FINS <- NULL

dados$NU_CNPJ_ESCOLA_PRIVADA <- NULL
dados$NU_CNPJ_MANTENEDORA <- NULL
dados$CO_ESCOLA_SEDE_VINCULADA <- NULL
dados$TP_OCUPACAO_PREDIO_ESCOLAR <- NULL
dados$TP_OCUPACAO_GALPAO <- NULL

# Removendo a variável resposta
dados$CO_REDE <- NULL

# Antes o banco apresentava 290 variáveis, agora apresenta 234.
# A quantidade de observações também diminiui, indo de 214.192 linhas para 180.540.

arrow::write_parquet(dados, "../outputs/dadosEscolaLimpos.parquet")