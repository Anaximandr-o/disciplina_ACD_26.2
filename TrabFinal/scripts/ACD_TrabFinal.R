# ============================================
# 1. Configuração e Bibliotecas
# ============================================
library(tidyverse)
library(lubridate)
library(tidymodels)
library(themis)
library(summarytools)
library(geobr)
library(sf)
library(arrow)

# Definição de semente para reprodutibilidade
set.seed(47) 

# ============================================
# 2. Leitura dos Dados
# ============================================
dados <- read.csv2("../dados/Tabela_Escola_2025_V2.csv", fileEncoding = "Windows-1252")

# ============================================
# 3. Variável Resposta & Separacao treino teste
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

divisao <- initial_split(
  dados,
  prop = 0.80,
  strata = vResposta
)

treino <- training(divisao)
teste  <- testing(divisao)

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

# ============================================
# 6. Pre-processamento
# ============================================

receita <- recipe(tipo_escola ~ ., data = treino) %>%
  # Remover variaveis sem variacao
  step_zv(all_predictors()) %>%
  
  # Tratar categorias novas e valores ausentes
  step_novel(all_nominal_predictors()) %>%
  step_unknown(
    all_nominal_predictors(),
    new_level = "Desconhecido"
  ) %>%
  
  # Imputar valores numericos ausentes pela mediana
  step_impute_median(all_numeric_predictors())


# ============================================
# 7. Treinamento da Random Forest
# ============================================

n_pred <- ncol(treino) - 1

rf_spec <- rand_forest(
  trees = 500,
  mtry = max(1, floor(sqrt(n_pred))),
  min_n = 5
) %>%
  set_engine(
    "ranger",
    importance = "permutation",
    probability = TRUE,
    num.threads = max(
      1,
      parallel::detectCores(logical = FALSE) - 1
    ),
    respect.unordered.factors = "order"
  ) %>%
  set_mode("classification")

modelo_rf <- workflow() %>%
  add_recipe(receita) %>%
  add_model(rf_spec) %>%
  fit(data = treino)


# ============================================
# 8. Avaliacao no conjunto de teste
# ============================================

pred_classe <- predict(
  modelo_rf,
  new_data = teste,
  type = "class"
)

pred_prob <- predict(
  modelo_rf,
  new_data = teste,
  type = "prob"
)

avaliacao <- bind_cols(
  teste %>% select(tipo_escola),
  pred_classe,
  pred_prob
)

# Metricas de classificacao
# Privada e a classe positiva

metricas <- tibble(
  Acuracia = accuracy_vec(
    avaliacao$tipo_escola,
    avaliacao$.pred_class
  ),
  
  Sensibilidade_privada = sens_vec(
    avaliacao$tipo_escola,
    avaliacao$.pred_class,
    event_level = "first"
  ),
  
  Especificidade_publica = spec_vec(
    avaliacao$tipo_escola,
    avaliacao$.pred_class,
    event_level = "first"
  ),
  
  AUC = roc_auc_vec(
    avaliacao$tipo_escola,
    avaliacao$.pred_Privada,
    event_level = "first"
  )
)

print(metricas)

# Matriz de confusao
conf_mat(
  avaliacao,
  truth = tipo_escola,
  estimate = .pred_class
)


# ============================================
# 9. Importancia das variaveis por permutacao
# ============================================

# Extrair importancia calculada pelo ranger
importancia <- extract_fit_engine(modelo_rf)$variable.importance

importancia_df <- tibble(
  variavel = names(importancia),
  importancia = as.numeric(importancia)
) %>%
  arrange(desc(importancia))

# Exibir as 20 variaveis mais importantes
top_importancia <- importancia_df %>%
  slice_head(n = 20)

print(top_importancia)


# ============================================
# 10. Grafico de importancia
# ============================================

ggplot(
  top_importancia,
  aes(
    x = reorder(variavel, importancia),
    y = importancia
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Importancia das variaveis - Random Forest",
    subtitle = "Importancia por permutacao",
    x = "Variavel",
    y = "Importancia"
  ) +
  theme_minimal()