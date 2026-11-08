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
dados <- read.csv2("dados/Tabela_Escola_2025_V2.csv", fileEncoding = "Windows-1252")

# ============================================
# 3. Criacao da variavel resposta
# ============================================

dados <- dados %>%
  mutate(
    tipo_escola = factor(
      case_when(
        CO_REDE == 1 ~ "Publica",
        CO_REDE == 2 ~ "Privada",
        TRUE ~ NA_character_
      ),
      levels = c("Privada", "Publica")
    )
  )

variaveis_vazamento <- c(
  "CO_REDE",
  "TP_DEPENDENCIA",
  "TP_CATEGORIA_ESCOLA_PRIVADA",
  "NU_CNPJ_ESCOLA_PRIVADA",
  "NU_CNPJ_MANTENEDORA",
  "TP_PODER_PUBLICO_PARCERIA",
  "CO_ENTIDADE",
  "NO_ENTIDADE",
  "IN_MANT_ESCOLA_PRIVADA_EMP",
  "IN_MANT_ESCOLA_PRIVADA_S_FINS",
  "IN_ALIMENTACAO"
)

# ============================================
# 4. Preparacao da base para modelagem
# ============================================

dados_modelo <- dados %>%
  # Remover variaveis que causam data leakage
  select(-any_of(variaveis_vazamento)) %>%
  
  # Remover registros sem classificacao
  filter(!is.na(tipo_escola)) %>%
  
  # Converter variaveis categoricas
  mutate(
    across(matches("^(CO_|TP_)"), as.factor),
    across(where(is.character), as.factor)
  )

# Conferir a base resultante
dim(dados_modelo)
table(dados_modelo$tipo_escola)

# ============================================
# 5. Separacao treino e teste
# ============================================

set.seed(47)

divisao <- initial_split(
  dados_modelo,
  prop = 0.80,
  strata = tipo_escola
)

treino <- training(divisao)
teste  <- testing(divisao)


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