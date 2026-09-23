# Lista de exercícios 2 - ACD
library(dplyr)
library(vcd)
library(caret)
library(rsample)
library(e1071)
library(caTools)
library(rpart)
library(MASS) 
library(neuralnet)

dados1 <- read.csv("dataset1.csv")
dados2 <- read.csv("dataset2.csv")
dados3 <- read.csv("dataset3.csv")

# --- EXERCÍCIO 1 ---dr
# 303 observações e 15 atributos

# --- EXERCÍCIO 2 ---
dados1Limpos <- dados1
dados1Limpos$ChestPain <- NULL
dados1Limpos$Thal <- NULL

# --- EXERCÍCIO 3 ---
sum(is.na(dados1Limpos)) # 4 NAs
dados1Limpos <- dados1Limpos[complete.cases(dados1Limpos),]
sum(is.na(dados1Limpos)) # 0 NAs

# --- EXERCÍCIO 4 ---
nominais <- c("Sex", "Fbs", "RestECG", "ExAng", "Slope", "Ca")
sapply(nominais, function(v) assocstats(table(dados1Limpos[[v]], dados1Limpos$AHD))$cramer)
#Ca possui variância de 0.49, seguido por ExAng com 0.42 e Slope com 0.38

# --- EXERCÍCIO 5 ---
set.seed(42)
divisao <- initial_split(
  dados1Limpos,
  prop = 0.70,
  strata = AHD
)
treino <- training(divisao)
teste  <- testing(divisao)

# --- EXERCÍCIO 6 ---
# --- 6a ---
modeloIngenuo <- naiveBayes(AHD ~ ., data = treino)
modeloIngenuo
yPredicao <- predict(modeloIngenuo, newdata = teste)
matrizConfusao <- table(teste$AHD, yPredicao)
confusionMatrix(matrizConfusao)
# Acurácia 0.7692 --- Sensibilidade 0.75 --- Especificidade 0.8

# --- 6b ---
controle <- trainControl(
  method = "cv",
  number = 10
)

regressaoLog <- train(
  AHD ~ X + Age + Sex + Oldpeak +
    RestBP + Chol + ExAng + Ca +
    Fbs + RestECG + MaxHR + Slope,
  data = treino,
  method = "glm",
  family = binomial,
  metric = "Accuracy",
  trControl = controle
)

# Probs previstas no teste
prob_teste <- predict(
  regressaoLog,
  newdata = teste,
  type = "prob"
)

# Classe prevista usando ponto de corte 0,5
classe_teste <- ifelse(
  prob_teste$Yes >= 0.50,
  "Yes",
  "No"
)

classe_teste <- factor(
  as.character(classe_teste),
  levels = c("No", "Yes")
)

teste$AHD <- factor(
  as.character(teste$AHD),
  levels = c("No", "Yes")
)

# Matriz de confusão
matrizRegressao <- confusionMatrix(
  classe_teste,
  teste$AHD,
  positive = "Yes"
)

matrizRegressao
# Acurácia 0.8352 --- Sensibilidade 0.8095 --- Especificidade 0.8571

# --- 6c ---
modeloArvore <- rpart(AHD ~ ., 
                    data = treino, 
                    method = "class",
                    control = rpart.control(minsplit = 10, cp = 0.01))
predArvore <- predict(modeloArvore, teste, type = "class")
confusionMatrix(predArvore, teste$AHD)
# Acurácia 0.7363 --- Sensibilidade 0.7551 --- Especificidade 0.7143

# --- 6d ---
formula <- AHD ~ X + Age + Sex + RestBP + Chol + Fbs + RestECG + MaxHR + ExAng + Oldpeak + Slope + Ca

modeloMLP <- neuralnet(formula, data = treino, hidden = c(5, 3), linear.output = FALSE, stepmax = 1e6)

predMLP <- predict(modeloMLP, teste)
classesPredMLP <- ifelse(predMLP[,2] > 0.5, "Yes", "No")

acuracia <- mean(classesPredMLP == as.character(teste$AHD))
print(paste("Acurácia: ", acuracia))
# Acc1: 0.6813 --- Acc2: 0.6483 --- Acc3: 0.7472 (Resultado bem acima, n sei pq, rodei o mesmo código)

# Modelo 2
set.seed(22)
modeloMLP2 <- neuralnet(formula, data = treino, hidden = c(5, 3), linear.output = FALSE, stepmax = 1e6)
predMLP2 <- predict(modeloMLP2, teste)
classesPredMLP2 <- ifelse(predMLP2[,2] > 0.5, "Yes", "No")
acuracia2 <- mean(classesPredMLP2 == as.character(teste$AHD))
print(paste("Acurácia MLP 2:", acuracia2))
# Acurácia do modelo 2: 0.7472 (Sim, deu a mesma acurácia do modelo 3. Loucura)

# Modelo 3
set.seed(11)

modeloMLP3 <- neuralnet(formula, data = treino, hidden = c(5, 3), linear.output = FALSE, stepmax = 2000)
predMLP3 <- predict(modeloMLP3, teste)
classesPredMLP3 <- ifelse(predMLP3[,2] > 0.5, "Yes", "No")

acuracia3 <- mean(classesPredMLP3 == as.character(teste$AHD))
print(paste("Acurácia MLP 3:", acuracia3))
# Tratando max_iter como stepmax no R, o modelo não consegue convergir :(

# --- Exercício 7 ---
# 2299 observações e 12 atributos
nominais2 <- c("Sex", "Fbs", "RestECG", "ExAng", "Slope", "Ca")
sapply(nominais, function(v) assocstats(table(dados2[[v]], dados2$AHD))$cramer)
#Ca possui variância de 0.94, seguido por Slope com 0.27 e ExAng com 0.25

set.seed(42)
divisao2 <- initial_split(
  dados2,
  prop = 0.70,
  strata = AHD
)
treino2 <- training(divisao2)
teste2  <- testing(divisao2)

# --- Exercício 8 ---
# 299 observações e 28 atributos
nominais3 <- c("Sex", "Fbs", "RestECG", "ExAng", "Slope", "Ca", "a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p")
sapply(nominais3, function(v) assocstats(table(dados3[[v]], dados3$AHD))$cramer)
#Ca possui variância de 0.49, seguido por ExAng com 0.42 e Slope com 0.38

set.seed(42)
divisao3 <- initial_split(
  dados3,
  prop = 0.70,
  strata = AHD
)
treino3 <- training(divisao3)
teste3  <- testing(divisao3)

# --- Exercício 9 ---
extrairAcuracia <- function(previsto, real) {
  previsto <- factor(as.character(previsto), levels = c("No", "Yes"))
  real     <- factor(as.character(real),     levels = c("No", "Yes"))
  confusionMatrix(previsto, real)$overall[["Accuracy"]]
}

avaliarModelos <- function(treino, teste, nomeDataset) {
  preditores <- setdiff(names(treino), "AHD")
  formulaCompleta <- as.formula(paste("AHD ~", paste(preditores, collapse = " + ")))
  
  modeloIngenuo <- naiveBayes(AHD ~ ., data = treino)
  accTreinoNB <- extrairAcuracia(predict(modeloIngenuo, newdata = treino), treino$AHD)
  accTesteNB  <- extrairAcuracia(predict(modeloIngenuo, newdata = teste),  teste$AHD)
  
  regressaoLog <- train(AHD ~ ., data = treino, method = "glm", family = binomial,
                        trControl = trainControl(method = "cv", number = 10))
  accTreinoLog <- extrairAcuracia(predict(regressaoLog, newdata = treino), treino$AHD)
  accTesteLog  <- extrairAcuracia(predict(regressaoLog, newdata = teste),  teste$AHD)
  
  modeloArvore <- rpart(AHD ~ ., data = treino, method = "class",
                        control = rpart.control(minsplit = 10, cp = 0.01))
  accTreinoArvore <- extrairAcuracia(predict(modeloArvore, treino, type = "class"), treino$AHD)
  accTesteArvore  <- extrairAcuracia(predict(modeloArvore, teste,  type = "class"), teste$AHD)
  
  parametrosEscala <- preProcess(treino[, preditores], method = c("center", "scale"))
  treinoEscalado <- predict(parametrosEscala, treino[, preditores])
  treinoEscalado$AHD <- treino$AHD
  testeEscalado <- predict(parametrosEscala, teste[, preditores])
  testeEscalado$AHD <- teste$AHD
  
  resultadoMLP <- tryCatch({
    modeloMLP <- neuralnet(formulaCompleta, data = treinoEscalado, hidden = c(5, 3),
                           linear.output = FALSE, stepmax = 1e6)
    previstoTreinoMLP <- ifelse(predict(modeloMLP, treinoEscalado)[, 2] > 0.5, "Yes", "No")
    previstoTesteMLP  <- ifelse(predict(modeloMLP, testeEscalado)[, 2]  > 0.5, "Yes", "No")
    c(extrairAcuracia(previstoTreinoMLP, treino$AHD), extrairAcuracia(previstoTesteMLP, teste$AHD))
  })
  accTreinoMLP <- resultadoMLP[1]
  accTesteMLP  <- resultadoMLP[2]
  
  data.frame(
    dataset        = nomeDataset,
    modelo         = c("Naive Bayes", "Regressão Logística", "Árvore de Decisão", "Rede Neural (MLP)"),
    acuraciaTreino = round(c(accTreinoNB, accTreinoLog, accTreinoArvore, accTreinoMLP), 4),
    acuraciaTeste  = round(c(accTesteNB,  accTesteLog,  accTesteArvore,  accTesteMLP), 4)
  )
}

set.seed(42)
resultados9 <- rbind(
  avaliarModelos(treino,  teste,  "Dataset 1"),
  avaliarModelos(treino2, teste2, "Dataset 2"),
  avaliarModelos(treino3, teste3, "Dataset 3")
)
resultados9$diferenca <- round(resultados9$acuraciaTreino - resultados9$acuraciaTeste, 4)
resultados9
# Bom ajuste: Naive Bayes (df 1, 2, 3), Regressão Logística (df 1, 2, 3), Árvore (df 2)
# Overfit: Árvore (df 1, 3), MLP (df 1, 2, 3)

# --- Exercício 10 ---
# Letra a
# Com muitas observações e poucos preditores, um modelo bem flexível
# é capaz de estimar com precisão e sem aumentar muito a variância, sem
# causar overfit.

# Letra b
# Nesse caso, a flexibilidade acaba sendo negativo, já que o modelo consegue
# decorar o padrão dos dados facilmente, estourando a variância e provavelmente
# resultando em overfit.

# Letra c
# Um modelo não flexível assume um formato linear, então não conseguiria captar
# corretamente o padrão de dados altamente não lineares. Assim, nesse caso, um
# modelo mais flexível é a melhor opção.