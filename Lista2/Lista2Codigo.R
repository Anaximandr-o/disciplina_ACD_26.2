# Lista de exercícios 2 - ACD
library(dplyr)

dados1 <- read.csv("Lista2/dataset1.csv")
dados2 <- read.csv("Lista2/dataset2.csv")
dados3 <- read.csv("Lista2/dataset3.csv")

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
