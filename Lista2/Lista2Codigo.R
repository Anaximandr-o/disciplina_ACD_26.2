# Lista de exercícios 2 - ACD
library(dplyr)
library(vcd)

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