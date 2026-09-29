# Lista de exercícios 3 - ACD
library(dplyr)

# --- Exercício 1 ---
# Letra A
# Problema de regressão interessado em inferência, já que analisa salário e busca entender quais fatores o alteram. 500 observações, 4 atributos e a variável alvo é o
# salário do CEO.


# Letra B
# Problema de classificação interessado em predição, já que busca acertar a classificação de um desfecho binário. 20 observações, 14 atributos e a variável alvo é se
# o produto é um sucesso ou fracasso.


# Letra C
# Problema de regressão interessado em predição, já que busca prever a variação percentual de um valor. A quantidade de observações é equivalente a quantidade de 
# semanas em 2025, 4 atributos e a variável alvo é a variação percentual da taxa de câmbio USD/Euro em relação às variações semanais nos mercados de ações mundiais


# --- Exercício 2 ---
# A vantagem de uma abordagem flexível é criar um modelo capaz de se adaptar a padrões inconsistentes, melhorando a capacidade preditiva. A desvantagem da flexibilidade
# é o overfitting, ou seja, quando um modelo se torna excessivamente flexível é capaz que o mesmo dê muita importância para ruídos na distribuição dos dados, de modo 
# que, em um cenário real, o modelo apresente um desempenho inferior, pois terá que lidar com distribuições com ruídos e padrões diferentes. Em resumo, um modelo ultra
# flexível acaba se tornando inflexível, na medida em que não é capaz de predizer diferentes padrões.

# --- Exercício 3 ---
# Uma abordagem paramétrica considera que a distribuição de dados assume uma distribuição normal, diferente das abordagens não paramétricas. A vantagem das abordagens
# paramétricas é que elas são mais efetivas e precisas do que abordagens não paramétricas, mas a desvantagem principal é que elas só funcionam com dados que apresentam
# uma distribuição normal.

# --- Exercício 4 ---
# Letra A
dados <- data.frame(
  Observacao = 1:6,
  X1 = c(0, 2, 0, 0, -1, 1),
  X2 = c(3, 0, 1, 1, 0, 1),
  X3 = c(0, 0, 3, 2, 1, 1),
  Y = c("Vermelho", "Vermelho", "Vermelho", "Verde", "Verde", "Vermelho")
)

obs1 <- c(0, 3, 0)
obs2 <- c(2, 0, 0)
obs3 <- c(0, 1, 3)
obs4 <- c(0, 1, 2)
obs5 <- c(-1, 0, 1)
obs6 <- c(1, 1, 1)
pontoTeste <- c(0, 0, 0)

distancia1 <- sqrt(sum((obs1 - pontoTeste)^2))
print(distancia1) # 3

distancia2 <- sqrt(sum((obs2 - pontoTeste)^2))
print(distancia2) # 2

distancia3 <- sqrt(sum((obs3 - pontoTeste)^2))
print(distancia3) # 3.162278

distancia4 <- sqrt(sum((obs4 - pontoTeste)^2))
print(distancia4) # 2.236068

distancia5 <- sqrt(sum((obs5 - pontoTeste)^2))
print(distancia5) # 1.313214

distancia6 <- sqrt(sum((obs6 - pontoTeste)^2))
print(distancia6) # 1.732051


# Letra B
# Vermelho, considerando que o seu vizinho mais próximo é a observação 3 (3 de dist do ponto teste para 3,16 do ponto teste), que apresenta cor vermelha.


# Letra C
# Vermelho, considerando que o seu vizinho mais próximo é a observação 1 (3,16 de dist do ponto teste para 3 do ponto teste), que apresenta cor vermelha.


# --- Exercício 5 ---
# Letra A
dados2 <- data.frame(
  Observacao = 1:6,
  X = c(2.5, 4.1, 1.2, 6.7, 3.8, 5.4),
  Y = c(7.3, 9.8, 4.6, 12.1, 8.5, 10.9)
)

xChapeu <- mean(dados2$X) # 3,95
yChapeu <- mean(dados2$Y) # 8,866667

# Eu acho q eu to confundindo e o termo certo n é chapéu, acho que chapéu é aproximação, sla n lembro


# Letra B
minimosQuadrados <- lm(Y ~ X, data = dados2)

summary(minimosQuadrados)
# B0/Intercepto estimado: 3.5579
# B1/Coeficiente angular estimado: 1.3440


# Letra C

predict(minimosQuadrados, newdata = data.frame(X = 6))
#11.62187 


# Letra D

#Residuos:
#1        2        3        4        5        6 
#0.38213  0.73173 -0.57067 -0.46267 -0.16507  0.08453 

fitted(minimosQuadrados)
# Estimativas:
#1         2         3         4         5         6 
#6.917867  9.068267  5.170667 12.562667  8.665067 10.815467 


# Letra E

resid(minimosQuadrados)
sum(resid(minimosQuadrados)^2)
# 1.255573
# O RSS indica a soma dos erros entre os valores observados e os valores preditos.
# No caso do ajuste do meu modelo, esse valor indica que o modelo se ajustou bem a distribuição
# dos dados, já que a soma dos erros foi próxima de 0.