library(data.table)
library(ggcorrplot)
library(ggplot2)

Base <- fread(input = paste0("exercicios.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(Base)

#Gráfico
plot(Base[,-5])

# Coeficiente de correlação
cor(Base[,-5])
correlacao <- cor(Base[,-5])

# Intervalo de Confiança
cor.test(Base$Carnes,Base$Gasto,conf.level=0.95)$conf.int

# Teste de Hipóteses para amostras comparadas com a população
#cor.test(Base$Carnes, Base$Gasto,alternative="two.sided")

# Reta de regressão no modelo
plot(Base$Carnes,Base$Gasto, lwd=8)
abline(lm(Base$Carnes ~ Base$Gasto))

# Modelo de regressão linear
modelo <- lm(Base$Carnes ~ Base$Gasto)

# Análise
summary(modelo)

# Informações relevantes são o Coefficients estimate as (Ex: Intercept + Base$Gasto*Carnes)
#Exemplo de conclusão: A cada kg a mais de carne espera-se que o gasto final aumente R$60,23 (Fique atento as medidas)

# R^2  = 0,547 ou 54,7%
# Exemplo de conclusão: 54,7% da variação do gasto final é explicado pela quantidade de carne na compra

# Análise de resíduo
plot(fitted(modelo), rstandard(modelo))
abline(0,0)
# Se resíduo não for aleatório(como uma parábola), isso significa que não foi levada em consideração algo importante em consideração
# E talvez seja mais adequado utilizar outro modelo que não seja a regressão linear simples