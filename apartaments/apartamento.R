library(data.table)
dados <- fread(input = paste0("apartamento.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(dados)

#A)
#Transform variável qualitativa
dados$Local = as.factor(dados$Local)

# Modelo de Regressão Múltipla
modelo <- lm(Valor ~ Area + Idade + Local, data=dados)
summary(modelo)
#Retirado energia com p-valor de 0,23
# Ŷ = 15,36978 + 1,00939x + -2,05789x - 11,11571x
# A cada m² a mais do imóvel, espera-se que aumente em R$1009,39 o valor do imóvel. 
# A cada ano a mais no tempo de construção do imóvel, espera-se uma diminuição de R$ 2057,89 no valor do imóvel
# Caso o imóvel esteja na região menos valorizada, espera-se uma diminuição de R$11115,71 no valor do imóvel

#B)
#PROBLEMA - análise de multicolinearidade 
library(car)
vif(modelo) # Avaliar se todas as variáveis realemente são necessárias
avPlots(modelo)
# Retiradas x porque

#C)
#Importancia das variáveis
library(relaimpo)
imp <-calc.relimp(modelo)
var.exp<-data.frame(round(imp$lmg*100,1))
colnames(var.exp)<-"imp.lmg"
nome<-rownames(var.exp)
var.exp<-data.frame(nome,var.exp)

library(ggplot2)
ggplot(var.exp,aes(nome,imp.lmg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = imp.lmg), vjust = 1.5, lwd=6, colour = "white")
#A variável da área é a mais importante porque explica 61% do valor final do imóvel, uma dirença significatica

#D)
#R² Coeficiente de Determinação Ajustado
summary(modelo)
# R-squared:  0.7992,	Adjusted R-squared:  0.7852
# Representa o quanto aquela variável está influenciando eo ajsutado leva em consideração as sobreposições

# Análise de resíduos
plot(fitted(modelo), rstandard(modelo))
abline(0,0)
library(car)
qqPlot(modelo)

# Gráfico de quantil
plot(fitted(modelo))
abline(0,0)
library(car)
qqPlot(modelo)