library(data.table)
dados <- fread(input = paste0("imc.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(dados)

# Modelo de Regressão Múltipla
modelo <- lm(dados$IMC ~ dados$TR + dados$SOMA_DC, data=dados)
summary(modelo)

# Análise Gráfica da relação das variáveis independentes com a variável dependente
library(car)
vif(modelo) # Avaliar se todas as variáveis realemente são necessárias
avPlots(modelo)

# Coeficiente de Determinação Ajustado (R2)
# R-squared:  0.7501,	Adjusted R-squared:  0.7485 

# Importância de cada variável no modelo
library(relaimpo)
imp<-calc.relimp(modelo)
var.exp<-data.frame(round(imp$lmg*100,1))
colnames(var.exp)<-"imp.lmg"
nome<-rownames(var.exp)
var.exp<-data.frame(nome,var.exp)

library(ggplot2)
ggplot(var.exp,aes(nome,imp.lmg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = imp.lmg), vjust = 1.5, lwd=6, colour = "white")

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
