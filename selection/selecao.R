library(data.table)
dados <- fread(input = paste0("selecao.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(dados)

#A)
m0=lm(y ~ 1, data=dados)
m1=step(m0,list(lower = ~ 1,
                upper = ~ x1+x2+x3+x4+x5+x6+x7+x8+x9),
                direction="forward")
# Modelo
modelo <- lm(y ~ x7 + x9 + x6 + x4 + x5 + x1 + x8, data=dados)
summary(modelo)

# Contribuição de cada variável
library(relaimpo)
imp<-calc.relimp(modelo)
var.exp<-data.frame(round(imp$lmg*100,1))
colnames(var.exp)<-"imp.lmg"
nome<-rownames(var.exp)
var.exp<-data.frame(nome,var.exp)
library(ggplot2)
ggplot(var.exp, aes(nome,imp.lmg)) +
  geom_bar(stat = "identity")+
  geom_text(aes(label = imp.lmg), vjust = 1.2, lwd=5, colour = "white")

#B)
plot(fitted(modelo), rstandard(modelo))
abline(0,0)
library(car)
qqPlot(modelo)
#Não está aleatório

plot(dados$x1,dados$y)
plot(dados$x2,dados$y)
plot(dados$x3,dados$y)
plot(dados$x4,dados$y)
plot(dados$x5,dados$y)
plot(dados$x6,dados$y)
plot(dados$x7,dados$y) # Logaritmo
plot(dados$x8,dados$y) # Parabola
plot(dados$x9,dados$y) # Parabola

#dados$x6_2 <- dados$x6^2
#dados$x7_2 <- dados$x7^2
dados$x8_2 <- dados$x8^2
dados$x9_2 <- dados$x9^2

#C)
m0=lm(y ~ 1, data=dados)
m2=step(m0,list(lower = ~ 1,
                upper = ~ x1+x2+x3+x4+x5+x6+x7+x8_2+x9_2),
                direction="forward")

# Modelo
modelo2 <- lm(log(y) ~ x7 + x9_2 + x8_2 + x6 + x4 + x5 + x1, data=dados)
summary(modelo2)

plot(fitted(modelo2), rstandard(modelo2))
abline(0,0)
library(car)
qqPlot(modelo2)
