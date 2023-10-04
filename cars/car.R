library(data.table)
dados <- fread(input = paste0("car_base.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(dados)

# Classificação das variáveis
dados$drivewheel <- as.factor(dados$drivewheel)
dados$carwidth <- as.numeric(dados$carwidth)
dados$price <- as.numeric(dados$price)

# A)
# Limpar dados
dados <- dados[dados$drivewheel != "4wd", ] 

# B)
# Modelo
modelo <- lm(price ~ drivewheel*carwidth, data=dados)
summary(modelo)
library(car)
Anova(modelo)

#C)
#ŷ = -102,0301 +1,7103carwidth -48,4992drivewheelrwd + 0.8210drivewheelrwd*carwidth 
# Ao nível de significancia de 5%, a relção é relevante 0.0252

#D)
# Gráficos
library(ggplot2)
ggplot(data = dados, aes(x = carwidth, y = price, color = drivewheel)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Gráfico da Interação",
       x = "Largura",
       y = "Valor")

#E)
# Intervalo de confiança e intervalo de predição
pred = data.frame(drivewheel='fwd',carwidth=70)
confidence_interval <- predict(modelo, pred, interval="confidence",level=0.95)
pred_interval <- predict(modelo, pred, interval="predict",level=0.95)

# Intervalo de valores dentro do qual está contido o parâmetro populacional com determinada confiança
# Estimativa de um intervalo de valores, a partir de dados observados, dentro do qual novos dados do mesmo contexto estarão contidos, com determinada probabilidade
