library(data.table)
dados <- fread(input = paste0("Deslocamento.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(dados)

#A)
dados <- as.data.frame(lapply(dados, as.factor))

#B)
m0=glm(desloc ~ 1, data=dados, family=binomial())
modelo=step(m0,list(lower = ~ 1,
                upper = ~ escola + sexo + idade + imc + tr + pa + t_livre),
        direction="forward")

summary(modelo)

plot(fitted(modelo), rstandard(modelo))
abline(0,0)

#C)
# Razão de Chances
OR <- data.frame(exp(modelo$coefficients))
IC <- data.frame(exp(confint(modelo)))
IC_OR <- cbind(OR[-1,],IC[-1,])
colnames(IC_OR) <- c("OR","2.5%","97.5%")
