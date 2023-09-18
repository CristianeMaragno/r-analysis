library(data.table)

Base <- fread(input = paste0("risco.csv"), header = T, na.strings = "NA", data.table = FALSE, dec=",")
names(Base)

modelo <- lm(Base$Risco ~ Base$Idade + Base$Pressao, data=Base)
summary(modelo)
library(car)
avPlots(modelo)
