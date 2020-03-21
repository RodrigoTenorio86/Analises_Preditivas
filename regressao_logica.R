library(readxl)
credito2 <- read_excel("C:/Users/RTenorio/Downloads/credito2.xlsx")
View(credito2)

attach(credito2)

install.packages(c("glmnet","glm2","ROCR","ResourceSelection"))
library(glmnet)
library(glm2)                 
library(ROCR)
library(ResourceSelection)

names(credito2)

table(risco_credito)
table(risco)
table(emprestimos)
table(hipoteca)

futuro_emprestimos<-glm(emprestimos~ data = idade+salario+sexo+estado_civil+n_filhos+n_cartoes+pagto_salario+hipoteca+emprestimos+risco,family = binomial)


class(hipoteca)
