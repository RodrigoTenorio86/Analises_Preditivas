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
298+270+148


credito2$novo_emprestimos <- ifelse(credito2$emprestimos >= 1,1,0)

table(credito2$novo_emprestimos)

credito2$novo_emprestimos<-as.factor(credito2$novo_emprestimos)
table(credito2$novo_emprestimos)
credito2$novo_emprestimos<-relevel(credito2$novo_emprestimos,ref = 1) 

futuro_emprestimos<-glm(credito2$novo_emprestimos ~ ., data = credito2,family = binomial)


class(hipoteca)
