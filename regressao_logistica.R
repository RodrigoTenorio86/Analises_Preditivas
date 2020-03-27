library(readxl)
credito2 <- read_excel("Downloads/credito2.xlsx")

View(credito2)


attach(credito2)

install.packages(c("glmnet","glm2","ROCR","ResourceSelection"))
library(glmnet)
library(glm2)                 
library(ROCR)
library(ResourceSelection)

names(credito2)

table(sexo)
table(estado_civil)
table(risco_credito)
table(risco)
table(emprestimos)
table(hipoteca)
298+270+148

credito2$sexo_masc<-ifelse(credito2$sexo=="masculino",1,0)
credito2$conjuge<-ifelse(credito2$estado_civil=="casado",1,0)
credito2$novo_emprestimos <- ifelse(credito2$emprestimos >= 1,1,0)
credito2$hipoteca_existe <-  ifelse(credito2$hipoteca == "nao",0,1)

table(credito2$conjuge)
table(credito2$novo_emprestimos)
table(credito2$hipoteca_existe)

#credito2$novo_emprestimos<-as.factor(credito2$novo_emprestimos)
#table(credito2$novo_emprestimos)
#credito2$novo_emprestimos<-relevel(credito2$novo_emprestimos,ref = 1) 


plot(credito2$salario,credito2$novo_emprestimos,pch=20 )
plot(credito2$n_filhos,credito2$novo_emprestimos,pch=20  )
plot(credito2$idade,credito2$novo_emprestimos,pch=20)
cor(credito2$idade,credito2$novo_emprestimos)
cor(credito2$salario,credito2$novo_emprestimos)
cor(credito2$emprestimos,credito2$novo_emprestimos )
cor(credito2$n_filhos,credito2$novo_emprestimos)
cor(credito2$idade,credito2$novo_emprestimos )
cor(credito2$n_cartoes,credito2$novo_emprestimos)
cor(credito2$hipoteca_existe,credito2$novo_emprestimos)
cor(credito2$salario,credito2$novo_emprestimos)
cor(credito2$risco_credito,credito2$novo_emprestimos)
cor(credito2$sexo_masc,credito2$novo_emprestimos)


futuro_emprestimos<-glm(credito2$novo_emprestimos ~ idade+salario+n_filhos+n_cartoes+salario+risco_credito+hipoteca_existe+sexo+conjuge, data = credito2,family = binomial)


summary(futuro_emprestimos)
exp(futuro_emprestimos$coefficients)

#A variavel probabilidade_novo_emprestimo, é a Probabilidade para tomada de novos emprestimos.
credito2$probabilidade_novo_emprestimo= predict(futuro_emprestimos,type = "response", newdata = credito2)
View(credito2)
