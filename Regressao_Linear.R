#Com base nesse arquivo, vamos criar um modelo de Regressão Linear para estimar o salário dos futuros empregados da empresa XBTO Ltda.
library(readxl)
Regressao_Linear_1_ <- read_excel("Downloads/Regressao_Linear.xlsx")
View(Regressao_Linear_1_)

attach(Regressao_Linear_1_)
names(Regressao_Linear_1_)
#OBS: excluir variavel salnow
Regressao_Linear_1_$sex_males<-ifelse(Regressao_Linear_1_$sex=="Males",1,0)
plot(Regressao_Linear_1_$sex_males,Regressao_Linear_1_$salbeg)
plot(Regressao_Linear_1_$time,Regressao_Linear_1_$salbeg)
plot(Regressao_Linear_1_$age,Regressao_Linear_1_$salbeg)
plot(Regressao_Linear_1_$edlevel,Regressao_Linear_1_$salbeg)
plot(Regressao_Linear_1_$work,Regressao_Linear_1_$salbeg)

cor(Regressao_Linear_1_$sex_males,Regressao_Linear_1_$salbeg)
cor(Regressao_Linear_1_$time,Regressao_Linear_1_$salbeg)
cor(Regressao_Linear_1_$age,Regressao_Linear_1_$salbeg)
cor(Regressao_Linear_1_$edlevel,Regressao_Linear_1_$salbeg)
cor(Regressao_Linear_1_$work,Regressao_Linear_1_$salbeg)

cor.test(Regressao_Linear_1_$sex_males,Regressao_Linear_1_$salbeg)
cor.test(Regressao_Linear_1_$time,Regressao_Linear_1_$salbeg)
cor.test(Regressao_Linear_1_$age,Regressao_Linear_1_$salbeg)
cor.test(Regressao_Linear_1_$edlevel,Regressao_Linear_1_$salbeg)
cor.test(Regressao_Linear_1_$work,Regressao_Linear_1_$salbeg)
