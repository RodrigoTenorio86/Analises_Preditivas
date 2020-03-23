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


boxplot(Regressao_Linear_1_$salbeg)

salario_futuro_func=lm(Regressao_Linear_1_$salbeg~edlevel+time+age+work+sex,data = Regressao_Linear_1_)
salario_futuro_func
#salario=b0+b1*edlevel+b2*time+b3*age+b4*work+b5*sex
#salario=-2525.81+652.74*edlevel+(-22.48*time)+36.75*age+16.14*work+1567.93*sex
#salario_futuro_func[[1]]+salario_futuro_func[[2]]*Regressao_Linear_1_$edlevel
plot(Regressao_Linear_1_$edlevel,Regressao_Linear_1_$salbeg, main = "Diagrama da Reta")
abline(salario_futuro_func,col="red")
summary(salario_futuro_func)

