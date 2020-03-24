library(readxl)
Regressao_Linear_1_ <- read_excel("Downloads/Regressao_Linear.xlsx")
View(Regressao_Linear_1_)

attach(Regressao_Linear_1_)
names(Regressao_Linear_1_)
#OBS: excluir variavel salnow
Regressao_Linear_1_$sex_males<-ifelse(Regressao_Linear_1_$sex=="Males",1,0)

#Com base nesse arquivo, vamos criar um modelo de Regressão Linear para estimar o salário dos futuros empregados da empresa XBTO Ltda.

salario_futuro_func=lm(Regressao_Linear_1_$salbeg~edlevel+time+age+work+sex_males,data = Regressao_Linear_1_)
salario_futuro_func
#Foi determinado uma equacao para estimar o salário dos futuros empregados da empresa XBTO Ltda.

#Interpretacao dos coeficiente associado ao efeito das variaveis(edlevel, time, age, work e sexMales)
#, no salário dos futuros empregados da empresa XBTO Ltda.
#Considerado os dados e o modelo das variaveis(edlevel, time, age, work e sex) para construir estes modelos:

#salario=b0+b1*edlevel+b2*time+b3*age+b4*work+b5*sex
#salario=-2525.81+652.74*edlevel+(-22.48*time)+36.75*age+16.14*work+1567.93*sex
#salario=salario_futuro_func[[1]]+salario_futuro_func[[2]]*Regressao_Linear_1_$edlevel+salario_futuro_func[[3]]*Regressao_Linear_1_$age+salario_futuro_func[[4]]*Regressao_Linear_1_$work+salario_futuro_func[[5]]*Regressao_Linear_1_$sexMales 

#plot(Regressao_Linear_1_$edlevel,Regressao_Linear_1_$salbeg, main = "Diagrama da Reta")
#abline(salario_futuro_func,col="red")
summary(salario_futuro_func)

##############################Regressão Linear#####################################################

#1.) Realizar uma análise exploratória de dados de todas as variáveis (estatísticas descritivas e gráficos) e,
# Teste T para as variáveis salbeg x sexo (não esquecer o teste de leveve). Comentar todas as respostas.


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


#2)   Faça uma correlação linear entre as variáveis:
#  2.1 Qual a variável que possui a MAIOR  correlação com a variável dependente e, qual é  valor do  Interprete a correlação e o 
#2.1 modelo < -Regressao_Linear_1_[,c("id","salbeg","time","age","edlevel","work")] head(modelo)

cor.test(Regressao_Linear_1_$sex_males,Regressao_Linear_1_$salbeg)
cor.test(Regressao_Linear_1_$time,Regressao_Linear_1_$salbeg)
cor.test(Regressao_Linear_1_$age,Regressao_Linear_1_$salbeg)
cor.test(Regressao_Linear_1_$edlevel,Regressao_Linear_1_$salbeg)
cor.test(Regressao_Linear_1_$work,Regressao_Linear_1_$salbeg)

modelo <- Regressao_Linear_1_[,c("id","salbeg","time","age","edlevel","work")] 
head(modelo)

#2.2 Realiza um regressão linear múltipla com todas as variáveis relevantes para a equação e, 
#qual o valor do  Justifique porquê as variáveis que entraram na equação é relevantes? 
#Se alguma variável ficou fora da equação justifique sua resposta


#2.3 Interprete todos os parâmetros (Betas) da equação do modelo final.


#2.4 Teste a multicolinearidade através da estatística VIF do modelo final.

#2.5 Faça uma análise dos resíduos (gráficos e normalidade do teste) do modelo final.

#2.6 O modelo/ equação está adequada?


#2.7 Faça um previsão de salário para um candidato masculino/ feminino. 
#Precisar conter na formula todas as variáveis relevantes.






