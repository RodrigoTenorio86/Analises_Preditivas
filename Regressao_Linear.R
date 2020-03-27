install.packages("car")
library(car)
library(readxl)
library(faraway)
Regressao_Linear_1_ <- read_excel("Downloads/Regressao_Linear.xlsx")
View(Regressao_Linear_1_)


#OBS: excluir variavel salnow
Regressao_Linear_1_<-Regressao_Linear_1_[,-c(6)]
attach(Regressao_Linear_1_)
names(Regressao_Linear_1_)
#Definido que 0 e representam os sexo femenino e masculino, respectivamente.
Regressao_Linear_1_$sex_males<-ifelse(Regressao_Linear_1_$sex=="Males",1,0)
Regressao_Linear_1_<-Regressao_Linear_1_[,-c(3)]

#Com base nesse arquivo, vamos criar um modelo de Regressão Linear para estimar o salário dos futuros empregados da empresa XBTO Ltda.

salario_futuro_func=lm(Regressao_Linear_1_$salbeg~edlevel+time+age+work+sex_males,data = Regressao_Linear_1_)
salario_futuro_func
#Foi determinado uma equacao para estimar o salário dos futuros empregados da empresa XBTO Ltda.

#Interpretacao dos coeficiente associado ao efeito das variaveis(edlevel, time, age, work e sexMales)
#, no salário dos futuros empregados da empresa XBTO Ltda.
#Considerado os dados e o modelo das variaveis(edlevel, time, age, work e sex) para construir estes modelos abaixo:

#salario=b0+b1*edlevel+b2*time+b3*age+b4*work+b5*sex
#salario=-2525.81+652.74*edlevel+(-22.48*time)+36.75*age+16.14*work+1567.93*sex
#salario=salario_futuro_func[[1]]+salario_futuro_func[[2]]*Regressao_Linear_1_$edlevel+salario_futuro_func[[3]]*Regressao_Linear_1_$age+salario_futuro_func[[4]]*Regressao_Linear_1_$work+salario_futuro_func[[5]]*Regressao_Linear_1_$sexMales 

summary(salario_futuro_func)

#Obs:Com este modelo consigo explica 49% de variabilidade dos dados. 

##############################Regressão Linear#####################################################

#1.) Realizar uma análise exploratória de dados de todas as variáveis (estatísticas descritivas e gráficos) e,
# Teste T para as variáveis salbeg x sexo (não esquecer o teste de leveve). Comentar todas as respostas.
Sexo = factor(Regressao_Linear_1_$sex_males)
par(mfrow=c(1,1))
plot(Regressao_Linear_1_$work[Sexo==1],Regressao_Linear_1_$salbeg[Sexo==1],xlab="Experiência na função",ylab="Salário")
points(Regressao_Linear_1_$work[Sexo==0],Regressao_Linear_1_$salbeg[Sexo==0], pch = 9)
#Com este grafico de dispers'ao de Salario versus Experiencia 
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

cor(Regressao_Linear_1_[2:7])

#separacao do salario por sexo
table(Regressao_Linear_1_$sex_males)
homens_salarios <- Regressao_Linear_1_$salbeg[Regressao_Linear_1_$sex_males==1]
#homens_salarios <- homens_salarios[1:216]
mulheres_salarios<-Regressao_Linear_1_$salbeg[Regressao_Linear_1_$sex_males==0]

mean(homens_salarios)-mean(mulheres_salarios)
# esta media mostra que sao os homens que granhao mais de R$2883. do salario medio.
mean(homens_salarios)
mean(mulheres_salarios)
t.test(homens_salarios ,mulheres_salarios)
#com valor p-value < 2.2e-16 significa que media de salario entre homens e mulheres sao muito grandes.


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

cor(Regressao_Linear_1_[2:7])
# As variaveis mais relevantes para entrar na regressao linear múltipla sao edlevel e sex_males
salario_futuro_func=lm(Regressao_Linear_1_$salbeg~edlevel+time+age+work+sex_males,data = Regressao_Linear_1_)
salario_futuro_func
summary(salario_futuro_func)$r.squared
# Com este modelo acima consigo explicar um 49% da variabilidade dos meus dados.


#2.3 Interprete todos os parâmetros (Betas) da equação do modelo final.


#2.4 Teste a multicolinearidade através da estatística VIF do modelo final.
summary(salario_futuro_func)
vif(salario_futuro_func)
cbind(vif(salario_futuro_func))
# Apos analise com a funcao vif e visto que nao existem valores maiores que 10 , logo nao existem presenca de multicolinearidade.

#cook<-influence.measures(salario_futuro_func)
#plot(lm(salario_futuro_func),which = 4)


#2.5 Faça uma análise dos resíduos (gráficos e normalidade do teste) do modelo final.
salario_futuro_func=lm(Regressao_Linear_1_$salbeg~edlevel+time+age+work+sex_males,data = Regressao_Linear_1_)
salario_futuro_func



#2.6 O modelo/ equação está adequada?
salario_futuro_func=lm(Regressao_Linear_1_$salbeg~edlevel+time+age+work+sex_males,data = Regressao_Linear_1_)
salario_futuro_func
summary(salario_futuro_func)$r.squared
# Com este modelo acima consigo explicar um 49% da variabilidade dos meus dados.

#2.7 Faça um previsão de salário para um candidato masculino/ feminino. 
#Precisar conter na formula todas as variáveis relevantes.
populacao_masc<-Regressao_Linear_1_[,c()]
previsao_masculina<-predict(salario_futuro_func,)





