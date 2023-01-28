#Regression multiple
#By Joe Monkila

library(readxl)
library(ggplot2)
library(car)
library(QuantPsyc)
#panel <- read_excel("~/Downloads/panel.xlsx")
#View(panel)

#donnees=read_excel("~/Downloads/panel.xlsx")

donnees = readxl::read_excel("~/Downloads/panel.xlsx")
names(donnees)=c("idhous","year","region","refper","hldtyp","nbpers","nbkid","aduk1","aduk2","idispy",
                 "sex","age","edcat","occupa","relarp","iavsy","hldtypnbr","tranch5","tranch10","tranch15",
                 "contrib","eligibilite","status","leveled","genrenb","avsnb","regnb","eligiblenb")

#names(donnees) = c("idhous","year","occupa")
head(donnees)
summary(donnees)

#chart 1
nuage = ggplot2(donnees, aes(x = year,y = age))
nuage + geom_point() + geom_smooth(method= "lm")

#Regression Lineare simple
model1 = lm(year~age, data = donnees)
summary(model1)
sqrt(0,06903)
cor(donnees$year, donnees$year)

#Regression Lineare multiple
model2 = lm(year~age, data = donnees)
summary(model2)

#Linearite
vif(model2)
1/vif(model2)
cor(donnees[,-6])
model3= lm(year~age, data = donnees)
summary(model3)
vif1=(1/(1-0.8086))
vif1
cor(donnees$year, donnees$age)


  
  
  
  
  
  
  
  
  
  
