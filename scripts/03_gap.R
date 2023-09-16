#packages
require("pacman")
library(fastDummies)
p_load("tidyverse","stargazer")

glimpse(dataset)

#Tabla con estadísticas descriptivas
stargazer(data.frame(dataset), header=FALSE, type='text',title="Variables Included in the Selected Data Set")

#a) 
reg1<-lm(log(num_salarioHora) ~ cat_sexo, data = dataset)
stargazer(reg1, type="text", digits=7) 
stargazer(reg1, digits=7, align=TRUE, type="latex", out="reg1")

#b) 
reg2<-lm(log(num_salarioHora) ~ cat_sexo+id_vivienda+id_hogar+id_persona+cat_posicion+cat_ocupacion+cat_estrato+cat_formalidad+cat_empresa+cat_educacion+num_edad, data = dataset)
stargazer(reg2,type="text",digits=7)
stargazer(reg2, digits=7, align=TRUE, type="latex", out="reg2")

#FWL: 
dataset<-dataset %>% mutate(sexResid=lm(cat_sexo~id_vivienda+id_hogar+id_persona+cat_posicion+cat_ocupacion+cat_estrato+cat_formalidad+cat_empresa+cat_educacion+num_edad,dataset)$residuals) #Residuals of sex~x 
dataset<-dataset %>% mutate(logyResid=lm(log(num_salarioHora)~id_vivienda+id_hogar+id_persona+cat_posicion+cat_ocupacion+cat_estrato+cat_formalidad+cat_empresa+cat_educacion+num_edad,dataset)$residuals) #Residuals of logy~x 
reg3<-lm(logyResid~sexResid,data)
stargazer(reg1,reg3,type="text",digits=7) 
stargazer(reg2, digits=7, align=TRUE, type="latex", out="reg2")

#Boostrap
p_load("boot")




