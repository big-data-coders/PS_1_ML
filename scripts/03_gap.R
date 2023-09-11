#packages
require("pacman")
p_load("tidyverse","stargazer")

#Lectura de los datos
current_user <- Sys.info()["user"]

if (current_user == "maria") {
  path <- "C:/Users/maria/Desktop/git"
} else if (current_user == "usuario_2") {
  path <- "ruta/en/otro/computador.csv"
} else if (current_user == "usuario_3") {
  path <- "ruta/en/otro/computador.csv"
} else {
  path <- "ruta/en/otro/computador.csv"
}

data <- read.csv(paste0(path,"/PS_1_ML/stores/dataframe.csv", sep=""))
glimpse(data)
data <- data %>% select(y_ingLab_m_ha, sex, relab, age)


#Tabla con estadísticas descriptivas
stargazer(data.frame(data), header=FALSE, type='text',title="Variables Included in the Selected Data Set")


#a) 
reg1<-lm(log(y_ingLab_m_ha) ~ sex, data = data)
stargazer(reg1,type="text",digits=7)

#b) #falta hacer dummyes de relab
reg2<-lm(log(y_ingLab_m_ha) ~ sex + relab, data = data)
stargazer(reg2,type="text",digits=7)

lax <- "relab + age" #como hago para llamarla a continuacion

#FWL: 
data<-data %>% 
  drop_na(sex, relab, age) %>% 
  mutate(sexResid=lm(sex~relab+age,data)$residuals) #Residuals of sex~lax 


data<-data %>% mutate(logyResid=lm(logy_ingLab_m_ha~relab+age,data)$residuals) #Residuals of logy~lax 

reg3<-lm(logyResid~sexResid,data)
stargazer(reg1,reg3,type="text",digits=7) 






