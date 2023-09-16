#packages
require("pacman")
library(fastDummies)
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

#Limpieza de datos
data <- data %>% select(y_ingLab_m_ha, sex, relab, age)
data<-data %>% drop_na(y_ingLab_m_ha, sex, relab, age)
data <- dummy_cols(data, select_columns = "relab") #Creo las dummys para esta categoría
data$relab <- NULL
glimpse(data)

#Tabla con estadísticas descriptivas
stargazer(data.frame(data), header=FALSE, type='text',title="Variables Included in the Selected Data Set")

#a) 
reg1<-lm(log(y_ingLab_m_ha) ~ sex, data = data)
stargazer(reg1,type="text",digits=7)

#b) 
reg2<-lm(log(y_ingLab_m_ha) ~ relab_1+relab_2+relab_3+relab_8+age, data = data)
stargazer(reg2,type="text",digits=7)

#FWL: 
data<-data %>% mutate(sexResid=lm(sex~relab_1+relab_2+relab_3+relab_8+age,data)$residuals) #Residuals of sex~x 
data<-data %>% mutate(logyResid=lm(log(y_ingLab_m_ha)~relab_1+relab_2+relab_3+relab_8+age,data)$residuals) #Residuals of logy~x 
reg3<-lm(logyResid~sexResid,data)
stargazer(reg1,reg3,type="text",digits=7) 

#Boostrap
p_load("boot")




