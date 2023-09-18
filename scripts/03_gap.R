#packages
require("pacman")
library(fastDummies)
p_load("tidyverse","stargazer")

glimpse(dataset)

#Tabla con estadísticas descriptivas
stargazer(data.frame(dataset), header=FALSE, type='text',title="Variables Included in the Selected Data Set")

#a) 
reg1<-lm(log(num_salarioHora) ~ cat_sexo, data = dataset)
stargazer(reg1, type="text", digits=3) 
stargazer(reg1, digits=3, align=TRUE, type="latex", out="views/4reg1.tex")

#b) 
reg2<-lm(log(num_salarioHora) ~ cat_sexo+cat_posicion+cat_ocupacion+cat_estrato+cat_formalidad+cat_empresa+cat_educacion+num_edad, data = dataset)
stargazer(reg2,type="text",digits=3)
summary(reg2)

#FWL: 
dataset<-dataset %>% mutate(sexResid=lm(as.numeric(cat_sexo)~cat_posicion+cat_ocupacion+cat_estrato+cat_formalidad+cat_empresa+cat_educacion+num_edad,dataset)$residuals) #Residuals of sex~x 
dataset<-dataset %>% mutate(logyResid=lm(log(num_salarioHora)~cat_posicion+cat_ocupacion+cat_estrato+cat_formalidad+cat_empresa+cat_educacion+num_edad,dataset)$residuals) #Residuals of logy~x 
reg3<-lm(logyResid~sexResid,dataset)
stargazer(reg3,type="text",digits=3) 
stargazer(reg3, digits=3, align=TRUE, type="latex", out="views/4reg3.tex")

#Boostrap

p_load("boot")

#Acá me surge la duda de si por cada iteración deberían cambiar las columnas de los errores o se usan las que ya tengo como estoy haciendo
eta_fn<-function(data,index){
  data<-data %>% mutate(sexResid=lm(as.numeric(cat_sexo)~cat_posicion+cat_ocupacion+cat_estrato+cat_formalidad+cat_empresa+cat_educacion+num_edad,data)$residuals) #Residuals of sex~x 
  data<-data %>% mutate(logyResid=lm(log(num_salarioHora)~cat_posicion+cat_ocupacion+cat_estrato+cat_formalidad+cat_empresa+cat_educacion+num_edad,data)$residuals) #Residuals of logy~x 
  coef(lm(logyResid~sexResid, data = data, subset = index))[2]  #returns the second coefficient of the linear regression
}

eta_fn(dataset,1:nrow(dataset))

boot(dataset, eta_fn, R = 2000) 


## 1. estimacion de los coeficientes 

modelo0 <- lm(log(num_salarioHora) ~ num_edad + I(num_edad^2), data = dataset, subset = (cat_sexo == "Hombre"))
modelo1 <- lm(log(num_salarioHora) ~ num_edad + I(num_edad^2), data = dataset, subset = (cat_sexo == "Mujer"))

## utilizo el método del ploteo del perfil de ingresos, donde utilizo los coeficientes estimados para trazar el perfil de ingresos en funcion de la edad. 
#primero creo un conjunto de edades para las cuales se desea hacer predicciones 
#luego utilizo los coeficientes estimados 

edades <- seq(min(dataset$num_edad), max(dataset$num_edad), by = 1)  # Secuencia de edades

predicciones0 <- predict(modelo0, newdata = data.frame(num_edad=edades))  # Predicciones
predicciones1 <- predict(modelo1, newdata = data.frame(num_edad=edades))  # Predicciones


#Boostrap

p_load("boot")

dataset0 <- dataset[dataset$cat_sexo == "Hombre",]
dataset1 <- dataset[dataset$cat_sexo == "Mujer",]

eta_peak<-function(data,index){
  coefficients <- coef(lm(log(num_salarioHora)~ num_edad + I(num_edad^2), data = data, subset = index))#returns the second coefficient of the linear regression
  coef2 <- coefficients[2]
  coef3 <- coefficients[3]
  coef_peak <- -(coef2/(2*coef3))
  return(coef_peak)
}

#Para los hombres
eta_peak(dataset0,1:nrow(dataset0))
valores0 <- boot(dataset0, eta_peak, R = 2000) 
valores0
quantile(valores0$t[,1], 0.025)
quantile(valores0$t[,1], 0.975)

#Para las mujeres
eta_peak(dataset1,1:nrow(dataset1))
valores1 <- boot(dataset1, eta_peak, R = 2000) 
valores1
quantile(valores1$t[,1], 0.025)
quantile(valores1$t[,1], 0.975)

#------------------------------------
# ruta del gráfico
graph_dt <- file.path(directorioResultados, "earnings_gen_gap.png")
# establezco dispositivo gráfico
png(filename = graph_dt ,width = 800, height = 600 )

# Gráfico del perfil de ingresos
plot(dataset$num_edad, log(dataset$num_salarioHora), xlab = "Edad", ylab = "log(Ingresos)", col =  alpha("grey", 0.8), main = "Perfil de Ingresos vs. Edad por sexo")
lines(edades, predicciones1, col = "red", lwd = 1)  # Línea de predicciones
lines(edades, predicciones0, col = "blue", lwd = 1)  # Línea de predicciones
# Añadir líneas verticales
abline(v = 42.50812, col = "red", lwd = 2, lty = 1)  # Línea para el valor central (estilo de línea discontinua)
abline(v = quantile(valores1$t[,1], 0.025), col = "red", lwd = 2, lty = 3) # Línea para el límite inferior (estilo de línea punteada)
abline(v = quantile(valores1$t[,1], 0.975), col = "red", lwd = 2, lty = 3) # Línea para el límite superior (estilo de línea punteada)
abline(v = 48.27164, col = "blue", lwd = 2, lty = 1)  # Línea para el valor central (estilo de línea discontinua)
abline(v = quantile(valores0$t[,1], 0.025), col = "blue", lwd = 2, lty = 3) # Línea para el límite inferior (estilo de línea punteada)
abline(v = quantile(valores0$t[,1], 0.975), col = "blue", lwd = 2, lty = 3) # Línea para el límite superior (estilo de línea punteada)
# Añadir leyenda
legend("topright",                    # posición de la leyenda
       legend = c("Mujer", "Hombre"), # etiquetas
       col = c("red", "blue"),        # colores de las líneas en la leyenda
       lwd = 1,                       # grosor de las líneas en la leyenda
       bg = "white")                  # color de fondo de la leyenda
# Agregar pie de página
#mtext("Lineas verticales continuas: Peak ages para cada sexo. Lineas verticales punteadas: Intervalos de confianza.", side = 1, line = 3.8, adj = 0, cex = 0.6)

# cerrar el dispositivo gráfico PNG
dev.off()















