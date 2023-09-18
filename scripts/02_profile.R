## corrección  de código 
## 1. estimacion de los coeficientes 
reg31 <- lm(log(num_salarioHora) ~ num_edad + I(num_edad^2), data = dataset)
stargazer(reg31, type="tex", digits=3) 
stargazer(reg31, digits=3, align=TRUE, type="latex", out="views/4reg2.tex", omit.stat=c("adj.rsq", "f", "ser"))


## utilizo el método del ploteo del perfil de ingresos, donde utilizo los coeficientes estimados para trazar el perfil de ingresos en funcion de la edad. 
#primero creo un conjunto de edades para las cuales se desea hacer predicciones 
#luego utilizo los coeficientes estimados 

edades <- seq(min(dataset$num_edad), max(dataset$num_edad), by = 1)  # Secuencia de edades
predicciones <- predict(reg31, newdata = data.frame(num_edad=edades))  # Predicciones


#Boostrap

install.packages("boot")
library(boot)


#Peak Age
eta_peak<-function(data,index){
  coefficients <- coef(lm(log(num_salarioHora)~ num_edad + I(num_edad^2), data = data, subset = index))#returns the second coefficient of the linear regression
  coef2 <- coefficients[2]
  coef3 <- coefficients[3]
  coef_peak <- -(coef2/(2*coef3))
  return(coef_peak)
}

eta_peak(dataset,1:nrow(dataset))

#set.seed(666)
#library(boot)
valores <- boot(dataset, eta_peak, R = 2000) 

valores
quantile(valores$t[,1], 0.025)
quantile(valores$t[,1], 0.975)


# la ruta completa del archivo de destino en la carpeta "views"
ruta_grafico <- file.path(directorioResultados, "perfil_ingresos_vs_edad.png")

# Establezco el dispositivo gráfico PNG
png(filename = ruta_grafico, width = 800, height = 600)  # Ajusta el ancho y alto según tus preferencias

# Creo el gráfico
plot(dataset$num_edad, log(dataset$num_salarioHora), xlab = "Edad", ylab = "log(Ingresos)", col =  alpha("grey", 0.8), main = "Perfil de Ingresos vs. Edad")
lines(edades, predicciones, col = "red", lwd = 2)  # Línea de predicciones

# Añadir líneas verticales
abline(v = 45.31037, col = "blue", lwd = 2, lty = 2)  # Línea para el valor central (estilo de línea discontinua)
abline(v = quantile(valores$t[,1], 0.025), col = "blue", lwd = 2, lty = 3) # Línea para el límite inferior (estilo de línea punteada)
abline(v = quantile(valores$t[,1], 0.975), col = "blue", lwd = 2, lty = 3) # Línea para el límite superior (estilo de línea punteada)

# Cerrar el dispositivo gráfico PNG
dev.off()
