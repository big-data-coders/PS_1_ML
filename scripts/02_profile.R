
# 1| Web scrapping --------------------------------------------------------
# De la página de Ignacio se extrae las diez tablas y se concatenan por filas. 
install.packages("rvest")
library(rvest)

if (primeraVez <- TRUE) {
  link <- paste0(
    "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html"
  )
  
  dataset <- data.frame()
  for (url in link) {
    print(url)
    table0 <- read_html(url) %>%  html_table() 
    table0 <- as.data.frame(table0[[1]])
    dataset <- rbind(dataset, table0)
  }
  
  write.csv(x = dataset, file = paste0(Directoriodatos, 'dataframe.csv'), 
            row.names = FALSE)
} else {
  dataset <- read.csv(file = paste0(Directoriodatos, 'dataframe.csv'))
}

___________________________________________________________________________________________________________________________________

#comienzo por observar los datos instalando de igual fomra paquetes para ello 

install.packages("skimr")
library(skimr)

dataset%>%head()

names(dataset)
str(dataset)
summary(dataset)
# vista previa de los datos 

------------------------------------------------------------------------------------------------------------------------------------
#detectar valores nulos y numero de datos nulos 
  
colSums(is.na(dataset))
total_na <- sum(is.na(dataset))
print(total_na)












#estat describ

#summary(dataset)

#num filas / columnas 

#num_filas <- nrow(dataset)
#num_columnas <- nrow(dataset)

#resultado 

#print(paste("numero de filas", num_filas))
#print(paste("numero de columnas", num_columnas))



#










#Restringir variable age en los datos apartir de 18 años 

#install.packages("dplyr")
#library(dplyr)

#dataset_filtrado <- dataset %>% filter (age >=18)








