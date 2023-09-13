# 1| Web scrapping --------------------------------------------------------
# De la página de Ignacio se extrae las diez tablas y se concatenan por filas. 
if (primeraVez == TRUE) {
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
  
  write.csv(x = dataset, file = paste0(directorioDatos, 'dataframe.csv'), 
            row.names = FALSE)
} else {
  dataset <- read.csv(file = paste0(directorioDatos, 'dataframe.csv'))
}

#---------------------------------------------------------------------------------
# Verificamos contenido de los datos

# Revisamos los NAs de la base
sapply(dataset, function(x) sum(is.na(x)))

summary(dataset)
# ---------------------
# Restringimos a mayores o iguales de 18 (edad para trabajar)
dataset <- dataset %>%  filter(age >= 18 )
table(dataset$age)

# Generamos datos para los problemas 2 y 3
# edad al cuadrado
dataset <- dataset %>% mutate( age2 = age * age)
dataset$age[1]
dataset$age2[1]


# Dummy = 1 si sexo es femenino
dataset <- dataset %>% mutate(female = case_when(sex == 0 ~ 1, sex == 1 ~ 0) )
table(dataset$female)
table(dataset$sex)



# ---------------------
# Seleccionar variables relevantes  // COORDINAR! --------+++++++++++++
  # dataset <- dataset %>% select ()

# Nuevas variables: -------------
# Ingresos laborales (1er, 2do, y todos los trabajos)

# tipo de trabajo


class(dataset$p7510s6)
?table
table(dataset$p7510s6) 
table(dataset$p7510s7a1)
p7510s7a1 

install.packages("expss")
library(expss)
dataset %>%
  tab_cells(p7510s6, p7510s7a1) %>%
  tab_stat_fun(Mean = w_mean , "Std. dev." = w_sd, "Valid N"= w_n , "Min" = w_min, "Max" = w_max,  method = list) %>%
  tab_pivot

table(dataset$oficio)


table(dataset$sex)
dataset$fem <- dataset$sex

