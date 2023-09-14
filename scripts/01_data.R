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

# ---------------------
# Restringimos a mayores o iguales de 18 (edad para trabajar)
dataset <- dataset %>%  filter(age >= 18 )  ##  ************************* CONFIRMAR
table(dataset$age)

# Restringimos la información a los trabajadores con ingreso positivo
dataset <- dataset %>%  filter(y_ingLab_m_ha > 0 )  ##  ************************* CONFIRMAR



# Dummy de jef(a) de hogar
dataset$jefx_hog <- ifelse(dataset$p6050 == 1, 1, 0)
# Dummy de pareja, esposo(a), cónyuge
dataset$pareja <- ifelse(dataset$p6050 == 2, 1, 0)
# Dummy de hijo de jefe de hogar
dataset$hijx_hog <- ifelse(dataset$p6050 == 3, 1, 0)


# Generamos datos para los problemas 2 y 3
# edad al cuadrado
dataset <- dataset %>% mutate( age2 = age * age)

# Dummy de sexo femenino 
dataset$female <- ifelse(dataset$sex == 0, 1, 0)

# Dummy por tipo de trabajo
dataset$obrero_particular <- ifelse(dataset$relab == 1, 1, 0)
dataset$obrero_gobierno <- ifelse(dataset$relab == 2, 1, 0)
dataset$empleado_domestico <- ifelse(dataset$relab == 3, 1, 0)
dataset$self_employed <- ifelse(dataset$relab == 4, 1, 0)
dataset$patron_empleador <- ifelse(dataset$relab == 5, 1, 0)
dataset$worker_fam_nowage <- ifelse(dataset$relab == 6, 1, 0)
dataset$worker_comp_nowage <- ifelse(dataset$relab == 7, 1, 0)
dataset$jornalero_peon <- ifelse(dataset$relab == 8, 1, 0)
dataset$otro_trabajo <- ifelse(dataset$relab == 9, 1, 0)

# Nos quedamos con las variables a usar
dataset <- dataset %>% 
  select(age, age2, clase, orden, directorio, cuentaPropia, depto, estrato1, formal,
  obrero_particular, obrero_gobierno, empleado_domestico, self_employed, patron_empleador,
  worker_fam_nowage, worker_comp_nowage, jornalero_peon, otro_trabajo,
  microEmpresa, jefx_hog, pareja, hijx_hog, p6210s1,female, sizeFirm, y_ingLab_m_ha)

#----------------------------------------------------------------
summary(dataset)

# Revisamos los NAs de la base
sapply(dataset, function(x) sum(is.na(x)))



