# 1| Web scrapping --------------------------------------------------------
# De la página de Ignacio se extrae las diez tablas y se concatenan por filas. 
if (primeraVez == TRUE) {
  link <- paste0(
    "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html"
  )
  
  dataset <- data.frame()
  for (url in link) {
    print(url)
    table0 <- read_html(url) |>  html_table() 
    table0 <- as.data.frame(table0[[1]])
    dataset <- rbind(dataset, table0)
  }
  
  write.csv(x = dataset, file = paste0(directorioDatos, 'dataframe.csv'), 
            row.names = FALSE)
} else {
  dataset <- read.csv(file = paste0(directorioDatos, 'dataframe.csv'))
}

# 2| Limpieza -------------------------------------------------------------
# 2.1| Selección de observaciones -----------------------------------------
# Restringimos a las observaciones para individuos:
# - con edad mayor o igual a 18 años (edad para trabajar).
# - con ingreso por hora positivo.
dataset <- dataset |> filter(age >= 18 )
dataset <- dataset |> filter(y_ingLab_m_ha > 0)

# 2.2| Creación de variables ----------------------------------------------
# Convertimos las diferentes variables numéricas en variables categóricas para 
# que, mediante funciones de tidymodels, podamos codificarlas vía 
# one-hot-encoding. 

# Notas.
# - DEPTO. Solo se cuenta con las entrevistas para Bogotá, por lo cual no
#   tomamos la variable como posible predictora.

#creamos dummy de sexo femenino
dataset$female <- ifelse(dataset$sex == 0, 1, 0)
dataset$sex <- dataset$female

# Revisamos si quedan missing values
  sapply(dataset, function(x) sum(is.na(x)))

# Como es una sola observación missing en nivel educativo más alto, asumimos que no tenía educación

table(dataset$p6210)

dataset <- dataset |> 
  mutate(p6050 = case_when(p6050 == 1 ~ 'Jefa o jefe del hogar',
                           p6050 == 2 ~ 'Pareja de la cabeza del hogar',
                           p6050 == 3 ~ 'Descendiente de la cabeza del hogar',
                           TRUE ~ 'Otro')) |> 
  mutate(relab = case_when(relab == 1 ~ 'Obrero del sector privado',
                           relab == 2 ~ 'Obrero del gobierno',
                           relab == 3 ~ 'Empleado doméstico',
                           relab == 4 ~ 'Independiente',
                           relab == 5 ~ 'Empleador',
                           relab == 6 ~ 'Trabajador familiar',
                           relab == 7 ~ 'Voluntariado',
                           relab == 8 ~ 'Jornalero',
                           relab == 9 ~ 'Otro',
                           TRUE ~ NA_character_)) |> 
  mutate(sex = case_when(sex == 0 ~ 'Hombre',
                         sex == 1 ~ 'Mujer')) |> 
  mutate(estrato1 = case_when(estrato1 == 1 ~ 'Uno',
                              estrato1 == 2 ~ 'Dos',
                              estrato1 == 3 ~ 'Tres',
                              estrato1 == 4 ~ 'Cuatro',
                              estrato1 == 5 ~ 'Cinco',
                              estrato1 == 6 ~ 'Seis')) |> 
  mutate(formal = case_when(formal == 0 ~ 'Informal',
                            formal == 1 ~ 'Formal')) |> 
  mutate(microEmpresa = case_when(microEmpresa == 1 ~ 'Microempresa',
                                  microEmpresa == 0 ~ 'Empresa pequeña, mediana, o grande')) |> 
  mutate(p6210 = case_when(p6210 == 1 ~ 'Ninguno',
                           p6210 == 2 ~ 'Preescolar',
                           p6210 == 3 ~ 'Primaria',
                           p6210 == 4 ~ 'Secundaria',
                           p6210 == 5 ~ 'Bachillerato',
                           p6210 == 6 ~ 'Superior',
                           TRUE ~ 'Ninguno')) |> 
  rename(id_vivienda = directorio,
         id_hogar = secuencia_p,
         id_persona = orden,
         cat_posicion = p6050,
         cat_ocupacion = relab,
         cat_sexo = sex,
         cat_estrato = estrato1,
         cat_formalidad = formal,
         cat_empresa = microEmpresa,
         cat_educacion = p6210,
         num_edad = age,
         num_salarioHora = y_ingLab_m_ha) |> 
  mutate(across(starts_with('cat_'), as.factor)) |> 
  mutate(across(starts_with('id_'), as.character))

# Nos quedamos con las variables a usar.
dataset <- dataset |> 
  select(c(starts_with('id_'), starts_with('cat_'), starts_with('num_')))

# Orden de las observaciones en variables relevantes
dataset <- dataset  |> 
  mutate(cat_educacion = factor(cat_educacion, levels = c('Ninguno', 'Primaria',
                                                          'Secundaria', 'Bachillerato', 'Superior'))) |> 
  mutate(cat_estrato = factor(cat_estrato, levels = c('Uno', 'Dos', 'Tres', 'Cuatro', 'Cinco', 'Seis'))) |> 
  mutate(cat_posicion = factor(cat_posicion, levels = c('Jefa o jefe del hogar', 'Pareja de la cabeza del hogar',
                                                      'Descendiente de la cabeza del hogar', 'Otro'))) 

# 3| Exportar estadística descriptiva -------------------------------------
tbl <- dataset |> 
  select(c(starts_with('cat_'), starts_with('num_'))) |> 
  tbl_summary(include = everything(),
              type = starts_with('num_') ~ 'continuous2',
              label = list(cat_estrato ~ 'Estrato socioeconómico',
                           cat_sexo ~ 'Sexo',
                           cat_posicion ~ 'Posición dentro del hogar',
                           cat_educacion ~ 'Nivel educativo más alto alcanzado',
                           cat_ocupacion ~ 'Ocupación',
                           cat_formalidad ~ 'Empleo formal o informal',
                           cat_empresa ~ 'Tamaño de la empresa',
                           num_edad ~ 'Edad',
                           num_salarioHora ~ 'Salario por hora'),
              statistic = list(all_continuous() ~ c("{mean} ({sd})",
                                                    "({min}, {max})"),
                               all_categorical() ~ "{n}  ({p}%)"),  #  / {N}
              missing_text = "(Valores faltantes)", sort= all_categorical() ~ "alphanumeric"  ) |> 
  add_stat_label(label = list(all_categorical() ~ "",
                              all_continuous() ~ c("Promedio (Desviación std)",
                                                   "Mínimo y máximo"))) |> 
  modify_header(label = "**Variable**") |> 
  bold_labels() 

tbl
gtsave(as_gt(tbl), filename="stores/tab2.tex")
  
# Agregar gráfica de salario. Distribución con o sin media

