# 1| Partición ------------------------------------------------------------
# Construimos los datos de entrenamiento y de evaluación para la semilla
# dada inicialmente.
# Nota. Es recomendable transformar la variable dependiente en su versión
# logarítmica por fuera de la función `recipe` para evitar errores.
dataset_split    <- initial_split(data = dataset,
                                  prop = 0.7, 
                                  strata = num_salarioHora)
dataset_training <- dataset_split |> training()
dataset_testing  <- dataset_split |> testing()

# 2| Estimación -----------------------------------------------------------
# Todos los modelos corresponden a regresiones lineales, por lo que podemos
# definir el mismo motor de estimación para todos. 
lm_model <- linear_reg() |> set_engine('lm') |> set_mode('regression')

# Creamos un dataframe vacío que almacene los resultados. En particular, que
# contenga el nombre del modelo y el RMSE tanto en logaritmo natural como en
# nivel, pues la interpretación varía en función de la escala de la variable
# dependiente.
resultados <- tibble(MODEL        = character(),
                     RMSE_LOG     = numeric(),
                     RMSE_EXP_LOG = numeric(),
                     RMSE_LEVEL   = numeric())

# 2.1| Cálculo del RMSE --------------------------------------------------
# Creamos una 'receta' que modifica los datos hacia la estructura que
# necesitamos. Posteriormente, estimamos el modelo y, con la porción de datos
# de validación, estimamos el RMSE por fuera de muestra.
# Nota. Las interacciones que queramos incluir son vía: step_interact().

# A| Edad -----------------------------------------------------------------
dataset_recipe_age <- recipe(num_salarioHora ~ num_edad, 
                             data = dataset_training) |>
  # update_role(id_vivienda, id_hogar, id_persona, new_role = 'ID') |>  
  step_log(num_salarioHora, base = exp(1)) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_poly(num_edad, degree = 2)

lm_fit_1 <- lm_model |> 
  fit(num_salarioHora ~ num_edad, data = dataset_training)

wf_lm_model_1 <- workflow() |> 
  add_recipe(dataset_recipe_age) |> 
  add_model(lm_model) |> 
  last_fit(split = dataset_split)

resultado <- wf_lm_model_1[['.predictions']][[1]] |> 
  select(c('num_salarioHora', '.pred')) |> 
  mutate(MSE_LOG = (num_salarioHora - .pred)^2) 

resultado <- resultado |> 
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred))^2) |> 
  # Corrección del sesgo. exp(prediccion) estima la mediana, mientras que
  # exp(prediccion + varianza_prediccion/2) estima la media.
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred+var(resultado$.pred)/2))^2)
RMSE_LOG   <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LOG))
RMSE_LEVEL <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LEVEL))

resultados <- resultados |> 
  bind_rows(tibble(MODEL        = 'Edad', 
                   RMSE_LOG     = RMSE_LOG,
                   RMSE_EXP_LOG = exp(RMSE_LOG),
                   RMSE_LEVEL   = RMSE_LEVEL))

# B| Género ---------------------------------------------------------------
dataset_recipe_gender <- recipe(num_salarioHora ~ cat_sexo, 
                                data = dataset_training) |>
  # update_role(id_vivienda, id_hogar, id_persona, new_role = 'ID') |>  
  step_log(num_salarioHora, base = exp(1)) |> 
  step_dummy(all_nominal_predictors())

lm_fit_2 <- lm_model |> 
  fit(num_salarioHora ~ cat_sexo, data = dataset_training)

wf_lm_model_2 <- workflow() |> 
  add_recipe(dataset_recipe_gender) |> 
  add_model(lm_model) |> 
  last_fit(split = dataset_split)

resultado <- wf_lm_model_2[['.predictions']][[1]] |> 
  select(c('num_salarioHora', '.pred')) |> 
  mutate(MSE_LOG = (num_salarioHora - .pred)^2) 

resultado <- resultado |> 
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred))^2) |> 
  # Corrección del sesgo. exp(prediccion) estima la mediana, mientras que
  # exp(prediccion + varianza_prediccion/2) estima la media.
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred+var(resultado$.pred)/2))^2)
RMSE_LOG   <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LOG))
RMSE_LEVEL <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LEVEL))

resultados <- resultados |> 
  bind_rows(tibble(MODEL        = 'Género', 
                   RMSE_LOG     = RMSE_LOG,
                   RMSE_EXP_LOG = exp(RMSE_LOG),
                   RMSE_LEVEL   = RMSE_LEVEL))

# C| Educación ------------------------------------------------------------
dataset_recipe_educ <- recipe(num_salarioHora ~ cat_educacion, 
                              data = dataset_training) |>
  # update_role(id_vivienda, id_hogar, id_persona, new_role = 'ID') |>  
  step_log(num_salarioHora, base = exp(1)) |> 
  step_dummy(all_nominal_predictors())

lm_fit_3 <- lm_model |> 
  fit(num_salarioHora ~ cat_educacion, data = dataset_training)

wf_lm_model_3 <- workflow() |> 
  add_recipe(dataset_recipe_educ) |> 
  add_model(lm_model) |> 
  last_fit(split = dataset_split) 

resultado <- wf_lm_model_3[['.predictions']][[1]] |> 
  select(c('num_salarioHora', '.pred')) |> 
  mutate(MSE_LOG = (num_salarioHora - .pred)^2) 

resultado <- resultado |> 
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred))^2) |> 
  # Corrección del sesgo. exp(prediccion) estima la mediana, mientras que
  # exp(prediccion + varianza_prediccion/2) estima la media.
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred+var(resultado$.pred)/2))^2)
RMSE_LOG   <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LOG))
RMSE_LEVEL <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LEVEL))

resultados <- resultados |> 
  bind_rows(tibble(MODEL        = 'Educación', 
                   RMSE_LOG     = RMSE_LOG,
                   RMSE_EXP_LOG = exp(RMSE_LOG),
                   RMSE_LEVEL   = RMSE_LEVEL))

# D| Familiar -------------------------------------------------------------
# Incluimos características familiares. En particular, la posición que tiene
# la persona encuestada en casa (cabeza del hogar, pareja de la cabeza del
# hogar, o descendiente) y el estrato en que viven.
dataset_recipe_family <- recipe(num_salarioHora ~ cat_posicion + cat_estrato, 
                                data = dataset_training) |>
  # update_role(id_vivienda, id_hogar, id_persona, new_role = 'ID') |>  
  step_log(num_salarioHora, base = exp(1)) |> 
  step_dummy(all_nominal_predictors())

lm_fit_4 <- lm_model |> 
  fit(num_salarioHora ~ cat_posicion + cat_estrato, data = dataset_training)

wf_lm_model_4 <- workflow() |> 
  add_recipe(dataset_recipe_family) |> 
  add_model(lm_model) |> 
  last_fit(split = dataset_split)

resultado <- wf_lm_model_4[['.predictions']][[1]] |> 
  select(c('num_salarioHora', '.pred')) |> 
  mutate(MSE_LOG = (num_salarioHora - .pred)^2) 

resultado <- resultado |> 
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred))^2) |> 
  # Corrección del sesgo. exp(prediccion) estima la mediana, mientras que
  # exp(prediccion + varianza_prediccion/2) estima la media.
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred+var(resultado$.pred)/2))^2)
RMSE_LOG   <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LOG))
RMSE_LEVEL <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LEVEL))

resultados <- resultados |> 
  bind_rows(tibble(MODEL        = 'Familiar', 
                   RMSE_LOG     = RMSE_LOG,
                   RMSE_EXP_LOG = exp(RMSE_LOG),
                   RMSE_LEVEL   = RMSE_LEVEL))

# E| Empresarial ----------------------------------------------------------
# Incluimos características laborales. En particular, si la persona encuestada
# tiene un empleo formal (medido a través de la cotización a salud y pensión),
# si trabaja para una empresa con un tamaño relativamente pequeño (es decir,
# microempresas con cinco o menos empleados), y en qué tipo de trabajo se
# desempeña.
dataset_recipe_labor <- recipe(num_salarioHora ~ cat_formalidad +
                                 cat_empresa + cat_ocupacion, 
                               data = dataset_training) |>
  # update_role(id_vivienda, id_hogar, id_persona, new_role = 'ID') |>  
  step_log(num_salarioHora, base = exp(1)) |> 
  step_dummy(all_nominal_predictors())

lm_fit_5 <- lm_model |> 
  fit(num_salarioHora ~ cat_formalidad + cat_empresa + cat_ocupacion, 
      data = dataset_training)

wf_lm_model_5 <- workflow() |> 
  add_recipe(dataset_recipe_labor) |> 
  add_model(lm_model) |> 
  last_fit(split = dataset_split)

resultado <- wf_lm_model_5[['.predictions']][[1]] |> 
  select(c('num_salarioHora', '.pred')) |> 
  mutate(MSE_LOG = (num_salarioHora - .pred)^2) 

resultado <- resultado |> 
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred))^2) |> 
  # Corrección del sesgo. exp(prediccion) estima la mediana, mientras que
  # exp(prediccion + varianza_prediccion/2) estima la media.
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred+var(resultado$.pred)/2))^2)
RMSE_LOG   <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LOG))
RMSE_LEVEL <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LEVEL))

resultados <- resultados |> 
  bind_rows(tibble(MODEL        = 'Empresarial', 
                   RMSE_LOG     = RMSE_LOG,
                   RMSE_EXP_LOG = exp(RMSE_LOG),
                   RMSE_LEVEL   = RMSE_LEVEL))

# F| Unión ----------------------------------------------------------------
# Unimos los modelos estimados previamente.
dataset_recipe_all <- recipe(num_salarioHora ~ ., 
                             data = dataset_training) |>
  update_role(id_vivienda, id_hogar, id_persona, new_role = 'ID') |>  
  step_log(num_salarioHora, base = exp(1)) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_poly(num_edad, degree = 2)

lm_fit_6 <- lm_model |> 
  fit(num_salarioHora ~ . - id_vivienda - id_hogar - id_persona, 
      data = dataset_training)

wf_lm_model_6 <- workflow() |> 
  add_recipe(dataset_recipe_all) |> 
  add_model(lm_model) |> 
  last_fit(split = dataset_split)

resultado <- wf_lm_model_6[['.predictions']][[1]] |> 
  select(c('num_salarioHora', '.pred')) |> 
  mutate(MSE_LOG = (num_salarioHora - .pred)^2) 

resultado <- resultado |> 
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred))^2) |> 
  # Corrección del sesgo. exp(prediccion) estima la mediana, mientras que
  # exp(prediccion + varianza_prediccion/2) estima la media.
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred+var(resultado$.pred)/2))^2)
RMSE_LOG   <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LOG))
RMSE_LEVEL <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LEVEL))

resultados <- resultados |> 
  bind_rows(tibble(MODEL        = 'Todo', 
                   RMSE_LOG     = RMSE_LOG,
                   RMSE_EXP_LOG = exp(RMSE_LOG),
                   RMSE_LEVEL   = RMSE_LEVEL))

# G| Interacción ----------------------------------------------------------
# Incluimos heterogeneidad en los efectos por género.
dataset_recipe_more <- recipe(num_salarioHora ~ ., 
                              data = dataset_training) |>
  update_role(id_vivienda, id_hogar, id_persona, new_role = 'ID') |>  
  step_log(num_salarioHora, base = exp(1)) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_poly(num_edad, degree = 2) |> 
  step_interact(terms = ~ starts_with('cat_sexo'):starts_with('cat_ocupacion') +
                  starts_with('cat_sexo'):starts_with('cat_educacion'))

lm_fit_7 <- lm_model |> 
  fit(num_salarioHora ~ . - id_vivienda - id_hogar - id_persona, 
      data = dataset_training)

wf_lm_model_7 <- workflow() |> 
  add_recipe(dataset_recipe_more) |> 
  add_model(lm_model) |> 
  last_fit(split = dataset_split)

resultado <- wf_lm_model_7[['.predictions']][[1]] |> 
  select(c('num_salarioHora', '.pred')) |> 
  mutate(MSE_LOG = (num_salarioHora - .pred)^2) 

resultado <- resultado |> 
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred))^2) |> 
  # Corrección del sesgo. exp(prediccion) estima la mediana, mientras que
  # exp(prediccion + varianza_prediccion/2) estima la media.
  mutate(MSE_LEVEL = (exp(num_salarioHora) - exp(.pred+var(resultado$.pred)/2))^2)
RMSE_LOG   <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LOG))
RMSE_LEVEL <- sqrt((1/nrow(resultado))*sum(resultado$MSE_LEVEL))

resultados <- resultados |> 
  bind_rows(tibble(MODEL        = 'Interacción', 
                   RMSE_LOG     = RMSE_LOG,
                   RMSE_EXP_LOG = exp(RMSE_LOG),
                   RMSE_LEVEL   = RMSE_LEVEL))

# 2.2| Exportar resultado -------------------------------------------------
colnames(resultados) <- c('Modelo', 'RMSE (ln)', 'RMSE (exp del ln)', 
                          'RMSE (nivel)')
print(xtable(x = resultados, type = "latex", 
             caption = "Resultados de la raíz del error cuadrático medio para los siete modelos.", 
             label = "Tab:RMSE", align = c('c', 'c', 'c', 'c', 'c'), 
             digits = c(1, 1, 3, 2, 0)), 
      file = "views/RMSE_todos.tex",
      table.placement = 'H!',
      caption.placement = 'top',
      include.rownames = FALSE)

# 3| Estudio de valores atípicos ------------------------------------------
# El modelo con el error de predicción más bajo fue el modelo completo junto con
# los términos de interacción -a pesar de que los términos de interacción no
# contribuyeron significativamente a la predicción-. En ese sentido, extraemos
# los pronósticos y los valores observados, para posteriormente graficar la
# distribución del error de pronóstico.
pronostico <- tibble(ID    = wf_lm_model_7$.predictions[[1]]$.row,
                     ERROR = wf_lm_model_7$.predictions[[1]]$num_salarioHora - 
                       wf_lm_model_7$.predictions[[1]]$.pred)
pronostico <- left_join(x = pronostico, 
                        y = dataset |> mutate(ID = row_number(dataset)),
                        by = 'ID')
valorCritico <- quantile(pronostico$ERROR, probs = 0.025)

# Definimos las variables gráficas.
base_exp     = 1
heightExp    = 1
widthExp     = 1.2
scale_factor = base_exp/widthExp

graficaExportar <- pronostico |>
  ggplot(mapping = aes(x = ERROR, color = factor(1), fill = factor(1))) + 
  geom_histogram(aes(y=..density..), 
                 position = "identity", alpha = 0.3, show.legend = FALSE) +
  geom_vline(xintercept = valorCritico, color = 'red', linetype = 'dashed') +
  annotate("rect", xmin = -Inf, xmax = valorCritico, ymin = 0, ymax = Inf,
           alpha = .2) +
  labs(title    = 'Distribución del error de pronóstico',
       subtitle = '',
       x        = paste0('Diferencia entre el logaritmo del valor observado y\n', 
                         'el logaritmo del valor estimado'),
       y        = 'Probabilidad de ocurrencia') +
  geom_density(alpha = .2, show.legend = FALSE)  +
  scale_color_manual(values = c("#E69F00")) +
  scale_fill_manual(values = c("#E69F00")) +
  theme_classic() +
  scale_y_continuous(limits = c(0, NA), 
                     expand = expansion(mult = c(0, .05))) +
  scale_x_continuous(expand = expansion(mult = c(0, .05))) +
  theme(legend.position = 'bottom',
        text            = element_text(family = 'Georgia'),
        axis.text.x     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.text.y     = element_text(size = scale_factor * 10, family = 'Georgia'),
        axis.title.x    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        axis.title.y    = element_text(hjust = 0.5, size = scale_factor * 10, family = 'Georgia'),
        legend.text     = element_text(size = scale_factor * 10, family = 'Georgia'),
        strip.text      = element_text(size = scale_factor * 10, family = 'Georgia'),
        plot.title      = element_text(face = 'bold', size = 10, family = 'Georgia'),
        plot.subtitle   = element_text(size = 10, family = 'Georgia'),
        legend.key.size = unit(0.5 * scale_factor, 'cm')) 

nombreArchivo <- 'Distribución del error.png'
ggsave(filename = paste0(directorioResultados, nombreArchivo), plot = graficaExportar,
       width = 6 * widthExp, height = 4 * heightExp * widthExp, scale = scale_factor)

# Definimos la estadística descriptiva de dichas observaciones.
# Nota. Nótese que las que interesan son aquellas de valor negativo, pues
# son precisamente las observaciones donde el ingreso reportado es muy inferior
# al pronosticado por el modelo.
tbl <- pronostico |>
  filter(ERROR <= valorCritico) |>
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
              missing_text = "(Valores faltantes)") |> 
  add_stat_label(label = list(all_categorical() ~ "",
                              all_continuous() ~ c("Promedio (Desviación std)",
                                                   "Mínimo y máximo"))) |> 
  modify_header(label = "**Variable**") |> 
  bold_labels() 

gtsave(as_gt(tbl), filename="stores/tab3.tex")

# 4| Leave-one-out CV -----------------------------------------------------
# No es necesario estimar N regresiones y N pronósticos. En su lugar, vamos a 
# emplear la matriz de proyección de X para extraer los valores de 
# apalancamiento de cada error.
resultados <- tibble(MODEL        = character(),
                     RMSE_LOG     = numeric(),
                     RMSE_EXP_LOG = numeric())

# A| Primer modelo --------------------------------------------------------
# La receta ahora tiene en cuenta todos los datos, no solo la base de datos
# del entrenamiento.
dataset_recipe_all <- recipe(num_salarioHora ~ ., 
                             data = dataset) |>
  update_role(id_vivienda, id_hogar, id_persona, new_role = 'ID') |>  
  step_log(num_salarioHora, base = exp(1)) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_poly(num_edad, degree = 2)

datos    <- prep(dataset_recipe_all) 
matriz_x <- juice(datos)
matriz_y <- as.matrix(matriz_x[, 'num_salarioHora'])
matriz_x <- as.matrix(matriz_x |> select(-c('id_vivienda', 'id_hogar', 'id_persona', 'num_salarioHora')))
matriz_h <- matriz_x %*% solve(t(matriz_x) %*% matriz_x) %*% t(matriz_x)
valores_h <- diag(matriz_h)

error <- workflow() |> 
  add_recipe(dataset_recipe_all) |> 
  add_model(lm_model) |> 
  fit(data = dataset)

error <- error[["fit"]][["fit"]][["fit"]][["residuals"]]
loocv <- (1/length(error))*sum((error/(1-valores_h))^2)

resultados <- resultados |> 
  bind_rows(tibble(MODEL        = 'Todo', 
                   RMSE_LOG     = loocv,
                   RMSE_EXP_LOG = exp(loocv)))

# B| Segundo modelo -------------------------------------------------------
dataset_recipe_more <- recipe(num_salarioHora ~ ., 
                              data = dataset) |>
  update_role(id_vivienda, id_hogar, id_persona, new_role = 'ID') |>  
  step_log(num_salarioHora, base = exp(1)) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_poly(num_edad, degree = 2) |> 
  step_interact(terms = ~ starts_with('cat_sexo'):starts_with('cat_ocupacion') +
                  starts_with('cat_sexo'):starts_with('cat_educacion'))

datos    <- prep(dataset_recipe_more) 
matriz_x <- juice(datos)
matriz_y <- as.matrix(matriz_x[, 'num_salarioHora'])
matriz_x <- as.matrix(matriz_x |> 
                        select(-c('id_vivienda', 'id_hogar', 'id_persona', 
                                  'num_salarioHora', 'cat_ocupacion_Jornalero',
                                  'cat_sexo_Mujer_x_cat_ocupacion_Jornalero')))
matriz_h <- matriz_x %*% solve(t(matriz_x) %*% matriz_x) %*% t(matriz_x)
valores_h <- diag(matriz_h)

error <- workflow() |> 
  add_recipe(dataset_recipe_more) |> 
  add_model(lm_model) |> 
  fit(data = dataset)

error <- error[["fit"]][["fit"]][["fit"]][["residuals"]]
loocv <- (1/length(error))*sum((error/(1-valores_h))^2)

resultados <- resultados |> 
  bind_rows(tibble(MODEL        = 'Interacciones', 
                   RMSE_LOG     = loocv,
                   RMSE_EXP_LOG = exp(loocv)))

colnames(resultados) <- c('Modelo', 'RMSE (ln)', 'RMSE (exp del ln)')
print(xtable(x = resultados, type = "latex", 
             caption = "Resultados del leave-one-out cross-validation para los dos mejores modelos.", 
             label = "Tab:loocv", align = c('c', 'c', 'c', 'c'), 
             digits = c(1, 1, 3, 2)), 
      file = "views/loocv.tex",
      table.placement = 'H!',
      caption.placement = 'top',
      include.rownames = FALSE)
