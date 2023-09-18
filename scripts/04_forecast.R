# 1| Partición ------------------------------------------------------------
# Construimos los datos de entrenamiento y de evaluación para la semilla
# dada inicialmente.
dataset_split    <- initial_split(data = dataset |> select(-'cat_zona'),
                                  prop = 0.7, 
                                  strata = num_salarioHora)
dataset_training <- dataset_split |> training()
dataset_testing  <- dataset_split |> testing()

# 2| Estimación -----------------------------------------------------------
# Todos los modelos corresponden a regresiones lineales, por lo que podemos
# definir el mismo motor de estimación para todos. 
lm_model <- linear_reg() |> set_engine('lm') |> set_mode('regression')
resultados <- tibble(MODEL      = character(),
                     RMSE_LOG   = numeric(),
                     RMSE_LEVEL = numeric())

# 2.1| Definición del workflow --------------------------------------------
# Creamos una 'receta' que modifica los datos hacia la estructura que
# necesitamos. 
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
  last_fit(split = dataset_split) |> 
  collect_metrics()

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
  last_fit(split = dataset_split) |> 
  collect_metrics()

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
  last_fit(split = dataset_split) |> 
  collect_metrics()

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
  last_fit(split = dataset_split) |> 
  collect_metrics()

# F| Unión ----------------------------------------------------------------
# Unimos los modelos estimados previamente.
dataset_recipe_all <- recipe(num_salarioHora ~ ., 
                             data = dataset_training) |>
  update_role(id_vivienda, id_hogar, id_persona, new_role = 'ID') |>  
  step_log(num_salarioHora, base = exp(1)) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_poly(num_edad, degree = 2)

lm_fit_6 <- lm_model |> 
  fit(num_salarioHora ~ ., data = dataset_training)

wf_lm_model_6 <- workflow() |> 
  add_recipe(dataset_recipe_all) |> 
  add_model(lm_model) |> 
  last_fit(split = dataset_split) |> 
  collect_metrics()

# G| Interacción ----------------------------------------------------------
# Incluimos heterogeneidad en los efectos por género.
dataset_recipe_more <- recipe(num_salarioHora ~ ., 
                              data = dataset_training) |>
  update_role(id_vivienda, id_hogar, id_persona, new_role = 'ID') |>  
  step_log(num_salarioHora, base = exp(1)) |> 
  step_dummy(all_nominal_predictors()) |> 
  step_poly(num_edad, degree = 2) |> 
  step_interact(terms = ~ cat_sexo:cat_ocupacion + cat_sexo:cat_educacion)

lm_fit_7 <- lm_model |> 
  fit(num_salarioHora ~ ., data = dataset_training)

wf_lm_model_1 <- workflow() |> 
  add_recipe(dataset_recipe_more) |> 
  add_model(lm_model) |> 
  last_fit(split = dataset_split) |> 
  collect_metrics()
