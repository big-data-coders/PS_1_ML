# Predicting income -------------------------------------------------------
# Autores: Martinez, K., Rojas, J., Obando, J.C. & Zegarra, D.
# Materia: Big Data and Machine Learning para Economía Aplicada
# Año: 2023

# 1| Preparacion ----------------------------------------------------------
rm(list = ls())
graphics.off()
set.seed(123)                       # Replicabilidad en las simulaciones.

# 1.1| Librerias ----------------------------------------------------------
librerias    <- c('here', 'tidyverse', 'rvest')
noInstaladas <- librerias[!(librerias %in% rownames(installed.packages()))]

if(length(noInstaladas)){
  install.packages(noInstaladas)
}

invisible(sapply(librerias, library, character.only = TRUE, quietly = TRUE))

# 1.2| Directorio ---------------------------------------------------------
directorioPrincipal  = enc2native(here())
directorioCodigo     = paste0(directorioPrincipal, '/scripts/')
directorioDatos      = paste0(directorioPrincipal, '/stores/')
directorioResultados = paste0(directorioPrincipal, '/views/')
setwd(directorioPrincipal)

# 1.3| Funciones ----------------------------------------------------------


# 2| Resultados -----------------------------------------------------------
# Al definir 'primeraVez := TRUE', se realiza el análisis desde ceros. Esto
# incluye descargar, nuevamente, la base de datos desde la página de Ignacio.
# Dado que el proceso es demorado se recomienda definir el parámetro en FALSE.
primeraVez <- FALSE
source(paste0(directorioCodigo, '01_data.R'), encoding = 'UTF-8')
