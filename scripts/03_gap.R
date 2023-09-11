#packages
require("pacman")
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
