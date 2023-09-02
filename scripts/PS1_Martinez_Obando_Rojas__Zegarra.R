###############################################################
# Problem set 1 
# Autores: Martinez, K., Rojas, J., Obando, J.C. & Zegarra, D.
# Materia: Big Data and Machine Learning para Economía Aplicada
# Año: 2023
###############################################################
# Fijo directorio e instalo paquetes


local_dav <- "D:/github/"
setwd(paste(local_dav,"/PS_1_ML/scripts",sep=""))
getwd()

#**********************************************************
install.packages("rvest")
library(pacman)
library(rvest)

#-------------------------------------------------------------
# 1. Data
rm(list=ls())

link <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")
link

df <- data.frame()
for (url in link) {
  print(url)
  table0 <- read_html(url) %>%  html_table() 
  table0 <- as.data.frame(table0[[1]])
  df <- rbind(df, table0)
}

