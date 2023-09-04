# 1| Web scrapping --------------------------------------------------------
# De la página de Ignacio se extrae las diez tablas y se concatenan por filas. 
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

write.csv(x = dataset, file = paste0(directorioDatos, 'dataframe.csv'))
