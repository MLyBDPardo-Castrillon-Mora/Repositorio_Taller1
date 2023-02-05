#Limpiamos el espacio de trabajo
rm(list = ls())

#Requerimos los paquetes que vamos a suar
library(pacman)
p_load(rvest, tidyverse)

#Obtenemos los datos

my_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
my_html <- read_html(my_url)

db <- my_html %>%
  html_nodes(xpath = "//table") %>%
  html_table() %>%
  as.data.frame()
db <- db[,-1]

#Creamos un loop para añadir las demás tablas y consolidar datos en db

for (i in 2:10){
  x = as.character(i)
  my_url <- paste("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",x,".html",sep="")
  my_html <- read_html(my_url)
  temp <- my_html %>%
    html_nodes(xpath = "//table") %>%
    html_table() %>%
    as.data.frame()
  temp <- temp[,-1]
  db <- bind_rows(db, temp)
}