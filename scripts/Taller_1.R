#Limpiamos el espacio de trabajo
rm(list = ls())

if(!require(pacman)) install.packages("pacman") ; require(pacman)
## Loading required package: pacman
## require/install packages on this session

require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret,# Classification And REgression Training
       readxl,# paquete pare leer archivos
       stargazer,
       boot,
       rvest,
       sys # paquete para acceder a atributos del sistema operativo
)

#Cargar paquetes para uso

library(rio)
library(tidyverse) #paquete para filtrar 
library(skimr) 
library(caret) 
library(readxl)
library(sys)
library(stargazer)
library(boot)
#library(ggplot)

p_load(rvest, tidyverse)

#Obtenemos los datos

# Sugerencia traerla de esa forma, como lo explico Lucas... Xpath es para traer una parte de la pagina web, pero aqui esta como tabla es más practico traerlo así.
my_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
my_html <- read_html(my_url)
db<-tablas <- my_html %>%
  html_table()%>%
  as.data.frame()


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


## LIMPIEZA DE DATOS

#Seed
set.seed(10101)

#Eliminar observaciones de menores de edad
temp <- temp[temp$age>18,]
db <- db[db$age>=18,]

#Eliminar observaciones de no-empleados
temp <- temp[temp$ocu==1,]
db <- db[db$ocu==1,]

#Eliminar observaciones sin salario
temp <- temp[temp$ingtot>0,]
db <- db[db$ingtot>0,]

## REGRESION: Salario vs Age

# Generar log de los ingresos
temp$log_w = log(temp$ingtot) 
db$log_w = log(db$ingtot)

# Regresion
lm(log_w ~ age+I(age^2), data=temp)
reg1 <- lm(log_w ~ age+I(age^2), data=db)

# Bootstrap

# Funcion de maximizacion
age_max.fn <- function(datos, index){
  X <- datos$age[index]
  Y <- datos$log_w[index]
  reg_aux <- lm(Y ~ X+I(X^2), data=datos)
  -reg_aux$coefficients[2]/(2*reg_aux$coefficients[3])
}

boot(db, age_max.fn, R=1000)
