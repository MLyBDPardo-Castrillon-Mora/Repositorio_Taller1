# LIBRARIES AND PREPARATION ====================================================

# Limpiamos el espacio de trabajo
rm(list = ls())

# Paquetes
library(pacman)
library(boot)
library(datawizard)
library(AICcmodavg)
library(olsrr)
library(stargazer)
p_load(rvest, tidyverse)


# DATA SCRAPING ================================================================

my_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
my_html <- read_html(my_url)

db <- my_html %>%
  html_nodes(xpath = "//table") %>%
  html_table() %>%
  as.data.frame()
db <- db[,-1]

# Creamos un loop para anadir las demás tablas y consolidar datos en db

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


# LIMPIEZA DE DATOS ============================================================

# Seed
set.seed(10101)

# Eliminar observaciones de menores de edad
temp <- temp[temp$age>18,]
db <- db[db$age>=18,]

# Eliminar observaciones de no-empleados
temp <- temp[temp$ocu==1,]
db <- db[db$ocu==1,]

# Eliminar observaciones sin salario
temp <- temp[temp$y_ingLab_m>0,]
db <- db[db$y_ingLab_m>0,]

# Generar log de los ingresos
temp$log_w = log(temp$y_ingLab_m) 
db$log_w = log(db$y_ingLab_m)


# AGE-WAGE PROFILE =============================================================

# Regresion
lm(log_w~age+I(age^2), data = temp)
reg1 <- lm(log_w~age+I(age^2), data = db)
stargazer(reg1, type = "text", digits = 5)

# Bootstrap

# Funcion de maximizacion
age_max.fn <- function(datos, index){
  X <- datos$age[index]
  Y <- datos$log_w[index]
  reg_aux <- lm(Y~X+I(X^2), data = datos)
  -reg_aux$coefficients[2]/(2*reg_aux$coefficients[3])
}

boot(db, age_max.fn, R = 1000)

# GENDER GAP====================================================================

# Regresion incondicional
reg2 <- lm(log_w~sex, data = db)
stargazer(reg2, type = "text", digits = 5)

# FWL
reg3 <- lm(log_w~sex+age+maxEducLevel+formal+hoursWorkUsual+oficio, data = db)
stargazer(reg3, type = "text", digits = 5)


# PREDICTING EARNINGS===========================================================

# Crear variable de interaccion 'age' and 'sex'.
db$interac_age_sex  <- db$age*db$sex

# Partición de la muestra
part_db <- data_partition(db, proportion = 0.7, seed = 10101)

# Especificaciones
model_1 <- lm(log_w~age+sex, data = part_db$p_0.7)
model_2 <- lm(log_w~age+I(age^2)+sex, data = part_db$p_0.7)
model_3 <- lm(log_w~age+sex+interac_age_sex, data = part_db$p_0.7)
model_4 <- lm(log_w~age+I(age^2)+sex+interac_age_sex, data = part_db$p_0.7)
model_5 <- lm(log_w~age+I(age^2)+I(age^3)+sex+interac_age_sex, data = part_db$p_0.7)

# lista de modelos 
models <- list(model_1, model_2, model_3, model_4, model_5)
aictab(cand.set = models)
bictab(cand.set = models)

# Prediccion
forecast_1 <- predict(model_1, newdata = part_db$test)
forecast_2 <- predict(model_2, newdata = part_db$test)
forecast_3 <- predict(model_3, newdata = part_db$test)
forecast_4 <- predict(model_4, newdata = part_db$test)
forecast_5 <- predict(model_5, newdata = part_db$test)

# MSE
p_aux <- data.frame(
  pred_1 = forecast_1, 
  pred_2 = forecast_2, 
  pred_3 = forecast_3,
  pred_4 = forecast_4,
  pred_5 = forecast_5,
  actual = part_db$test$log_w
)

mean((p_aux$actual - p_aux$pred_1)^2)
mean((p_aux$actual - p_aux$pred_2)^2)
mean((p_aux$actual - p_aux$pred_3)^2)
mean((p_aux$actual - p_aux$pred_4)^2)
mean((p_aux$actual - p_aux$pred_5)^2)

# LOOCV - Modelos 4 y 5

CV4_aux <- rep(0,1000)
for (i in 1:16277) {
  CV4 <- lm(log_w~age+I(age^2)+sex+interac_age_sex, data = db[-i,])
  F_4 <- predict(CV4, newdata = db[i,])
  CV4_aux[i] <- (db$log_w[i] - F_4)
}
CV4_aux <- CV4_aux^2
CV_4 <- 1/16277*sum(CV4_aux)


CV5_aux <- rep(0,1000)
for (i in 1:16277) {
  CV5 <- lm(log_w~age+I(age^2)+I(age^3)+sex+interac_age_sex, data = db[-i,])
  F_5 <- predict(CV5, newdata = db[i,])
  CV5_aux[i] <- (db$log_w[i] - F_5)
}
CV5_aux <- CV5_aux^2
CV_5 <- 1/16277*sum(CV5_aux)


