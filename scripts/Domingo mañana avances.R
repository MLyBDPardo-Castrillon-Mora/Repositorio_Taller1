# LIBRARIES AND PREPARATION ====================================================



#Initial configuration

## install pacman
if(!require(pacman)) install.packages("pacman") ; require(pacman)
## Loading required package: pacman
## require/install packages on this session

require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       BiocManager,
       skimr, # summary data
       caret,# Classification And REgression Training
       readxl,# paquete pare leer archivos
       stargazer,
       revest, 
       olsrr,
       xml2,
       sys # paquete para acceder a atributos del sistema operativo
       
) 

# las anteriores nos ayudan a hacer una instalacion de los packages, posterior a preguntar al sistema si se requieren o los tiene instalados
# install.packages("rio")
# install.packages("tidyverse") #paquete para filtrar 
# install.packages("skimr") 
#i nstall.packages("caret") 

#Cargar paquetes para uso

library(rio)
library(tidyverse) #paquete para filtrar 
library(rvest)
library(skimr) 
library(caret) 
library(readxl)
library(AICcmodavg)
library(datawizard)
library(sys)
library(stargazer)
library(xml2)
library(pacman)
library(boot)
library(olsrr)
library(ggplot2)

#library(ggplot)

sys.status()
getwd() # trae el directorio de trabajo 

getwd() # trae el directorio de trabajo 
install_formats() #This function installs various ‘Suggests’ dependencies for rio that expand its support to the full range of support import and export formats.

# LIBRARIES AND PREPARATION ====================================================

rm(list = ls()) #Limpieza espacio trabajo


# DATA SCRAPING ================================================================

my_url <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
my_html <- read_html(my_url)

 
db <- my_html %>% 
  html_nodes(xpath = "//table") %>% #Buscamos un elemento, los datos, teniendo en cuenta la estructura jerárquica del XML.
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

#Conservamos las variables relevantes
db %>% select(age,ocu,y_ingLab_m,sex,maxEducLevel,formal,hoursWorkUsual,oficio)

# Eliminar observaciones de menores de edad
db <- db[db$age>=18,]

# Eliminar observaciones de no-empleados
db <- db[db$ocu==1,]

# Eliminar observaciones sin salario
db <- db[db$y_ingLab_m>0,]
db <- db[!is.na(db$age),]

# Generar log de los ingresos
db$log_w = log(db$y_ingLab_m)

# AGE-WAGE PROFILE =============================================================

# Regresion
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

# Bootstrap para edad que maximiza ingresos
b_age_wage <-boot(db, age_max.fn, R = 1000)
print(b_age_wage)

# Prediccion
forecast_w <- predict(reg1, data = db$log_w)

#Grafica age-earnings 

ggplot(data=db, mapping=aes(x=age, y=forecast_w))+geom_point(col='#6E8B3D')+xlab("Edad")+ylab("Salario")+ggtitle("Perfil Estimado Edad vs Salario")+theme_bw()

#Bootstrap intervalos de confianza 
intervaloMax <- b_age_wage$t0+(0.4383546*1.96)
intervaloMin <- b_age_wage$t0-(0.4383546*1.96)
print(paste ("Min:", intervaloMin,", Max:", intervaloMax))


# GENDER GAP====================================================================
#Hacemos un summary para revisar los controles que se integraran a la regresion
summary(db[,c('sex','age','maxEducLevel','formal','hoursWorkUsual','oficio')])

#Creamos una nueva base de datos sin missing values con 9891 observaciones
db <- db[!is.na(db$maxEducLevel),]
# Regresión incondicional

reg2 <- lm(log_w~sex, data = db)
stargazer(reg2, type = "text", digits = 5)

# FWL___________________________________________________________________________
# Paso 0: Regresion original

reg3 <- lm(log_w~sex+age+maxEducLevel+formal+hoursWorkUsual+oficio, data = db)
stargazer(reg3, type = "text", digits = 5)

# Paso 1: Residuos de 'sex' en 'controles'
sex_resid_c = lm(sex~age+maxEducLevel+formal+hoursWorkUsual+oficio, db)$residuals

# Paso 2: Residuos de 'log_w' en 'controles' 
wage_resid_c = lm(log_w~age+maxEducLevel+formal+hoursWorkUsual+oficio, db)$residuals

# Paso 3: Regresion de residuos
reg_fwl <- lm(wage_resid_c~sex_resid_c, db)
stargazer(reg3, reg_fwl, type = "text", digits = 5)

db <- db %>% mutate(sexast=mean(db$sex)+sex_resid_c,
                             log_wage_ast=mean(db$log_wage)+wage_resid_c,db)
# FWL - Bootstrap_______________________________________________________________
# Datos auxiliares
resid <- cbind.data.frame(sex_resid_c, wage_resid_c)

# Funcion de coeficiente
fwl_coef.fn <- function(datos, index){
  X <- datos$sex_resid_c[index]
  Y <- datos$wage_resid_c[index]
  reg_aux <- lm(Y~X, data = datos)
  reg_aux$coefficients[2]
}

# Bootstrap para GAP condicionado
b_age_wage_sex <- boot(resid, fwl_coef.fn, R = 1000)
print(b_age_wage_sex)

# Prediccion

forecast_wr <- predict(reg_fwl,data =db$log_w)

#Grafica age-earnings 

ggplot(data=db, mapping=aes(x=age, y=forecast_wr ))+geom_point(col='#6E8B3D')+xlab("Edad")+ylab("Salario")+ggtitle("Perfil Estimado Edad vs Salario")+theme_bw()

#ggplot(data=db, mapping=aes(x=age, y=sex_resid_c))+geom_point(col='lightskyblue2')+xlab("Edad")+ylab("Logaritmo Salario")+ggtitle("Perfil Estimado Edad vs Salario")+theme_bw()


#Intervalo de confianza 

intervaloMax <- b_age_wage_sex$t0+(0.01245347*1.96)
intervaloMin <- b_age_wage_sex$t0-(0.01245347*1.96)
print(paste ("Min:", intervaloMin,", Max:", intervaloMax))

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
model_6 <- lm(log_w~age+I(age^2), data = part_db$p_0.7)
model_7 <- lm(log_w~sex, data = part_db$p_0.7)
model_8 <- lm(log_w~age+maxEducLevel+formal+hoursWorkUsual+oficio, data = part_db$p_0.7)

# lista de modelos 
models <- list(model_1, model_2, model_3, model_4, model_5, model_6, model_7, model_8)
aictab(cand.set = models)
bictab(cand.set = models)

# Prediccion
forecast_1 <- predict(model_1, newdata = part_db$test)
forecast_2 <- predict(model_2, newdata = part_db$test)
forecast_3 <- predict(model_3, newdata = part_db$test)
forecast_4 <- predict(model_4, newdata = part_db$test)
forecast_5 <- predict(model_5, newdata = part_db$test)
forecast_6 <- predict(model_6, newdata = part_db$test)
forecast_7 <- predict(model_7, newdata = part_db$test)
forecast_8 <- predict(model_8, newdata = part_db$test)

# MSE
p_aux <- data.frame(
  pred_1 = forecast_1, 
  pred_2 = forecast_2, 
  pred_3 = forecast_3,
  pred_4 = forecast_4,
  pred_5 = forecast_5,
  pred_6 = forecast_6,
  pred_7 = forecast_7,
  pred_8 = forecast_8,
  actual = part_db$test$log_w
)

mean((p_aux$actual - p_aux$pred_1)^2)
mean((p_aux$actual - p_aux$pred_2)^2)
mean((p_aux$actual - p_aux$pred_3)^2)
mean((p_aux$actual - p_aux$pred_4)^2)
mean((p_aux$actual - p_aux$pred_5)^2)
mean((p_aux$actual - p_aux$pred_6)^2)
mean((p_aux$actual - p_aux$pred_7)^2)
mean((p_aux$actual - p_aux$pred_8)^2)

# LOOCV - Modelos 4 y 5

CV8_aux <- rep(0,9892)
for (i in 1:9892) {
  CV8 <- lm(log_w~age+maxEducLevel+formal+hoursWorkUsual+oficio, data = db[-i,])
  F_8 <- predict(CV8, newdata = db[i,])
  CV8_aux[i] <- (db$log_w[i] - F_8)
}
CV8_aux <- CV8_aux^2
CV_8 <- 1/9891*sum(CV8_aux, na.rm=TRUE)


CV5_aux <- rep(0,9892)
for (i in 1:9892) {
  CV5 <- lm(log_w~age+I(age^2)+I(age^3)+sex+interac_age_sex, data = db[-i,])
  F_5 <- predict(CV5, newdata = db[i,])
  CV5_aux[i] <- (db$log_w[i] - F_5)
}
CV5_aux <- CV5_aux^2
CV_5 <- 1/16277*sum(CV5_aux)
CV_5 <- 1/9892*sum(CV5_aux)

# FILES TO VIEWS FOLDER===========================================================

#Estadisticas descriptivas
stargazer(db[c("age","sex","formal","hoursWorkUsual","y_ingLab_m")], digits=1,
          covariate.labels = c("Edad","Sexo","1 si es formal; 0 si no",
                               "Horas de trabajo Semanales","Ingreso Laboral Mensual"),
          summary.stat = c("n","mean","sd","min","p25","median","p75","max"),
          type = "latex", title = "Estadisticas Descriptivas",flip = TRUE, out = "./views/est_desc.tex")

#Regresión Age-Wage Profile
stargazer(reg1, type = "latex", digits = 3, title = "Perfil Edad-Salario",
          covariate.labels = c("Edad", "Edad Cuadrado", "Constante"),
          dep.var.labels = ("Logaritmo del Salario"),
          dep.var.caption = "Variable respuesta",
          keep.stat = (c("n","rsq","f")), out="./views/reg1.tex")

#Regresión Gender Gap
stargazer(reg2,)