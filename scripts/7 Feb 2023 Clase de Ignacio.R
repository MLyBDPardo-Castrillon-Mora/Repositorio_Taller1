
#Initial configuration

## install pacman
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
library(skimr) 
library(caret) 
library(readxl)
library(sys)
library(stargazer)
#library(ggplot)

sys.status()
getwd() # me trae el directorio de trabajo 
setwd("C:/Users/E0993099/OneDrive - Ecopetrol S.A/Cesar.Pardo/02 -- Laura/00 -- Uniandes/00 -- Semestre 7") #fija el directorio de trabajo, tener en cuenta que hay cambiar los \ por /
getwd() # me trae el directorio de trabajo 
install_formats() #This function installs various ‘Suggests’ dependencies for rio that expand its support to the full range of support import and export formats.

## load data
precio_de_accion <- read_excel("Consola vs precio de accion.xlsx")
View(precio_de_accion)
ggplot(data = precio_de_accion, mapping = aes(x = TipoConsola , y = priceConsola ) , color=as.factor(formal)) +
  geom_point()

db <- import("https://github.com/ignaciomsarmiento/datasets/blob/main/GEIH_sample1.Rds?raw=true")
db
head()
glimpse(db)
skim(db)


# Notas del video de limpieza de datos con tidyverse

# link de github del profe
# https://github.com/ignaciomsarmiento/test_repo
#sat <- read.csv("https://github.com/ignaciomsarmiento/datasets/blob/main/2012_SAT_Results.csv")
sat <- read.csv("2012_SAT_Results.csv")

#demog <- read.csv("https://github.com/ignaciomsarmiento/datasets/blob/main/demog.csv")
demog <- read.csv("demog.csv")%>% view()  #view me permite verlo en la ventana

str(sat)
str(demog)

#demog_subset <- demog %>% select (DBN, Name,schoolyear,asian_per,black_per,hispanic_per,white_per, starts_with(("grade")))
#rm(demog_subset)



#Ejeplo minimo reproducible

df <- data.frame(x = c(1, 2, 3, 4, 5),
                 y = c(2, 3, 1, 5, 4))
df
dput(head(iris,4)) # cuatro primeras filas de la base iris que viene con R base

require(ggplot2)
ggplot(data = df,  mapping = aes(x=x, y=y)) + geom_line()

sessionInfo()


#  31 de Enero de 2023

# 1.2 Case 1. predecir el precio del gas. vs cantidad

#error para preguntar, porque no se puede llamar desde este link
#gas<- read.csv("https://github.com/ignaciomsarmiento/datasets/blob/main/gas.csv")

gas<- read.csv(url("https://raw.githubusercontent.com/ignaciomsarmiento/datasets/main/gas.csv"))
#pueden aparecer precios en negativo, porque se muestra es el logaritmo
head(gas)

#n(q,p)= Delta Ln Quantity / Delta Ln Price = Beta_1, aquie la elasticidad es Beta_1

mod1<-lm(consumption~price+income,gas)
stargazer (mod1,type="text")

# Ya con la elasticidad, podemos sacar los coeficientes y luego la varianza

# Como quiero hace run muestreo, creo una semilla, y estoy sacando numeros aleatorios
set.seed(112)
R<-1000 # numeo de repeticiones
eta_mod1<-rep(0,R) #aqui voy a guardar la elasticidad, los Betas1 gorro

for (i in 1:R){
  db_sample <- sample_frac(gas,size=1,replace=TRUE) # el size 1, representa el 100% de la muestrsa, si se quiere un 75%, se pone 0.75
  f<-lm(consumption~price+income,db_sample)
  coefs<-f$coefficients
  eta_mod1[i]<-coefs[2] #el coefs[1] es la constante, el coefs[2] es el price
}
plot(hist(eta_mod1))
#por el teorema fundalmental "
head(eta_mod1)
mean(eta_mod1)
quantile(eta_mod1) 
varianza <-var(eta_mod1) #se calcula la varianza
head(varianza)
sqrt(varianza) # se calcula desviacion standar. clave la explicacion
#por homocedasticidad (constante) y heterocedasticidad (no es constante)
#0.025 relamente es la varianza de B gorro = 1/n (sigma^2/(1-Rj^2)V(x))
# las estrellas salen de calcular salen de hacer un t de hipotesis el t=B^/Se(B), para ser significativo tiene que ser mas grande de 1,96, muchos usan 1 , Se es error estandar

# ahora podemos hacer esto mediante paquetes o mas corto, ahora vamos a usar el paquete boot

p_load("boot")
eta.fn<-function(data,index){     # aqui el index, es donde saca la muestra, es mas eficiente en temas computacionales
  coef(lm(consumption~price+income, data = data,subset=index))  # puede poner el [2] solo para precios
}
boot(gas,eta.fn,R=1000)

# este boot, es mas eficiente en coplejidad algoritmica y mucho mas eficiente

# Caso 2  Aqui empieza

# bootstrap tecnica de remuestro para caracteriza la incertidumbre

#n(q,p)= Delta Ln Quantity / Delta Ln Price = Beta_1, aquie la elasticidad es Beta_1

head (gas)

# creo con mutate las variables faltantes, price2 y price_income
gas <-  gas %>% mutate(price2=price^2,price_income=price*income)
head (gas)

# formula de estimacion

mod2<- lm(consumption~price+price2+income+price_income,gas)
stargazer(mod1,mod2,type="text")

coefs<-mod2$coef
coefs

b0= coefs[1]
b1= coefs[2]
b2= coefs[3]
b3= coefs[4]
b4= coefs[5]

# ahora necesitamos calcualr la elasticidad en algun punto, para este ejemplo, lo vamos a calcular en la media 

price_bar <- mean(gas$price)
income_bar <- mean(gas$income)

elastpt = b1+b2*2*price_bar+b4*income_bar
elastpt

#     price -0.67177, cuando el precio aumenta en 1%, la cantidad demanda baja en un -0.67%, demanda tiene pendiente negativa

eta_mod2_fn<-function(data,index,
                      price_bar=mean(gas$price),
                      income_bar=mean(gas$income)){
  
  #get the coefficients
  coefs<-lm(consumption~price+price2+income+price_income,data, subset = index)$coefficients
  
  #put the coefficients in scalars  
  b1<-coefs[2]
  b2<-coefs[3] 
  b4<-coefs[5] 
  
  #calculate the elasticity of demand
  elastpt<-b1+2*b2*price_bar+b4*income_bar
  
  #return the elasticty of demand
  return(elastpt)
}

eta_mod2_fn(gas,1:nrow(gas)) 

# yo la habia calculado... en la media, ahora en otro punto

eta_mod2_fn(gas,1:nrow(gas),price_bar=-1,income_bar=2)  

results <- boot(gas, eta_mod2_fn,R=1000)
results


# armar las formulas, tener en cuenta las covarianzas, a través el metodo delta que es el que se utuliza
# mejor usar el Bootstrap, me intesa es la elacticidad.
# La incertidumbre del parametro, tengo que combinar tres parametros





# Fin de caso 2





#Predicting the number of rented bikes

p_load(tidyverse,fixest, stargazer,knitr,kableExtra,jtools,ggstance,broom,broom.mixed,skimr)


# Import dataset
load(url("https://github.com/ignaciomsarmiento/datasets/blob/main/bike.RData?raw=true"))
str(bike)

