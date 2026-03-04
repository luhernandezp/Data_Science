
# Librerías a utilizar ----------------------------------------------------


library(tidyverse)
library(magrittr)




# Cargue de los datos -----------------------------------------------------

X <- read.csv("Datos/X.csv")
Y <- read.csv("Datos/Y.csv", sep = ";")
Z <- read.csv("Datos/Z.csv")



# Adecuación del formato fecha (Año-Mes-Día) de la variable Date
X$Date %<>% ymd()
Y$Date %<>% dmy()
Z$Date %<>% ymd()



# Organización de las fechas (orden ascendente)

X <- X %>% arrange(Date)
Y <- Y %>% arrange(Date)
Z <- Z %>% arrange(Date) 



test_forecast(actual = serie_tsx, forecast.obj = fore1x,test = test_x)


















