
# Librerías a utilizar ----------------------------------------------------

library(tidyverse)
library(ggthemes)
library(magrittr)
library(lubridate)
library(paletteer)
library(viridis)

library(TSstudio)
library(forecast)


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




# Calculando el promedio mensual para X, Y y Z

df_mensual_x <- X %>%
  mutate(year_month = floor_date(Date, "month")) %>%
  group_by(year_month) %>%
  summarise(Price = round(mean(Price, na.rm = TRUE),2)) %>%
  ungroup()


df_mensual_y <- Y %>%
  mutate(year_month = floor_date(Date, "month")) %>%
  group_by(year_month) %>%
  summarise(Price = round(mean(Price, na.rm = TRUE),2)) %>%
  ungroup()


df_mensual_Z <- Z %>%
  mutate(year_month = floor_date(Date, "month")) %>%
  group_by(year_month) %>%
  summarise(Price = round(mean(Price, na.rm = TRUE),2)) %>%
  ungroup()



# Volviendo un objeto ts cada serie mensual.

serie_tsx <- ts(df_mensual_x$Price, start = c(1988, 06), end = c(2024, 04), frequency = 12)   

serie_tsy <- ts(df_mensual_y$Price, start = c(2006, 07), end = c(2023, 09), frequency = 12)

serie_tsz <- ts(df_mensual_Z$Price, start = c(2010, 01), end = c(2023, 08), frequency = 12)





# Hallando el mejor modelo arima y calculando los pronósticos para 36 meses -------------


# Para la serie de X
modelo_x <- auto.arima(serie_tsx)
fore_x <- forecast(modelo_x, h = 36)


# Para la serie de Y
modelo_y <- auto.arima(serie_tsy)
fore_y <- forecast(modelo_y, h = 36)

# Para la serie de Z
modelo_z <- auto.arima(serie_tsz)
fore_z <- forecast(modelo_z, h = 36)



# Calculo de los costos esperados para cada equipo con los pronósticos ya obtenidos ------------

# Equipo 1
C1_hat <- 0.2 * X_hat + 0.8 * Y_hat

# Equipo2
C2_hat <- (X_hat + Y_hat + Z_hat) / 3


# guardando los resultados en un dataframe
resultado <- data.frame(
  "Mes" = 1:36,
  "C1" = C1_hat,
  "C2" = C2_hat
)
