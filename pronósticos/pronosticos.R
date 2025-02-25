#ESTIMACIONES A 5 AÑOS

#librerias
library(tidyverse)

###########################################
#creación de series temporales
###########################################

#CREACIÓN DE TABLA PIB ANUAL
archivo = file.choose()
data = read.csv(archivo)

#filtramos PIB por datos anuales
pib_anual = data %>%
  filter(periodo == "Anual")

#filtramos PIB por datos trimestrales
pib_trimestral = data %>%
  filter(periodo %in% c("T1","T2","T3","T4"))



#CREACIÓN DE TABLA TASA INTERES
#Extraemos el archivo
archivo = file.choose()
data = read.csv(archivo)

#creamos variable "trimestre" para identificar la serie
#temporal por trimestre y empatar con PIB de ser necesario
tasa_interes = data %>%
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y")) %>%  # Especificar formato correcto
  arrange(fecha) %>%  # Ordenar correctamente
  mutate(
    mes = month(fecha),
    año = year(fecha),
    trimestre = ifelse(
      row_number() == 1 |  # La primera observación siempre es 1
        (mes %in% c(1, 4, 7, 10) & (lag(mes, default = 0) != mes | lag(año, default = 0) != año)), 
      1, 
      0
    ),
    log_tasa = log(tasa_objetivo) #logaritmo de la tasa en caso de necesitarse
  ) %>%
  select(-mes, -año)



#CREANDO LA TABLA DE TIPO DE CAMBIO
archivo = file.choose()
data = read.csv(archivo)

tipo_cambio = data%>%
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y")) %>%  # Especificar formato correcto
  arrange(fecha) %>%  # Ordenar correctamente
  mutate(
    mes = month(fecha),
    año = year(fecha),
    trimestre = ifelse(
      row_number() == 1 |  # La primera observación siempre es 1
        (mes %in% c(1, 4, 7, 10) & (lag(mes, default = 0) != mes | lag(año, default = 0) != año)), 
      1, 
      0
    )
    ) %>%
  select(-mes, -año)






