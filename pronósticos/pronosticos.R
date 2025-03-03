#ESTIMACIONES A 5 AÑOS

setwd("C:/SEM 8/Economía del Riesgo/pronósticos/csv")

#librerias
library(tidyverse)
library(fuzzyjoin)
library(tidymodels)
library(forecast)
library(ggplot2)
library(scales)

###########################################
#creación de series temporales
###########################################

#CREACIÓN DE TABLA PIB ANUAL
data = read.csv("pib.csv", stringsAsFactors = FALSE)

#filtramos PIB por datos anuales
pib_anual = data %>%
  filter(periodo == "Anual")

#filtramos PIB por datos trimestrales
pib_trimestral <- data %>%
  filter(periodo %in% c("T1", "T2", "T3", "T4")) %>% 
  mutate(
    Q1 = ifelse(periodo == "T1", 1, 0),
    Q2 = ifelse(periodo == "T2", 1, 0),
    Q3 = ifelse(periodo == "T3", 1, 0),
    Q4 = ifelse(periodo == "T4", 1, 0),
    trimestre = ifelse(Q1 == 1 | Q2 == 1 | Q3 == 1 | Q4 == 1,1,0)
  )%>%
  mutate(
    mes = case_when(
      periodo == "T1" ~ "01",
      periodo == "T2" ~ "04",
      periodo == "T3" ~ "07",
      periodo == "T4" ~ "10"
    ),
    fecha = as.Date(paste("01", mes, año, sep = "/"), format = "%d/%m/%Y")
  ) %>%
  select(-mes)

###############################
#CREACIÓN DE TABLA TASA INTERES
#Extraemos el archivo
data = read.csv("tasa_interes.csv", stringsAsFactors = FALSE)


#creamos variable "trimestre" para identificar la serie temporal
# por trimestre y empatar con PIB de ser necesario
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

######################################
#CREANDO LA TABLA DE TIPO DE CAMBIO
data = read.csv("tipo_cambio.csv", stringsAsFactors = FALSE)


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

#################################
#CREANDO TABLA DE INFLACION
data = read.csv("inflacion.csv", stringsAsFactors = FALSE)


inflacion = data %>% 
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


###########################
#CREANDO TABLA DE DESEMPLEO
data = read.csv("desempleo.csv", stringsAsFactors = FALSE)

desempleo <- data %>%
  mutate(
    fecha = as.Date(fecha, format = "%Y-%m-%d"),  # Convertir correctamente
    fecha = format(fecha, "%d/%m/%Y")  # Cambiar el formato a "DD/MM/YYYY"
  ) %>%
  arrange(fecha)


#########################
#CREANDO UNA TABLA GENERAL

# Convertir todas las fechas a formato Date
pib_trimestral <- pib_trimestral %>% mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"))
inflacion <- inflacion %>% mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"))
tasa_interes <- tasa_interes %>% mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"))
tipo_cambio <- tipo_cambio %>% mutate(fecha = as.Date(fecha, format = "%d/%m/%Y"))

# Eliminar la columna "trimestre" de las series macroeconómicas
inflacion <- inflacion %>% select(-trimestre)
tasa_interes <- tasa_interes %>% select(-trimestre)
tipo_cambio <- tipo_cambio %>% select(-trimestre)

# Hacer joins asegurando que fecha siempre está presente y eliminando duplicados
general <- pib_trimestral %>%
  difference_left_join(inflacion, by = "fecha", max_dist = 5, distance_col = "diff_inflacion") %>%
  mutate(fecha = coalesce(fecha.x, fecha.y)) %>%  # Restaurar fecha si se fragmentó en .x y .y
  select(-fecha.x, -fecha.y, -diff_inflacion) %>%  # Limpiar columnas extra
  group_by(fecha) %>%
  slice_min(abs(as.numeric(difftime(fecha, fecha, units = "days"))), with_ties = FALSE) %>%
  ungroup() %>%
  
  difference_left_join(tasa_interes, by = "fecha", max_dist = 5, distance_col = "diff_tasa") %>%
  mutate(fecha = coalesce(fecha.x, fecha.y)) %>%
  select(-fecha.x, -fecha.y, -diff_tasa) %>%
  group_by(fecha) %>%
  slice_min(abs(as.numeric(difftime(fecha, fecha, units = "days"))), with_ties = FALSE) %>%
  ungroup() %>%
  
  difference_left_join(tipo_cambio, by = "fecha", max_dist = 5, distance_col = "diff_tc") %>%
  mutate(fecha = coalesce(fecha.x, fecha.y)) %>%
  select(-fecha.x, -fecha.y, -diff_tc) %>%
  group_by(fecha) %>%
  slice_min(abs(as.numeric(difftime(fecha, fecha, units = "days"))), with_ties = FALSE) %>%
  ungroup() %>% 
  
  mutate(
    pib_lag1 = lag(pib, 1),
    pib_lag2 = lag(pib, 2),
    pib_lag3 = lag(pib, 3),
    pib_lag4 = lag(pib, 4),
    pib_lag5 = lag(pib, 5),
    pib_lag6 = lag(pib, 6),
    pib_lag7 = lag(pib, 7),
    pib_lag8 = lag(pib, 8),
    pib_lag9 = lag(pib, 9),
    pib_lag10 = lag(pib, 10),
    pib_lag11 = lag(pib, 11),
    pib_lag12 = lag(pib, 12),
    tasa_cambio_inflacion = log(variacion_anual),
    across(where(is.character), ~ as.numeric(gsub("[^0-9.-]", "", .)))
    )






general <- pib_trimestral %>%
  difference_left_join(inflacion, by = "fecha", max_dist = 5, distance_col = "diff_inflacion") %>%
  mutate(fecha = coalesce(fecha.x, fecha.y)) %>%
  select(-fecha.x, -fecha.y, -diff_inflacion) %>%
  group_by(fecha) %>%
  slice_min(abs(as.numeric(difftime(fecha, fecha, units = "days"))), with_ties = FALSE) %>%
  ungroup() %>%

  difference_left_join(tasa_interes, by = "fecha", max_dist = 5, distance_col = "diff_tasa") %>%
  mutate(fecha = coalesce(fecha.x, fecha.y)) %>%
  select(-fecha.x, -fecha.y, -diff_tasa) %>%
  group_by(fecha) %>%
  slice_min(abs(as.numeric(difftime(fecha, fecha, units = "days"))), with_ties = FALSE) %>%
  ungroup() %>%

  difference_left_join(tipo_cambio, by = "fecha", max_dist = 5, distance_col = "diff_tc") %>%
  mutate(fecha = coalesce(fecha.x, fecha.y)) %>%
  select(-fecha.x, -fecha.y, -diff_tc) %>%
  group_by(fecha) %>%
  slice_min(abs(as.numeric(difftime(fecha, fecha, units = "days"))), with_ties = FALSE) %>%
  ungroup() %>%

  mutate(
    pib_lag1 = lag(pib, 1),
    pib_lag2 = lag(pib, 2),
    pib_lag3 = lag(pib, 3),
    pib_lag4 = lag(pib, 4),
    pib_lag5 = lag(pib, 5),
    pib_lag6 = lag(pib, 6),
    pib_lag7 = lag(pib, 7),
    pib_lag8 = lag(pib, 8),
    pib_lag9 = lag(pib, 9),
    pib_lag10 = lag(pib, 10),
    pib_lag11 = lag(pib, 11),
    pib_lag12 = lag(pib, 12),
    
    # Asegurar que variacion_anual es numérica antes de aplicar log()
    variacion_anual = as.numeric(gsub("[^0-9.-]", "", variacion_anual)),
    tasa_cambio_inflacion = (variacion_anual - lag(variacion_anual)) / lag(variacion_anual),
    
    # Convertir caracteres a numérico si quedan otras columnas con texto
    across(where(is.character), ~ as.numeric(gsub("[^0-9.-]", "", .)))
  )




################
################

#PRONOSTICOS

################
################

# Convertir la fecha al formato correcto y ordenar
desempleo <- desempleo %>% 
  mutate(fecha = as.Date(fecha, format = "%d/%m/%Y")) %>%
  arrange(fecha)

# Crear la serie de tiempo con frecuencia trimestral
ts_desempleo <- ts(desempleo$total_desempleo, start = c(2005,1), frequency = 4)

# Ajustar el modelo ARIMA
modelo <- auto.arima(ts_desempleo)

# Generar pronóstico a 5 años (20 trimestres)
pronostico <- forecast(modelo, h = 20)

# Crear data frame de la serie original con fechas reales
df_original <- data.frame(
  fecha = seq(from = min(desempleo$fecha), by = "quarter", length.out = length(ts_desempleo)),
  valor = as.numeric(ts_desempleo)
)

# Crear data frame del pronóstico asegurando 20 observaciones completas
df_pronostico <- data.frame(
  fecha = seq(from = max(df_original$fecha) + 90, by = "quarter", length.out = 20),  # Cada trimestre = 90 días aprox.
  media = pronostico$mean,
  lower_80 = pronostico$lower[,1],
  upper_80 = pronostico$upper[,1],
  lower_95 = pronostico$lower[,2],
  upper_95 = pronostico$upper[,2]
)

# Asegurar que todas las columnas tienen 20 valores
df_pronostico <- df_pronostico %>%
  mutate(
    lower_80 = ifelse(is.na(lower_80), tail(lower_80, 1), lower_80),
    upper_80 = ifelse(is.na(upper_80), tail(upper_80, 1), upper_80),
    lower_95 = ifelse(is.na(lower_95), tail(lower_95, 1), lower_95),
    upper_95 = ifelse(is.na(upper_95), tail(upper_95, 1), upper_95)
  )

# Definir los años para mostrar en el eje X cada 5 años
años_marcados <- seq(from = as.Date(paste(format(min(df_original$fecha), "%Y"), "-01-01", sep="")), 
                     to = as.Date(paste(format(max(df_pronostico$fecha), "%Y"), "-01-01", sep="")), 
                     by = "5 years")

# Crear la gráfica con formato corregido
ggplot() +
  # Línea de la serie original
  geom_line(data = df_original, aes(x = fecha, y = valor), color = "black", size = 1) +
  
  # Área sombreada para el intervalo de confianza del 95%
  geom_ribbon(data = df_pronostico, aes(x = fecha, ymin = lower_95, ymax = upper_95), fill = "gray80", alpha = 0.5) +
  
  # Área sombreada para el intervalo de confianza del 80%
  geom_ribbon(data = df_pronostico, aes(x = fecha, ymin = lower_80, ymax = upper_80), fill = "gray60", alpha = 0.5) +
  
  # Línea de la predicción media
  geom_line(data = df_pronostico, aes(x = fecha, y = media), color = "gray30", size = 1, linetype = "dashed") +
  
  # Líneas verticales para cada año
  geom_vline(xintercept = seq(from = min(df_original$fecha), to = max(df_pronostico$fecha), by = "1 year"), 
             linetype = "dotted", color = "gray70") +
  
  # Etiquetas del eje X cada 5 años
  scale_x_date(breaks = años_marcados, date_labels = "%Y") +
  
  # Formato del eje Y en millones con límite mínimo en 0
  scale_y_continuous(labels = label_number(scale = 1e-6, accuracy = 0.1, suffix = "M"), limits = c(0, NA)) +
  
  # Etiquetas y formato
  ggtitle("Desempleo a 5 años") +
  ylab("Total de Desempleo") +
  xlab("Año") +
  theme_minimal()  # Fondo blanco




# Lista de variables a analizar
variables <- c("pib", "variacion_anual", "tasa_cambio_inflacion", "log_tasa", "pesos_x_dolar", "tasa_objetivo")

# Diccionario de nombres para etiquetas en los gráficos
nombres_variables <- list(
  "pib" = "PIB",
  "variacion_anual" = "Inflación",
  "log_tasa" = "Logaritmo de Tasa de Interés",
  "pesos_x_dolar" = "MX/USD",
  "tasa_objetivo" = "Tasa de Referencia",
  "tasa_cambio_inflacion" = "Cambio en inflación"
)

# Función para hacer el pronóstico y graficar
pronosticar_variable <- function(var_name, general) {
  # Eliminar filas donde haya NA en la variable seleccionada
  datos_filtrados <- general[!is.na(general[[var_name]]), ]
  
  # Convertir en serie de tiempo
  ts_var <- ts(datos_filtrados[[var_name]], start = c(min(datos_filtrados$año), min(datos_filtrados$periodo)), frequency = 4) 
  
  # Aplicar media móvil con ventana de 6 trimestres
  var_ma <- ma(ts_var, order = 6, centre = TRUE)
  
  # Eliminar los valores NA de la media móvil
  var_ma_clean <- na.omit(var_ma)
  
  # Ajustar modelo ARIMA
  modelo <- auto.arima(var_ma_clean)
  pronostico <- forecast(modelo, h = 20)
  
  # Crear dataframe de pronóstico
  df_pronostico <- data.frame(
    fecha = seq(from = end(ts_var)[1] + end(ts_var)[2] / 4, length.out = 20, by = 0.25),
    media = pronostico$mean,
    lower_80 = pronostico$lower[,1],
    upper_80 = pronostico$upper[,1],
    lower_95 = pronostico$lower[,2],
    upper_95 = pronostico$upper[,2]
  )
  
  # Convertir serie de tiempo original a dataframe para ggplot
  df_original <- data.frame(fecha = time(ts_var), valor = as.numeric(ts_var))
  
  # Definir años para mostrar cada 5 años
  años_marcados <- seq(floor(min(df_original$fecha)), ceiling(max(df_pronostico$fecha)), by = 5)
  
  # Obtener el nombre descriptivo de la variable
  nombre_grafico <- nombres_variables[[var_name]]
  
  # Crear el gráfico en escala de grises
  p <- ggplot() +
    # Línea de la serie original
    geom_line(data = df_original, aes(x = fecha, y = valor), color = "black", size = 1) +
    
    # Área sombreada para el intervalo de confianza del 95%
    geom_ribbon(data = df_pronostico, aes(x = fecha, ymin = lower_95, ymax = upper_95), fill = "gray80", alpha = 0.5) +
    
    # Área sombreada para el intervalo de confianza del 80%
    geom_ribbon(data = df_pronostico, aes(x = fecha, ymin = lower_80, ymax = upper_80), fill = "gray60", alpha = 0.5) +
    
    # Línea de la predicción media
    geom_line(data = df_pronostico, aes(x = fecha, y = media), color = "gray30", size = 1, linetype = "dashed") +
    
    # Líneas verticales cada 5 años
    geom_vline(xintercept = años_marcados, linetype = "dotted", color = "gray50") +
    
    # Etiquetas y formato
    ggtitle(paste(nombre_grafico, "a 5 años")) +
    ylab(nombre_grafico) +
    xlab("Año") +
    scale_x_continuous(breaks = años_marcados) +
    theme_minimal()
  
  # Mostrar la gráfica
  print(p)
}

# Generar gráficos para cada variable por separado
for (var in variables) {
  pronosticar_variable(var, general)
}







