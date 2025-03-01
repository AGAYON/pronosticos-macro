#ESTIMACIONES A 5 AÑOS

setwd("C:/SEM 8/Economía del Riesgo/pronósticos/csv")

#librerias
library(tidyverse)
library(fuzzyjoin)
library(ranger)
library(workflowsets)

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
  ungroup()


################
################

#PRONOSTICOS

################
################

# ⚡ Definir las variables a usar (antes de correr el modelo)
target_var <- "pib"
predictors <- c("tasa_objetivo", "pesos_x_dolar", "total_desempleo", "variacion_anual") 

# ⚡ Crear la receta flexible
receta <- recipe(as.formula(paste(target_var, "~", paste(predictors, collapse = " + "))), data = general) %>%
  step_naomit(all_predictors(), all_outcomes()) %>%  # Eliminar filas con NA
  step_normalize(all_numeric_predictors()) %>%  # Normalizar las variables numéricas
  step_dummy(all_nominal_predictors())  # Convertir variables categóricas en dummies (si hubiera)

# ⚡ Especificar el modelo Random Forest
modelo_rf <- rand_forest(mode = "regression", trees = 500) %>% 
  set_engine("ranger")

# ⚡ Crear el workflow (flujo de trabajo)
workflow_rf <- workflow() %>%
  add_recipe(receta) %>%
  add_model(modelo_rf)

# ⚡ Dividir los datos en entrenamiento y prueba (80/20)
set.seed(123)  # Para reproducibilidad
split <- initial_split(general, prop = 0.8)
train_data <- training(split)
test_data <- testing(split)

# ⚡ Ajustar el modelo con los datos de entrenamiento
modelo_entrenado <- workflow_rf %>%
  fit(data = train_data)

# ⚡ Hacer predicciones en los datos de prueba
predicciones <- predict(modelo_entrenado, new_data = test_data) %>%
  bind_cols(test_data %>% select(target_var))

# ⚡ Evaluar el rendimiento del modelo
metricas <- predicciones %>%
  metrics(truth = !!sym(target_var), estimate = .pred)

# ⚡ Mostrar resultados
print(metricas)





