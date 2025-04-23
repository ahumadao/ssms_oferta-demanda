# 1. Importar librerías ----

pacman::p_load(
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  lubridate,  # trabajar con fechas
  summarytools,
  rio,
  ggthemes,
  ggsci,
  readxl
)

data <- import("demanda/sigte_15012025.xlsx", sheet='Sigte')

sigte <- data %>% clean_names() %>%
  filter(tipo_prest == 4, is.na(tipo_error), estab_dest == 113100,is.na(f_salida)) %>%
  mutate(espera = round(as.numeric(difftime('2025-01-15', f_entrada, units = "days")),0)) %>% 
  group_by(especialidad_grupo) %>%
  summarise(
    n = n(),
    mean = round(mean(espera),0),
    median = round(median(espera),0)
  ) 

# Crear el boxplot para el promedio de tiempo de espera
ggplot(sigte, aes(x = reorder(especialidad_grupo), y = mean)) +
  geom_bar() +
  coord_flip() +  # Para hacerlo horizontal
  labs(title = "Distribución del tiempo de espera promedio por especialidad",
       x = "Especialidad",
       y = "Tiempo de espera promedio")

# Crear el boxplot para la mediana del tiempo de espera
ggplot(datos, aes(x = reorder(especialidad_grupo, mediana_tiempo_espera), y = mediana_tiempo_espera)) +
  geom_boxplot() +
  coord_flip() +  # Para hacerlo horizontal
  labs(title = "Distribución del tiempo de espera (mediana) por especialidad",
       x = "Especialidad",
       y = "Tiempo de espera (mediana)")