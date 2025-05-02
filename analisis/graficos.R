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
  readxl,
  zoo,
  scales)

# 2. Cargar los datos ----
source("cubo.R")


# 3. Seleccionar la especialidad y establecimiento deseada----

especialidad <- "07-051"  # Cambiar por la especialidad que quieras
establecimiento <- 'HBLT'

# 4. Procesar para graficar ----


demanda_acumulada_inicial <- fact_le_cne_mensual %>%
  filter(ano < 2024) %>%
  group_by(estab_dest,presta_min) %>%
  summarise(n=sum(demanda_mensual)) %>%
  left_join(import(here('general','abreviacion.xlsx')) %>% clean_names() %>% mutate(codigo_vigente=as.character(codigo_vigente)) , 
            by = c('estab_dest'='codigo_vigente')) %>% 
  filter(abreviacion == establecimiento, presta_min == especialidad) %>%
  summarise(valor_inicial = sum(n, na.rm = TRUE)) %>%
  pull(valor_inicial)

nombre_especialidad <- dim_especialidades %>%
  filter(codigosigte == especialidad) %>%
  pull(desc_esp)

nombre_hospital <- dim_estab %>%
  filter(abreviacion == establecimiento) %>%
  pull(nombre_oficial)

cubo_cne_filtrado <- cubo_rem_cne %>%
  filter(codigosigte == especialidad, abreviacion==establecimiento) %>% 
  mutate(fecha = ymd(paste(ano, mes, "01", sep = "-"))) %>%
  arrange(fecha) %>%
  mutate(demanda_acumulada = demanda_acumulada_inicial + cumsum(demanda_mensual - egresos_mensual))
    
  
  

ggplot(cubo_cne_filtrado, aes(x = fecha)) +
  # Área para demanda acumulada
  geom_area(aes(y = demanda_acumulada / 10, fill = "Demanda acumulada"), alpha = 0.4) +  # Escalamos para el eje secundario
  # Líneas para los otros indicadores
  geom_line(aes(y = demanda_mensual, color = "Demanda mensual"), size = 1) +
  #geom_line(aes(y = produccion_cne, color = "Producción CNE"), size = 1) +
  geom_line(aes(y = prog_mensual, color = "Programación mensual"), size = 1) +
  geom_line(aes(y = egresos_mensual, color = "Egresos mensual"), size = 1) +
  # Configurar ejes
  scale_y_continuous(
    name = "Indicadores",
    labels = label_comma(),  # Formato para el eje izquierdo
    sec.axis = sec_axis(~ . * 10, name = "Demanda acumulada", labels = label_comma())  # Formato para el eje derecho
  ) +
  # Etiquetas y tema
  labs(
    title = paste("Evolución de la oferta, demanda y producción de", nombre_especialidad),
    subtitle = paste('en', nombre_hospital),
    x = "Fecha",
    y = "Cantidad",
    color = "Indicadores",
    fill = "Demanda Acumulada"
  ) +
  theme_minimal()


