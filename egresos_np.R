pacman::p_load(
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  rio,
  fs, ggthemes, ggsci,RColorBrewer
)

source('carga_dims.R')
source('analisis/funciones_analisis_egresos.R')

egresos_bruto <- import("C:/Users/rahumadao/Desktop/UPS/le/cerrada/le_cerrada_12052025.xlsx")

egresos_procesado <- procesar_egresos(egresos_bruto, prestacion = '1')

egresos_procesado_np <- egresos_procesado %>%
  filter(c_salida == '14',
         presta_min == '07-037') 
  

egresos_procesado_nefro <- egresos_procesado %>%
  filter(presta_min == '07-037') %>%
  mutate(month = month(f_salida),
         year = year(f_salida),
         ymd = ymd(paste0(year,' - ',month,'- 01')),
         causal_grupo = case_when(
           c_salida == '1' ~ 'Atendido',
           c_salida == '14' ~ 'No Pertinente',
           TRUE ~ 'Otra')
         ) %>%
  group_by(year, estab_orig, causal_grupo) %>%
  summarise(n = n()) %>%
  filter(year == '2024')

tabla <- egresos_procesado_nefro %>%
  count(causal)

hblt <- procesar_y_graficar_np(
  archivo_egresos = egresos_bruto,
  codigo_establecimiento = "113100",
  abreviacion_establecimiento = "HBLT",
  tipo_prestacion = 1,
  anios = 2023:2025,
  anio_referencia = 2025)
