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

################### Gráficos ##############
############ Tema personalizado ############

np_hegc <- procesar_y_graficar_np(
  archivo_egresos = egresos_bruto,
  codigo_establecimiento = "113130",
  abreviacion_establecimiento = "HEGC",
  tipo_prestacion = 1,
  anios = 2023:2025,
  anio_referencia = 2025)


########## Diagnósticos NP ################

egresos_procesado_hegc <- procesar_egresos(egresos_bruto,'113130',1)

diagnosticos_np <- egresos_procesado_hegc |>
  filter(c_salida == '14',
         f_salida > dmy('31-12-2023'), f_salida < dmy('01-01-2025'),
         desc_esp %in% c('Psiquiatría Pediátrica y de la Adolescencia',
                           'Dermatología',
                           'Otorrinolaringología',
                           'Neurología Pediátrica',
                           'Oftalmología')) |>
  group_by(desc_esp, sospecha_diag) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(desc_esp) |>
  slice_max(order_by = n, n = 10, with_ties = FALSE)

nombre_archivo <- 'diagnosticos_top5np2024_especialidad_hegc.xlsx'
export(diagnosticos_np, here('output',nombre_archivo))

########## Diagnóstico autismo ################

autismo <- egresos_procesado_hegc |>
  mutate(edad_sic = floor(time_length(interval(fecha_nac, f_entrada), "years")),
         grupo_edad = case_when(
           edad_sic >= 0  & edad_sic <= 5  ~ "Menor de 5",
           edad_sic > 5  & edad_sic <= 14  ~ "6 a 14",
           edad_sic > 14 & edad_sic < 18 ~ "15 a 17",
           edad_sic > 18 ~ "18 o más",
           TRUE                    ~ NA_character_
         )) |>
  filter(grepl('autismo',sospecha_diag,ignore.case = TRUE),
         c_salida == 14, 
         edad_sic > 5,
         f_salida >= ymd('2024-05-08')) |>
  group_by(grupo_edad, desc_esp) |>
  summarise(n = n())

orl <- egresos_procesado_hegc |>
  mutate(edad_sic = floor(time_length(interval(fecha_nac, f_entrada), "years")),
         grupo_edad = case_when(
           edad_sic >= 0  & edad_sic <= 5  ~ "Menor de 5",
           edad_sic > 5  & edad_sic <= 14  ~ "6 a 14",
           edad_sic > 14 & edad_sic < 18 ~ "15 a 17",
           edad_sic > 18 ~ "18 o más",
           TRUE                    ~ NA_character_
         )) |>
  filter(c_salida == 14, 
         f_salida >= ymd('2024-01-01'),
         f_salida <= ymd('2024-12-31'),
         desc_esp == 'Otorrinolaringología') |>
  group_by(sospecha_diag) |>
  summarise(n = n())
