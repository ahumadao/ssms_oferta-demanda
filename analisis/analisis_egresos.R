pacman::p_load(
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  rio,
  fs
)

source('carga_dims.R')

# Carga egresos 

egresos_bruto <- import("C:/Users/rahumadao/Desktop/UPS/le/cerrada/le_cerrada_28042025.xlsx")

# Procesamiento de los egresos 

egresos_procesado <- egresos_bruto |>
  clean_names() |>  # limpio los nombres de las columnas
  mutate(
    estab_dest = as.character(estab_dest),
    estab_orig = as.character(estab_orig),
    c_salida = as.character(c_salida)) |> 
  left_join(dim_estab |> select(codigo_vigente, nombre_oficial, nivel_de_atencion), 
            by=c('estab_dest'='codigo_vigente')) |>
  rename(establecimiento_destino = nombre_oficial,
         nivel_de_atencion_destino = nivel_de_atencion) |> # renombro para evitar duplicidad de nombres
  left_join(dim_estab |> #junto los dataframes para obtener el est orig, comuna, nivel de atención
              select(codigo_vigente,
                     nombre_oficial, 
                     codigo_comuna,
                     nombre_comuna,
                     nivel_de_atencion), 
            by=c('estab_orig'='codigo_vigente')) |>
  rename(establecimiento_origen = nombre_oficial,
         nivel_de_atencion_origen = nivel_de_atencion) |> # renombro para evitar duplicidad de nombres
  left_join(dim_especialidades, by=c('presta_min'='codigosigte')) |> # combino con especialidades
  left_join( # combino con dataframe de abreviaciones
    import(here('general','abreviacion.xlsx')) |> 
      clean_names() |> 
      mutate(codigo_vigente = as.character(codigo_vigente)), 
    by=c('estab_dest'='codigo_vigente')) |>
  rename('abrev_destino'='abreviacion') |>
  filter(estab_dest == '113130', tipo_prest == 1, !grepl("^09", presta_min)) |> #tipo_prest =1:CNE =4:Qx
  select(-comuna) |>
  select(run,dv, sexo, fecha_nac, 
         estab_orig ,codigo_comuna, f_entrada, tipo_prest ,presta_min, sospecha_diag, 
         estab_dest,presta_est, 
         f_salida, c_salida) |>
  left_join(dim_especialidades |> select(codigosigte, desc_esp), by = c('presta_min'='codigosigte')) |>
  left_join(dim_causales, by = c('c_salida'='n'))

causales_especialidad_2024 <- egresos_procesado |>
  filter(c_salida %in% c('1','14'),
         f_salida > dmy('31-12-2023'), f_salida < dmy('01-01-2025'),) |> 
  group_by(desc_esp,causal) |>
  summarise(n = n()) |>
  pivot_wider(
    names_from = causal,
    values_from = n,
    values_fill = 0  # rellena con 0 si no hay registros
  ) |> clean_names() |> 
  mutate(
    porc_np_realizada = round(no_pertinencia/(atencion_realizada+no_pertinencia),2)
  )

########### Causales por especialidad ##########

causales_especialidad_2025 <- egresos_procesado |>
  filter(c_salida %in% c('1','14'),
         f_salida > dmy('31-12-2024'), f_salida < dmy('01-01-2026'),) |> 
  group_by(desc_esp,causal) |>
  summarise(n = n()) |>
  pivot_wider(
    names_from = causal,
    values_from = n,
    values_fill = 0  # rellena con 0 si no hay registros
  ) |> clean_names() |> 
  mutate(
    porc_np_realizada = round(no_pertinencia/(atencion_realizada+no_pertinencia),2)
  )

causales_especialidad_hegc <- causales_especialidad_2024 |>
  left_join(causales_especialidad_2025, by = 'desc_esp')

export(causales_especialidad_hegc, here('output','causales_egreso_especialidad_hegc.xlsx'))

########## Diagnósticos NP ################

diagnosticos_np <- egresos_procesado |>
  filter(c_salida == '14',
         f_salida > dmy('31-12-2023'), f_salida < dmy('01-01-2026')) |>
  group_by(desc_esp, sospecha_diag) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(desc_esp) |>
  slice_max(order_by = n, n = 10, with_ties = FALSE)

nombre_archivo <- 'diagnosticos_np_especialidad_hegc.xlsx'
export(diagnosticos_np, here('output','diagnosticos_np_especialidad_hegc.xlsx'))

