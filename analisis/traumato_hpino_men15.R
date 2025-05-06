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
### Función para procesar LE

# Procesar desde fact_le_abierta
procesar_fact_le <- function(data, establecimiento,prestacion){
  data.frame(data) |>
    clean_names() |>  # limpio los nombres de las columnas
    mutate(
      estab_dest = as.character(estab_dest),
      estab_orig = as.character(estab_orig),
      c_salida = as.character(c_salida)) |> 
    left_join(dim_estab |> select(codigo_vigente, nombre_oficial, nivel_de_atencion), 
              by=c('estab_dest'='codigo_vigente')) |>
    rename(establecimiento_destino = nombre_oficial,
           nivel_de_atencion_destino = nivel_de_atencion) |> # renombro para evitar duplicidad de nombres
    left_join(dim_estab |> select(
      codigo_vigente,
      nombre_oficial, 
      nombre_comuna,
      nivel_de_atencion), 
      by=c('estab_orig'='codigo_vigente')) |>
    rename(establecimiento_origen = nombre_oficial,
           nivel_de_atencion_origen = nivel_de_atencion) |> # renombro para evitar duplicidad de nombres
    left_join(dim_especialidades, by=c('presta_min'='codigosigte')) |> # combino con especialidades
    left_join( dim_abreviaciones |> mutate(codigo_vigente = as.character(codigo_vigente)), 
               by=c('estab_dest'='codigo_vigente')) |>
    rename('abrev_destino'='abreviacion') |>
    filter(estab_dest == establecimiento, tipo_prest == prestacion, !grepl("^09", presta_min)) |> 
    select(run,dv, sexo, fecha_nac, estab_orig ,codigo_comuna, nivel_de_atencion_origen, f_entrada, tipo_prest ,presta_min, sospecha_diag, 
           estab_dest,presta_est, f_salida, c_salida, abrev_destino, nivel_de_atencion_destino) |>
    left_join(dim_especialidades |> select(codigosigte, desc_esp), by = c('presta_min'='codigosigte')) |>
    left_join(dim_causales, by = c('c_salida'='n')) |>
    mutate(
      fecha_nac = ymd(fecha_nac),
      f_entrada = ymd(f_entrada),
      f_salida = ymd(f_salida),
      f_salida_o_maxima = if_else(is.na(f_salida), max(c(data$f_entrada, data$f_salida), na.rm = TRUE), f_salida),
      tiempo_espera = time_length(interval(f_entrada, f_salida_o_maxima), "days"),
      edad_sic = floor(time_length(interval(fecha_nac, f_entrada), "years"))
    )
}


le_hpino_abierta <- procesar_fact_le(fact_le_abierta,'113181',1) |> 
  filter(edad_sic < 15, presta_min == '07-067') 

le_hpino_cerrada <- procesar_fact_le(fact_le_cerrada,'113181',1) |> 
  filter(edad_sic < 15, presta_min == '07-067') 
