
#### PROCESAMIENTO LE ####


procesar_fact_le <- function(data, 
                             prestacion = NULL, 
                             establecimiento = NULL, 
                             f_entrada_min = NULL, 
                             f_entrada_max = NULL,
                             f_salida_min = NULL,
                             f_salida_max = NULL,
                             abierta = NULL) {
  
  data.frame(data) |>
    clean_names() |>
    mutate(
      estab_dest = as.character(estab_dest),
      estab_orig = as.character(estab_orig),
      c_salida = as.character(c_salida)
    ) |>
    left_join(dim_estab |> select(codigo_vigente, nombre_oficial, nivel_de_atencion),
              by = c('estab_dest' = 'codigo_vigente')) |>
    rename(establecimiento_destino = nombre_oficial,
           nivel_de_atencion_destino = nivel_de_atencion) |>
    left_join(dim_estab |> select(
      codigo_vigente, nombre_oficial, codigo_comuna, nombre_comuna, nivel_de_atencion),
      by = c('estab_orig' = 'codigo_vigente')) |>
    rename(establecimiento_origen = nombre_oficial,
           nivel_de_atencion_origen = nivel_de_atencion) |>
    left_join(dim_especialidades, by = c('presta_min' = 'codigosigte')) |>
    left_join(dim_abreviaciones |> mutate(codigo_vigente = as.character(codigo_vigente)),
              by = c('estab_dest' = 'codigo_vigente')) |>
    rename('abrev_destino' = 'abreviacion') |>
    mutate(
      fecha_nac = ymd(fecha_nac),
      f_entrada = ymd(f_entrada),
      f_salida = ymd(f_salida)
    ) |>
    filter(
      if (!is.null(prestacion)) tipo_prest %in% prestacion else TRUE,
      if (!is.null(establecimiento)) estab_orig %in% establecimiento else TRUE,
      if (!is.null(f_entrada_min)) f_entrada >= ymd(f_entrada_min) else TRUE,
      if (!is.null(f_entrada_max)) f_entrada <= ymd(f_entrada_max) else TRUE,
      if (!is.null(f_salida_min)) f_salida >= ymd(f_salida_min) else TRUE,
      if (!is.null(f_salida_max)) f_salida <= ymd(f_salida_max) else TRUE,
      if (!is.null(abierta) && abierta) is.na(f_salida) else TRUE,
    ) |>
    select(run, dv, sexo, fecha_nac, estab_orig, establecimiento_origen,
           codigo_comuna, nombre_comuna, nivel_de_atencion_origen, f_entrada, tipo_prest,
           presta_min, sospecha_diag, estab_dest, establecimiento_destino,
           presta_est, f_salida, c_salida, abrev_destino, nivel_de_atencion_destino, sigte_id, id_local) |>
    left_join(dim_especialidades |> select(codigosigte, desc_esp),
              by = c('presta_min' = 'codigosigte')) |>
    left_join(dim_causales, by = c('c_salida' = 'n')) |>
    mutate(
      f_salida_o_maxima = if_else(
        is.na(f_salida),
        as.Date(max(c(f_entrada, f_salida), na.rm = TRUE)),
        f_salida
      ),
      tiempo_espera = time_length(interval(f_entrada, f_salida_o_maxima), "days"),
      edad_sic = floor(time_length(interval(fecha_nac, f_entrada), "years"))
    )
}


unir_le_abierta_cerrada <- function(data_abierta, data_cerrada, prestacion = NULL, 
                                        establecimiento = NULL,
                                        f_entrada_min = NULL, f_entrada_max = NULL,
                                        f_salida_min = NULL, f_salida_max = NULL) {
  
  le_abierta <- procesar_fact_le(
    data = data_abierta,
    prestacion = prestacion,
    establecimiento = establecimiento,
    f_entrada_min = f_entrada_min,
    f_entrada_max = f_entrada_max,
    f_salida_min = f_salida_min,
    f_salida_max = f_salida_max,
    abierta = TRUE
  )
  
  le_cerrada <- procesar_fact_le(
    data = data_cerrada,
    prestacion = prestacion,
    establecimiento = establecimiento,
    f_entrada_min = f_entrada_min,
    f_entrada_max = f_entrada_max,
    f_salida_min = f_salida_min,
    f_salida_max = f_salida_max,
    abierta = FALSE
  )
  
  bind_rows(le_abierta, le_cerrada)
}



