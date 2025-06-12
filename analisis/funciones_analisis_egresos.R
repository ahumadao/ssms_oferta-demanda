########### Funciones varias #########

procesar_egresos <- function(data, establecimiento = NULL,prestacion){
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
      import(ruta_dim_abreviaciones) |> 
        clean_names() |> 
        mutate(codigo_vigente = as.character(codigo_vigente)), 
      by=c('estab_dest'='codigo_vigente')) |>
    rename('abrev_destino'='abreviacion') |>
    filter(if (!is.null(establecimiento)) estab_dest %in% establecimiento else TRUE, 
           tipo_prest == prestacion, 
           !grepl("^09", presta_min)) |> #tipo_prest =1:CNE =4:Qx
    select(-comuna) |>
    select(run,dv, sexo, fecha_nac, 
           estab_orig ,codigo_comuna, f_entrada, tipo_prest ,presta_min, sospecha_diag, 
           estab_dest,presta_est, 
           f_salida, c_salida, abrev_destino) |>
    left_join(dim_especialidades |> select(codigosigte, desc_esp), by = c('presta_min'='codigosigte')) |>
    left_join(dim_causales, by = c('c_salida'='n'))
}
calcular_porcentaje_np <- function(data, anio) {
  data.frame(data) |>
    filter(f_salida >= ymd(paste0(anio, "-01-01")), f_salida < ymd(paste0(anio + 1, "-01-01"))) |> 
    group_by(abrev_destino, desc_esp, causal) |>
    summarise(n = n(), .groups = "drop") |> 
    pivot_wider(
      names_from = causal,
      values_from = n,
      values_fill = 0
    ) |> 
    clean_names() |> 
    mutate(
      total_egresos = rowSums(across(where(is.numeric) & !matches("desc_esp"))),
      porc_np_total = round(no_pertinencia / total_egresos, 2),
      anio = anio) |>
    filter(total_egresos >= 10)
}
tema_personalizado <- function(){
  theme_minimal() + 
    theme(
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 8, face = "italic", hjust = 0.5),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 7), 
      legend.title = element_text(size = 7, face = "italic"),
      legend.text = element_text(size = 5),
      panel.grid.minor = element_blank(),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      axis.line = element_line(linewidth = 0.3, color = "black"), 
      legend.background = element_rect(fill = "white"), 
      legend.key = element_rect(fill = "white", color = NA),
      legend.key.size = unit(0.5, "cm"),  # Adjust size of legend keys
      legend.spacing.y = unit(0.01, "cm"),
      legend.position = "right",
      plot.margin = margin(20, 20, 20, 20))
  
}
custom_theme <- tema_personalizado()
grafico_barras <- function(data,tema){
  np_especialidad <- data 
  ggplot(np_especialidad, aes(x = desc_esp, y = porc_np_total, fill = factor(anio))) +
    geom_col(position = position_dodge()) +
    labs(
      title = paste0("Porcentaje de No Pertinencia por Especialidad - ", np_especialidad$abrev_destino[1]),
      subtitle = "En orden decreciente según porcentaje de NP del año 2025",
      x = NULL,
      y = "Porcentaje No Pertinencia",
      fill = "Año"
    ) +
    tema +
    scale_y_continuous(breaks = seq (0,1, by =0.05), 
                       labels = scales::percent_format()) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_colour_tableau("Tableau 20") 
  ggsave(paste0("figs/grafico_np_especialidad_",np_especialidad$abrev_destino[1],".png"), plot = last_plot(), dpi = 300, width = 12, height = 6, units = "in")
}

procesar_y_graficar_np <- function(archivo_egresos, codigo_establecimiento, abreviacion_establecimiento, tipo_prestacion, anios, anio_referencia, ruta_figs = "figs/") {
  # Procesar egresos
  egresos_procesado <- procesar_egresos(archivo_egresos, codigo_establecimiento, tipo_prestacion)
  # Calcular % NP por año
  datos_np <- purrr::map_dfr(anios, ~calcular_porcentaje_np(egresos_procesado, .x))
  # Ordenar especialidades según año de referencia
  orden_esp <- datos_np |>
    filter(anio == anio_referencia) |>
    arrange(desc(porc_np_total)) |>
    pull(desc_esp)
  datos_np$desc_esp <- factor(datos_np$desc_esp, levels = orden_esp)
  
  # Gráfico
  grafico <- ggplot(datos_np, aes(x = desc_esp, y = porc_np_total, fill = factor(anio))) +
    geom_col(position = position_dodge()) +
    labs(
      title = paste0("Porcentaje de No Pertinencia por Especialidad - ", abreviacion_establecimiento),
      subtitle = paste0("Ordenado según el año ", anio_referencia),
      x = NULL,
      y = "Porcentaje No Pertinencia",
      fill = "Año"
    ) +
    scale_fill_brewer(palette = "Purples") +
    tema_personalizado() +
    scale_y_continuous(
      breaks = seq(0, 1, by = 0.05),
      labels = scales::percent_format()
    ) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  # Guardar
  ggsave(
    filename = paste0(ruta_figs, "grafico_np_especialidad_", abreviacion_establecimiento, ".png"),
    plot = grafico,
    dpi = 300,
    width = 12,
    height = 6,
    units = "in"
  )
  
  export(datos_np,paste0('output/egresos_np_',abreviacion_establecimiento,'.xlsx'))
  # Retornar el dataset para seguimiento si se quiere
  return(invisible(datos_np))
}
