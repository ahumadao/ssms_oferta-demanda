pacman::p_load(
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  rio,
  fs
)

fact_prog_CNEyCCE <- consol_prog_med |> #resumen anual
  filter(
    id_actividad %in% c(1,2)
  ) |> 
  group_by(
    id_deis,
    id_especialidad,
    id_actividad,
  ) |> 
  summarise(
    produccion_1_sem = sum(as.numeric(produccion_1_sem)),
    produccion_2_sem = sum(as.numeric(produccion_2_sem)),
    produccion_anual = produccion_1_sem + produccion_2_sem,
    horas = sum(horas_asignadas)
  )

fact_horas_especialidades <- consol_prog_med |>
  group_by(id_deis,id_especialidad,id_actividad) |>
  summarise(
    horas_totales = sum(total_horas_semanales_contratadas_run)
  )


rm(consol_prog_med)
