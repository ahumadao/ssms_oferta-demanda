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

buscar_consultorio <- function(rut_buscar,base){
  base <- data.frame(base) |>
    filter(rut == rut_buscar|rut_paciente == rut_buscar)
}
rut <- "181261889"
buscar <- buscar_consultorio(rut,dim_inscripcion)
