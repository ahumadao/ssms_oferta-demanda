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
  readxl,fs
)

source('variables_proyecto.R')


# 2. DIM ESTABLECIMIENTOS ----

dim_estab <- import(ruta_dim_establecimientos, skip = 1) %>% 
  clean_names() %>% 
  select(codigo_vigente, codigo_antiguo, nombre_oficial, codigo_comuna,nombre_comuna, nivel_de_atencion, nombre_dependencia_jerarquica_seremi_servicio_de_salud, nivel_de_complejidad)  %>% 
  mutate(codigo_vigente = as.character(codigo_vigente)) %>%
  filter(nombre_dependencia_jerarquica_seremi_servicio_de_salud == 'Servicio de Salud Metropolitano Sur') %>%
  left_join(
    import(ruta_dim_abreviaciones) %>% 
      clean_names() %>% 
      mutate(codigo_vigente = as.character(codigo_vigente)), 
    by=c('codigo_vigente'='codigo_vigente')) 


# 3. DIM ESPECIALIDADES ----

dim_especialidades <- import(ruta_dim_especialidades) %>% clean_names()


# 4. DIM CAUSALES DE EGRESO ----
dim_causales <- import(ruta_dim_causales) %>%
  mutate(n=as.character(n))

# 5. DIM ACTIVIDADES PROGRAMACION ----

dim_act_programacion <- import(ruta_dim_actividades_programacion) %>%
  mutate(n=as.character(n))

# 6. DIM TIPO PRESTACIÓN LE ----

dim_tipo_prestaciones_le <- data.frame(
  n = c(1, 2,3,4),
  descripcion = c("Consulta nueva de especialidad",
                  "Consulta control",
                  "Procedimiento",
                  "Intervención quirúrgica"))

dim_abreviaciones <- import(ruta_dim_abreviaciones) |> clean_names()

# 7. DIM ESTABLECIMIENTO DE INSCRIPCIÓN ----

# dim_inscripcion <- import(ruta_dim_inscripcion_APS) %>% clean_names %>%
#   select(rut_paciente,rut,apellido_paterno,consultorio_inscripcion,comuna_establecimiento_id,comuna_establecimiento) %>%
#   filter(!is.na(rut_paciente)|!is.na(rut)) %>%
#   mutate(
#     rut_paciente_sdv = str_sub(rut_paciente, 1, -2),
#     rut_sdv = str_sub(rut, 1, -2)
#   )


