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

# 2. DIM ESTABLECIMIENTOS ----

dim_estab <- import(here("general","deis2024.xlsx")) %>% #importo datos
  clean_names() %>% #normalizo los nombres nombres de las columnas
  select(codigo_vigente, codigo_antiguo, nombre_oficial, codigo_comuna,nombre_comuna, nivel_de_atencion, nombre_dependencia_jerarquica_seremi_servicio_de_salud, nivel_de_complejidad)  %>% 
  mutate(codigo_vigente = as.character(codigo_vigente)) %>%
  filter(nombre_dependencia_jerarquica_seremi_servicio_de_salud == 'Servicio de Salud Metropolitano Sur') %>%
  left_join( # combino con dataframe de abreviaciones
    import(here('general','abreviacion.xlsx')) %>% 
      clean_names() %>% 
      mutate(codigo_vigente = as.character(codigo_vigente)), 
    by=c('codigo_vigente'='codigo_vigente')) 


# 3. DIM ESPECIALIDADES ----

dim_especialidades <- import(here("general","especialidades_ok.xlsx")) %>% clean_names()


# 4. DIM CAUSALES DE EGRESO ----
dim_causales <- import(here("general",'causales.xlsx')) %>%
  mutate(n=as.character(n))

# 5. DIM ACTIVIDADES PROGRAMACION ----

dim_act_programacion <- import(here("general",'actividades_programacion.xlsx')) %>%
  mutate(n=as.character(n))

# 6. DIM TIPO PRESTACIÓN LE ----

dim_tipo_prestaciones_le <- data.frame(
  n = c(1, 2,3,4),
  descripcion = c("Consulta nueva de especialidad",
                  "Consulta control",
                  "Procedimiento",
                  "Intervención quirúrgica"))


# 7. DIM ESTABLECIMIENTO DE INSCRIPCIÓN ----

dim_inscripcion <- import('general/paciente_inscripcion_20032025.csv') %>% clean_names %>%
  select(rut_paciente,rut,apellido_paterno,consultorio_inscripcion,comuna_establecimiento_id,comuna_establecimiento) %>%
  filter(!is.na(rut_paciente)|!is.na(rut)) %>%
  mutate(
    rut_paciente_sdv = str_sub(rut_paciente, 1, -2),
    rut_sdv = str_sub(rut, 1, -2)
  )
