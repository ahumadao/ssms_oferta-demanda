pacman::p_load(
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  rio,
  here,
  fs, ggthemes, ggsci,RColorBrewer
)

source('carga_dims.R')
source('analisis/funciones.R')

estab_ssms <- import("C:/Users/rahumadao/Desktop/UPS/establecimientos/establecimientos_ssms.xlsx") %>% clean_names()
le <- procesar_fact_le(import('C:/Users/rahumadao/Desktop/UPS/le/abierta/le_abierta_09062025.xlsx'), prestacion = 1)
inscripcion <- import("C:/Users/rahumadao/Desktop/UPS/inscripcion aps/paciente_inscripcion_20032025.csv") %>% clean_names()
inscripcion_corto <- inscripcion %>% 
  select(rut, rut_paciente, codigo_deis_comuna, comuna_establecimiento, consultorio_inscripcion)

le_gineobs <- le %>%
  filter(presta_min %in% c('07-058','07-043', '07-066'),
         nombre_comuna %in% c('Buin','Paine','Calera de Tango'),
         nivel_de_atencion_destino %in% c('Terciario','Secundario')) 

export(le_gineobs, here('output/le_gineObs_09062025.xlsx'))

red_sur_poniente <- estab_ssms %>% 
  clean_names() %>% mutate(codigo_antiguo = as.character(codigo_antiguo)) %>% select(codigo_antiguo,x2, comuna, minired) %>%
  filter(minired == 'Red Sur Poniente') %>%
  rename(estab_orig = x2)


le_dg_hpsb <- le %>%
  filter(estab_orig %in% red_sur_poniente$estab_orig,
         estab_dest != "200556") %>%
  separate(
    col    = sospecha_diag,
    into   = c("codigo_cie10", "glosa_cie10"),
    sep    = "\\s*-\\s*",
    fill   = "right",   # si no hay guión, deja NA en 'glosa'
    extra  = "merge" 
  ) %>% 
  mutate(
    cartera_hpsb = case_when(
      codigo_cie10 %in% dg_hpsb$codigo ~ 'si',
      TRUE ~ 'no'),
    destino_hpsb = case_when(
      estab_dest == '113190' ~ 'si',
      TRUE ~ 'no')
  ) %>%
  filter(nivel_de_atencion_origen == 'Primario', 
         nivel_de_atencion_destino %in% c('Secundario','Terciario'))

tabla <- le_dg_hpsb  %>% 
  count(destino_hpsb, cartera_hpsb) 