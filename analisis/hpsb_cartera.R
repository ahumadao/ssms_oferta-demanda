pacman::p_load(
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  rio,
  fs, ggthemes, ggsci,RColorBrewer
)

source('carga_dims.R')
source('analisis/funciones.R')

dg_hpsb <- import('C:/Users/rahumadao/Desktop/UPS/mapa derivacion/hpsb/dg_aps_hpsb_junio2025.xlsx') %>% clean_names()
estab_ssms <- import("C:/Users/rahumadao/Desktop/UPS/establecimientos/establecimientos_ssms.xlsx") %>% clean_names()

le <- procesar_fact_le(import('C:/Users/rahumadao/Desktop/UPS/le/abierta/le_abierta_12052025.xlsx'), prestacion = 1)


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
  
  
