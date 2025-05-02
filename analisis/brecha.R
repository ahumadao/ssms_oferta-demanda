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
  readxl
)

# 2. Traigo los DIM y FACTS  ----
source('carga_facts.R')

# 3. Cálculo de Brecha ----

################################################################ #
              #  #### Cálculo de Brecha ###### #
################################################################ #

## DEFINO EL HOSPITAL A CALCULAR ##
hospital <- '113100'

## PROCESO PARA OBTENER EL TIPO DE PRESTACION Y EL HOSPITAL ##

a_le_cne_completa <- rbind(fact_le_abierta,fact_le_cerrada) |>
  filter(tipo_prest == 1, estab_dest == hospital, !grepl('09-',presta_min)) |> # saqué las especialidades odontológicas
  left_join(dim_especialidades |> select(desc_esp, codigosigte), by = c('presta_min'='codigosigte'))
  
a_demanda_inicial <- le_cne_completa |>
  filter(as.Date(f_entrada) < as.Date('2024-01-01'), (as.Date(f_salida) >= as.Date('2024-01-01')|is.na(f_salida)) ) |>
  count(presta_min, desc_esp) |>
  rename(demanda_inicial = n)

a_demanda_2024 <- le_cne_completa |> 
  filter(as.Date(f_entrada) >= as.Date('2024-01-01')) |>
  count(presta_min, desc_esp) |>
  rename(demanda_2024 = n)

a_produccion_2024 <- fact_produccion_rem_consultas  |>
  filter(id_establecimiento == '113100') |>
  left_join(dim_especialidades |> select(desc_esp, id_esp_seccion_a ), by = c('codigo_prestacion.x'='id_esp_seccion_a')) |> 
  group_by(codigosigte, desc_esp) |>
  summarise(
    produccion_total = sum(total_consultas),
    produccion_cne = sum(consultas_menores15, na.rm = TRUE) + sum(consultas_mayores, na.rm = TRUE),
    proporcion_cne = round(ifelse(produccion_total == 0 | is.na(produccion_total), NA, produccion_cne / produccion_total), 2),
    produccion_control = produccion_total-produccion_cne, 
    produccion_nsp_total = sum(nsp_nuevas, na.rm = TRUE) + sum(nsp_controles, na.rm = TRUE),
    produccion_nsp_cne = sum(nsp_nuevas,na.rm = TRUE),
    proporcion_nsp_cne = round(ifelse(produccion_nsp_total == 0 | is.na(produccion_nsp_total), NA, produccion_nsp_cne / produccion_nsp_total), 2)
    
  )

a_calculo_brecha <- a_demanda_inicial |>
  left_join(a_demanda_2024, by=c('presta_min','desc_esp')) |>
  mutate(demanda_total = demanda_inicial+demanda_2024) |>
  left_join(a_produccion_2024, by=c('presta_min'='codigosigte','desc_esp')) |>
  mutate(produccion_total_cne_2024 = produccion_cne + produccion_nsp_cne,
         brecha = round(demanda_total/produccion_total_cne_2024, 2))

a_calculo_brecha_actuales <- a_calculo_brecha |>
  filter(!is.na(produccion_cne))



