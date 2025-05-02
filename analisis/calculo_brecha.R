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

# 2. Traigo los objetos del normalizador ----
source("normalizador.R")

# 3. Proceso las bases para demanda inicial y producción ----

################################################################ #
######### TRABAJO CON BUIN, PAINE y CALERA DE TANGO ############ #
################################################################ #
################################################################ #
#### Además, filtro especialidades a evaluar  ################## #
#### En este caso, las especialidades del nuevo HSLBP ########## #
################################################################ #
################################################################ #

especialidades_hslbp <- import('especialidades_médicas_hslbp.xlsx')

fact_egresos_cne_insc <- fact_egresos_cne %>%
  mutate(run = as.character(run)) %>% 
  inner_join(dim_inscripcion, by=c('run'='rut_paciente_sdv')) %>%
  filter(comuna_establecimiento %in% c('Buin','Paine','Calera De Tango'),
         presta_min %in% especialidades_hslbp$codigosigte) %>%
  select(-c(run,dv,rut_sdv,rut,rut_paciente,apellido_paterno))

fact_le_cne_insc <- fact_le_cne %>%
  mutate(run = as.character(run)) %>% 
  inner_join(dim_inscripcion, by=c('run'='rut_paciente_sdv')) %>%
  filter(comuna_establecimiento %in% c('Buin','Paine','Calera De Tango'),
         presta_min %in% especialidades_hslbp$codigosigte) %>%
  select(-c(run,dv,rut_sdv,rut,rut_paciente,apellido_paterno))


fact_ingresos_egresos_le_inscripcion <- bind_rows(fact_egresos_cne_insc, fact_le_cne_insc) %>%
  left_join(
    dim_especialidades %>% select(codigosigte,desc_esp), 
    by = c('presta_min'='codigosigte')
  )

################################################################ #
#### filtro especialidades de HSLBP en producción REM 2024###### #
################################################################ #

produccion_rem_hslbp <- fact_produccion_rem  %>%
  filter(codigo_prestacion.x %in% especialidades_hslbp$id_esp_seccion_a,
         id_establecimiento == '113150') %>%
  mutate(total_cne = consultas_mayores + consultas_menores15,
         total_nsp = nsp_nuevas + nsp_controles) %>%
  select(mes,ano,id_establecimiento, codigo_prestacion.x, codigosigte,total_consultas,total_cne,total_nsp,nsp_nuevas) %>%
  left_join(especialidades_hslbp %>% select(codigosigte,desc_esp), by='codigosigte') %>%
  group_by(codigosigte, desc_esp) %>%
  summarise(
    produccion_total = sum(total_consultas),
    produccion_cne = sum(total_cne),
    proporcion_cne = round(produccion_cne/produccion_total,2),
    produccion_control = produccion_total-produccion_cne, 
    produccion_nsp_total = sum(total_nsp),
    produccion_nsp_cne = sum(nsp_nuevas)
  )

################################################################ #
#### exportar ###### #
################################################################ #

export(fact_ingresos_egresos_le_inscripcion, 'ingresos y egresos a LE - inscripcion B-P-CdeT - final.xlsx')
export(produccion_rem_hslbp, 'producción REM a07 especialidades del nuevo hslbp.xlsx')


# 4. Cálculo de Brecha ----

################################################################ #
              #  #### Cálculo de Brecha ###### #
################################################################ #

a_demanda_inicial <- fact_ingresos_egresos_le_inscripcion %>%
  filter(as.Date(f_entrada) < as.Date('2024-01-01')) %>%
  count(presta_min, desc_esp) %>%
  rename(demanda_inicial = n)

a_demanda_2024 <- fact_ingresos_egresos_le_inscripcion %>% 
  filter(as.Date(f_entrada) > as.Date('2023-12-31')) %>%
  count(presta_min, desc_esp) %>%
  rename(demanda_2024 = n)

a_produccion_2024 <- fact_produccion_rem  %>%
  filter(codigo_prestacion.x %in% especialidades_hslbp$id_esp_seccion_a,
         id_establecimiento == '113150') %>%
  left_join(especialidades_hslbp %>% select(codigosigte,desc_esp), by='codigosigte') %>%
  group_by(codigosigte, desc_esp) %>%
  summarise(
    produccion_total = sum(total_consultas),
    produccion_cne = sum(consultas_menores15, na.rm = TRUE) + sum(consultas_mayores, na.rm = TRUE),
    proporcion_cne = round(ifelse(produccion_total == 0 | is.na(produccion_total), NA, produccion_cne / produccion_total), 2),
    produccion_control = produccion_total-produccion_cne, 
    produccion_nsp_total = sum(nsp_nuevas, na.rm = TRUE) + sum(nsp_controles, na.rm = TRUE),
    produccion_nsp_cne = sum(nsp_nuevas,na.rm = TRUE),
    proporcion_nsp_cne = round(ifelse(produccion_nsp_total == 0 | is.na(produccion_nsp_total), NA, produccion_nsp_cne / produccion_nsp_total), 2),
    
  )

a_calculo_brecha <- a_demanda_inicial %>%
  left_join(a_demanda_2024, by=c('presta_min','desc_esp')) %>%
  mutate(demanda_total = demanda_inicial+demanda_2024) %>%
  left_join(a_produccion_2024, by=c('presta_min'='codigosigte','desc_esp')) %>%
  mutate(produccion_total_cne_2024 = produccion_cne + produccion_nsp_cne,
         brecha = round(demanda_total/produccion_total_cne_2024, 2))

a_calculo_brecha_actuales <- a_calculo_brecha %>%
  filter(!is.na(produccion_cne))

export(a_calculo_brecha %>%
         left_join(proyeccion_produccion %>% select(-especialidad), 
                   by=c('presta_min'='x1')),
       'calculo_brecha.xlsx' )

proyeccion_produccion <- import(here('producción/proyeccion_produccion.xlsx')) %>%
  clean_names()

a_calculo_brecha_nuevas <- a_calculo_brecha %>%
  filter(is.na(produccion_cne)) %>%
  select(1:5) #%>%
  #left_join(proyeccion_produccion %>% select(-especialidad), by=c('presta_min'='x1'))

total_cne_2024 <- sum(a_calculo_brecha_actuales$produccion_cne, na.rm = TRUE)

a_calculo_brecha_nuevas_PROY_ACT <- a_calculo_brecha_nuevas %>%
  left_join(
    proyeccion_produccion %>% select(1,5,8:12,19:21,-especialidad), by=c('presta_min'='x1')
    )


