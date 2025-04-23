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

especialidades_hslbp <- import('general/especialidades_médicas_hslbp.xlsx')

produccion_rem_hblt <- fact_produccion_rem  %>%
  filter(codigo_prestacion.x %in% especialidades_hslbp$id_esp_seccion_a,
         id_establecimiento == '113100') %>%
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

programacion_cne_hblt <- fact_prog_CNEyCCE %>% filter(id_deis == 113100, id_actividad == '1') %>%
  mutate(total_cne = produccion_1_sem + produccion_2_sem) %>% 
  select(id_deis,id_especialidad, total_cne) %>%
  filter(id_especialidad %in% especialidades_hslbp$id_programacion) %>% 
  left_join(dim_especialidades %>% select(desc_esp,codigosigte, id_programacion) %>% mutate(id_programacion=as.character(id_programacion)), by= c('id_especialidad'='id_programacion'))

produccion_programacion <- produccion_rem_hblt %>%
  left_join(programacion_cne_hblt %>% select(codigosigte,total_cne), by='codigosigte') %>%
  rename(prog_cne_anual = total_cne) %>% 
  left_join(import('general/rendimiento por especialidad.xlsx') %>% select(-desc_esp),by=c('codigosigte'='presta_min')) %>%
  left_join(fact_horas_especialidades %>% 
              filter(id_actividad == '1', id_deis == '113100') %>%
              select(-id_actividad),
            by= 'id_especialidad') %>%
  mutate(
    produccion_cne_estandar = round(horas_totales *distribucion_ambulatoria*distribucion_nueva*rendimiento_nueva*31.4,0),
    proporcion_real_estandar = round(((produccion_cne + produccion_nsp_cne)/produccion_cne_estandar),2)
  )

promedios <- mean(produccion_programacion$proporcion_real_estandar, na.rm = TRUE)
            