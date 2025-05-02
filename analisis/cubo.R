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
# 3. proceso tablas fact mensuales ----

fact_le_cne_mensual <- fact_le_cne %>%
  mutate(
    mes = month(f_entrada),
    ano = year(f_entrada)
  ) %>%
  count(ano,mes,estab_dest,presta_min) %>%
  rename(demanda_mensual = n)

fact_egresos_cne_mensual <- fact_egresos_cne %>%
  mutate(
    mes = month(f_salida),
    ano = year(f_salida)
  ) %>%
  count(ano,mes,estab_dest,presta_min) %>%
  rename(egresos_mensual = n) %>%
  filter( ano > 2023, ano < 2025 )

fact_prog_mensual <- fact_prog_CNEyCCE %>%
  mutate(
    prog_mensual = round((produccion_1_sem + produccion_2_sem) / 12),
    id_especialidad = paste0('0',id_especialidad),
    id_deis = as.character(id_deis)
      )
  
fact_egresos_cne_2024 <- fact_egresos_cne_mensual %>%
  group_by(estab_dest,presta_min) %>%
  summarise(n= sum(egresos_mensual)) %>% 
  left_join(dim_estab %>% select(abreviacion,codigo_vigente),by=c('estab_dest'='codigo_vigente')) %>% 
  left_join(dim_especialidades %>% select(codigosigte,desc_esp),by=c('presta_min'='codigosigte')) %>% 
  filter(abreviacion %in% c('HBLT','HSLBP','HEGC','CRSPINO','HPSB'), !is.na(desc_esp))


fact_le_cne_acumulada <- fact_le_cne_mensual %>%
  filter(ano < 2024) %>%
  group_by(estab_dest,presta_min) %>%
  summarise(n=sum(demanda_mensual)) %>%
  left_join(dim_estab %>% select(abreviacion,codigo_vigente),by=c('estab_dest'='codigo_vigente')) %>% 
  left_join(dim_especialidades %>% select(codigosigte,desc_esp),by=c('presta_min'='codigosigte')) %>% 
  filter(abreviacion %in% c('HBLT','HSLBP','HEGC','CRSPINO','HPSB'),!is.na(desc_esp))

fact_demanda_acumulada <- fact_le_cne_acumulada %>%
  left_join(fact_egresos_cne_2024, by = c('estab_dest','abreviacion','presta_min','desc_esp')) %>%
  rename(acum_le = n.x, acum_egresos = n.y) %>% 
  mutate(
    demanda_acumulada = acum_le + acum_egresos )


# 4. Genero cubo de demanda y produccion. ----

cubo_rem_cne <- fact_produccion_rem %>%
  left_join(dim_estab %>% select(codigo_vigente,nombre_oficial,nombre_comuna), 
            by = c('id_establecimiento'='codigo_vigente')) %>%
  left_join(dim_especialidades %>% select(codigosigte,desc_esp),
            by='codigosigte') %>%
  left_join(fact_le_cne_mensual, 
            by = c('ano','mes','id_establecimiento'='estab_dest',
                   'codigosigte'='presta_min')) %>%
  left_join(fact_prog_mensual %>% select(-produccion_1_sem,-produccion_2_sem) %>% filter(id_actividad == 1), 
            by= c('id_establecimiento' = 'id_deis',
                  'codigo_prestacion.x'='id_especialidad')) %>% 
  left_join(import(here('general','abreviacion.xlsx')) %>% clean_names() %>% mutate(codigo_vigente=as.character(codigo_vigente)) , 
            by = c('id_establecimiento'='codigo_vigente')) %>%
  left_join(fact_egresos_cne_mensual, 
            by = c('ano','mes','id_establecimiento'='estab_dest',
                   'codigosigte'='presta_min')) %>% 
  mutate(produccion_cne = consultas_menores15+consultas_mayores) %>%
  select(mes, ano, id_establecimiento,abreviacion, codigosigte,desc_esp, demanda_mensual, egresos_mensual, produccion_cne, prog_mensual) %>%
  mutate(fecha = ymd(paste(ano, mes, "01", sep = "-")))
  


export(cubo_rem_cne,'cubo.xlsx')
export(fact_demanda_acumulada,'demanda_inicial_2024.xlsx')
export(fact_produccion_rem,'produccionREM_2024.xlsx')

################################
################################
################################
################################

brecha <- fact_demanda_acumulada
