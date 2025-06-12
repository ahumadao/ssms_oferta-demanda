pacman::p_load(
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  rio,
  fs, ggthemes, ggsci,RColorBrewer
)


egresos <- procesar_fact_le(egresos_bruto, 
                            prestacion = 1, 
                            establecimiento = '113130',
                            f_salida_min = '2023-01-01',
                            f_salida_max = '2025-12-31')


export(egresos, 'output/egresos_hegc_2325_ssms.xlsx')
