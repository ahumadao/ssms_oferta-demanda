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
### Función para procesar LE

# Procesar desde fact_le_abierta


### Proceso los datos de LE

le_hegc <- procesar_fact_le(fact_le_abierta,'113130',1)

le_hblt <- procesar_fact_le(fact_le_abierta,'113100',1) |> 
  filter(edad_sic < 15, desc_esp == 'Otorrinolaringología') 
  
le_hpino <- procesar_fact_le(fact_le_abierta,'113180',1) |> 
  filter(edad_sic < 15, desc_esp == 'Otorrinolaringología') 

### Obtengo los diagnósticos para ORL HEGC

dg_orl_hegc <- fact_mapa_derivacion_trakcare |>
  filter(descripcion_especialidad_demandada == 'Otorrinolaringología',
         codigo_deis_establecimiento_destino == '13-130',
         codigo_cie_10_diagnostico_opcional != "") |>
  distinct(codigo_cie_10_diagnostico_opcional)

### Cruzo la LE del HBLT con la del HEGC
patron_diagnosticos <- paste(dg_orl_hegc$codigo_cie_10_diagnostico_opcional, collapse = "|")

le_hblt_error <- le_hblt |> 
  mutate(corresponde_hegc = if_else(str_detect(sospecha_diag, regex(patron_diagnosticos, ignore_case = TRUE)), "si", "no")) |>
  filter(corresponde_hegc == 'si', f_entrada >= ymd('2022-04-01')) 



### Exporto la base 

export(le_hblt_error, 'output/le_otorrino_hblt_error_cID_06052025.xlsx')
