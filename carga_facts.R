# 1. Importar librerías ----

pacman::p_load(
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  rio,
  fs
)

source('variables_proyecto.R')
source('carga_dims.R')


# 2. FACT PROGRAMACIÓN ----

hegc_m <- import(ruta_fact_programacion_hegc, skip=0, sheet="Programación Medica") |> clean_names()
hblt_m <- import(ruta_fact_programacion_hblt, skip=0,sheet=2) |> clean_names()
hdlc_m <- import(ruta_fact_programacion_hdlc, skip=0,sheet=2) |> clean_names()
hpino_m <- import(ruta_fact_programacion_hpino, skip=0,sheet=2) |> clean_names() |> rename('rutsdv'=3)
hslb_m <- import(ruta_fact_programacion_hslbp, skip=0,sheet=2) |> clean_names()
bases_m <- list(hegc_m, hblt_m,hdlc_m,hpino_m,hslb_m)

fact_programacion_medica <- do.call(rbind, bases_m) #unir bases
rm(hblt_m, hdlc_m,hegc_m,hpino_m,hslb_m,bases_m) #remover datos no usados


# 3. FACT LISTA DE ESPERA ----

data_bruta <- import(ruta_fact_le_abierta)

fact_le_abierta <- data_bruta |>
  clean_names() |>  
  mutate(
    estab_dest = as.character(estab_dest),
    estab_orig = as.character(estab_orig)) |> 
  left_join(dim_estab |> select(codigo_vigente, nombre_oficial, nivel_de_atencion), 
            by=c('estab_dest'='codigo_vigente')) |>
  rename(establecimiento_destino = nombre_oficial,
         nivel_de_atencion_destino = nivel_de_atencion) |> # renombro para evitar duplicidad de nombres
  left_join(dim_estab |> #junto los dataframes para obtener el est orig, comuna, nivel de atención
              select(codigo_vigente,
                     nombre_oficial, 
                     codigo_comuna,
                     nombre_comuna,
                     nivel_de_atencion), 
            by=c('estab_orig'='codigo_vigente')) |>
  rename(establecimiento_origen = nombre_oficial,
         nivel_de_atencion_origen = nivel_de_atencion) |> # renombro para evitar duplicidad de nombres
  left_join(dim_especialidades, by=c('presta_min'='codigosigte')) |> # combino con especialidades
  left_join( # combino con dataframe de abreviaciones
    import(ruta_dim_abreviaciones) |> 
      clean_names() |> 
      mutate(codigo_vigente = as.character(codigo_vigente)), 
    by=c('estab_dest'='codigo_vigente')) |>
  rename('abrev_destino'='abreviacion') |>
  filter(is.na(f_salida)) |> #tipo_prest =1:CNE =4:Qx
  select(-comuna) |>
  select(run,dv,sexo, fecha_nac, 
         estab_orig ,codigo_comuna, f_entrada, tipo_prest ,presta_min,sospecha_diag, 
         estab_dest,presta_est, 
         f_salida, c_salida) 


rm(data_bruta)

# 4. FACT EGRESOS 2024 ----

egresos_bruto <- import(ruta_fact_le_cerrada)

fact_le_cerrada <- egresos_bruto |>
  clean_names() |>  
  mutate(
    estab_dest = as.character(estab_dest),
    estab_orig = as.character(estab_orig)) |> 
  left_join(dim_estab |> select(codigo_vigente, nombre_oficial, nivel_de_atencion), 
            by=c('estab_dest'='codigo_vigente')) |>
  rename(establecimiento_destino = nombre_oficial,
         nivel_de_atencion_destino = nivel_de_atencion) |> # renombro para evitar duplicidad de nombres
  left_join(dim_estab |> #junto los dataframes para obtener el est orig, comuna, nivel de atención
              select(codigo_vigente,
                     nombre_oficial, 
                     codigo_comuna,
                     nombre_comuna,
                     nivel_de_atencion), 
            by=c('estab_orig'='codigo_vigente')) |>
  rename(establecimiento_origen = nombre_oficial,
         nivel_de_atencion_origen = nivel_de_atencion) |> # renombro para evitar duplicidad de nombres
  left_join(dim_especialidades, by=c('presta_min'='codigosigte')) |> # combino con especialidades
  left_join( # combino con dataframe de abreviaciones
    import(ruta_dim_abreviaciones) |> 
      clean_names() |> 
      mutate(codigo_vigente = as.character(codigo_vigente)), 
    by=c('estab_dest'='codigo_vigente')) |>
  rename('abrev_destino'='abreviacion') |>
  select(-comuna) |>
  select(run,dv, sexo, fecha_nac, 
         estab_orig ,codigo_comuna, f_entrada, tipo_prest ,presta_min,sospecha_diag, 
         estab_dest,presta_est, 
         f_salida, c_salida) 


rm(egresos_bruto)

  
# 5. FACT PRODUCCIÓN REM DATOS ABIERTOS ----

# Descargar archivo

# url <- "https://repositoriodeis.minsal.cl/DatosAbiertos/REM/SERIE_REM_2024.zip" # Definir URL
# output_dir <- here('produccion')
# dir_create(output_dir)   # Crear carpeta declarada si no existe. # Uso 'here' para directorio relativo. Aquí se declara la carpeta de output.
# fzip <- tempfile(fileext = ".zip") # Generar un archivo ZIP en archivos temporales "temp".
# download.file(url, destfile = fzip, mode = "wb", quiet = TRUE) # Descargar archivo en fzip. 
# contenido <- unzip(fzip, list = TRUE) # Listar contenido del ZIP.
# archivo_target <- contenido$Name[grep("SerieA2024", contenido$Name)] # Identificar el archivo que necesitas
# if(length(archivo_target) == 0) stop("Archivo 'SerieA2024' no encontrado en el ZIP.") # Validar que exista
# unzip(fzip, files = archivo_target, exdir = output_dir) # Extraer SOLO el archivo deseado
# file_delete(fzip) # Limpiar: eliminar ZIP temporal
# cat("Archivo extraído exitosamente a:\n", file.path(output_dir, basename(archivo_target)), "\n")# Confirmación

# Importar 

rem_serieA <- import(ruta_fact_produccion_rem_deis) |> clean_names()

# 

columnas_a07_seccion_a <- c("mes","ano", "id_establecimiento", "codigo_prestacion",
                            "id_region", "id_region", "id_comuna", 
                            "col01", #total
                            'col19','col20','col21', #beneficiarios,hombres,mujeres
                            "col22", "col26")#menor15,mayor15

prestaciones_a07_seccion_a <- c('07020130', '07020230', '07020330', '07020331', '07020332', '07024219',
                                '07020500', '07020501', '07020600', '07020601', '07020700', '07020800',
                                '07020801', '07020900', '07020901', '07021000', '07021001', '07021100',
                                '07021101', '07021230', '07021300', '07021301', '07022000', '07022001',
                                '07021531', '07022132', '07022133', '07022134', '07021700', '07021800',
                                '07021801', '07021900', '07022130', '07022142', '07022143', '07022144',
                                '07022135', '07022136', '07022137', '07022700', '07022800', '07022900',
                                '07021701', '07023100', '07023200', '07023201', '07023202', '07023203',
                                '07023700', '07023701', '07023702', '07023703', '07024000', '07024001',
                                '07024200', '07030500', '07024201', '07024202', '07030501', '07030502')

columnas_a07_seccion_a1 <- c("mes", 'id_servicio' , "ano", "id_establecimiento", "codigo_prestacion",
                             "id_region", "id_region", "id_comuna", 
                             "col01",'col02', #pertinentes,pertinentes_aps
                             'col09','col10', #pertinentes_box, pertinentes_box_aps
                             'col13','col16','col17', #contrarreferencia_inicial,contrarreferencia_alta_menor15,mayor15
                             'col20','col21') #nsp_nuevas, nsp_controles




prestaciones_a07_seccion_a1 <- c('09600342','09600343','09600344','09600345','09600346','09600347',
                                 '09600348','09600349','09600350','09600351','09600352','09600353',
                                 '09600354','09600355','09600356','09600357','09600358','09600359',
                                 '09600360','09600361','09600362','09600363','09600364','09600365',
                                 '09600366','09600367','09600368','09600369','09600370','09600371',
                                 '09600372','09600373','09600374','09600375','09600376','09600377',
                                 '09600378','09600379','09600380','09600381','09600382','09600383',
                                 '09600384','09600385','09600386','09600387','09600388','09600389',
                                 '09600390','09600391','09600392','09600393','09600394','09600395',
                                 '09600396','09600397','09600398','09600399','09600400','09600401')


establecimientos <- c('113100','113130','113150','113180','113181')

produccion_a07_seccion_a <- rem_serieA |>
  filter(id_servicio == 13, codigo_prestacion %in% prestaciones_a07_seccion_a, id_establecimiento %in% establecimientos) |>
  select(columnas_a07_seccion_a) |>
  rename(
    total_consultas = col01,
    total_beneficiarios = col19,
    sexo_hombres = col20,
    sexo_mujeres = col21,
    consultas_menores15 = col22,
    consultas_mayores = col26
  ) |>
  left_join(
    dim_especialidades |> select(id_esp_seccion_a,codigosigte), 
    by=c('codigo_prestacion' ='id_esp_seccion_a' )
  )

produccion_a07_seccion_a1 <- rem_serieA |>
  filter(id_servicio == 13, codigo_prestacion %in% prestaciones_a07_seccion_a1, id_establecimiento %in% establecimientos) |>
  select(columnas_a07_seccion_a1) |>
  rename(
    pertinentes = col01,
    pertinentes_aps = col02,
    pertinentes_box = col09,
    pertinentes_box_aps = col10,
    cr_inicial = col13,
    cr_alta_menor15 = col16,
    cr_alta_mayor15 = col17,
    nsp_nuevas = col20,
    nsp_controles = col21
  ) |>
  left_join(
    dim_especialidades |> select(id_esp_seccion_b,codigosigte), 
    by=c('codigo_prestacion' ='id_esp_seccion_b' )
  ) 

fact_produccion_rem <- produccion_a07_seccion_a |>
  left_join(produccion_a07_seccion_a1, by = c('id_establecimiento','codigosigte','mes','ano','id_region','id_comuna'))

rm(produccion_a07_seccion_a,produccion_a07_seccion_a1,produccion_combinada,produccion_da, rem_serieA)
rm(columnas_a07_seccion_a,columnas_a07_seccion_a1,establecimientos,prestaciones_a07_seccion_a,prestaciones_a07_seccion_a1)

fact_produccion_rem_consultas <- fact_produccion_rem |>
  mutate(id_establecimiento = as.character(id_establecimiento))

fact_mapa_derivacion_trakcare <- import(ruta_fact_mapa_derivacion_trakcare,encoding = 'Latin-1') |> clean_names() 
