pacman::p_load(
  tidyverse, #manejo de datos
  here,       # directorios relativos
  janitor,    # limpiar dataframes
  lubridate,  # trabajar con fechas
  summarytools,
  rio,
  ggthemes,
  ggsci,
  readxl,
  zoo,
  scales,
  fastmap,
  shiny)

# Cargar los datos
df_demanda_inicial <- read_excel("demanda_inicial_2024.xlsx")
df_cubo <- read_excel("cubo.xlsx")

# UI
iu <- fluidPage(
  titlePanel("Evolución de Demanda y Producción"),
  sidebarLayout(
    sidebarPanel(
      selectInput("establecimiento", "Selecciona Establecimiento:", 
                  choices = unique(df_demanda_inicial$abreviacion)),
      selectInput("especialidad", "Selecciona Especialidad:", 
                  choices = unique(df_demanda_inicial$desc_esp))
    ),
    mainPanel(
      plotOutput("grafico")
    )
  )
)

# Servidor
servidor <- function(input, output, session) {
  output$grafico <- renderPlot({
    demanda_inicial <- df_demanda_inicial %>%
      filter(abreviacion == input$establecimiento, desc_esp == input$especialidad) %>%
      summarise(valor_inicial = sum(as.numeric(demanda_acumulada), na.rm = TRUE)) %>%
      pull(valor_inicial)
    
    df_cubo_filtrado <- df_cubo %>%
      filter(abreviacion == input$establecimiento, desc_esp == input$especialidad) %>%
      mutate(fecha = ymd(paste(ano, mes, "01", sep = "-"))) %>%
      arrange(fecha) %>%
      mutate(demanda_acumulada = demanda_inicial + cumsum(demanda_mensual - egresos_mensual))
    
    ggplot(df_cubo_filtrado, aes(x = fecha)) +
      geom_area(aes(y = demanda_acumulada, fill = "Demanda acumulada"), alpha = 0.4) +
      geom_line(aes(y = demanda_mensual, color = "Demanda mensual"), size = 1) +
      geom_line(aes(y = prog_mensual, color = "Programación mensual"), size = 1) +
      geom_line(aes(y = egresos_mensual, color = "Egresos mensual"), size = 1) +
      scale_y_continuous(
        name = "Indicadores",
        labels = scales::comma,
        sec.axis = sec_axis(~ ., name = "Demanda acumulada", labels = scales::comma)
      ) +
      labs(
        title = paste("Evolución de la oferta, demanda y producción de", input$especialidad),
        subtitle = paste("en", input$establecimiento),
        x = "Fecha",
        y = "Cantidad",
        color = "Indicadores",
        fill = "Demanda Acumulada"
      ) +
      theme_minimal()
  })
}

# Ejecutar app
shinyApp(ui = iu, server = servidor)

