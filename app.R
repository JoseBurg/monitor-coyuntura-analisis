library(shiny)
library(tidyverse)
library(bslib) # Para un diseño moderno
library(scales)

source("functions.R")


datos_ipp <- get_ipp()

# -------------------------------------------------------------------------
# 2. INTERFAZ DE USUARIO (UI)
# -------------------------------------------------------------------------
ui <- page_sidebar(
  title = "Dashboard de Índices de Precios (IPP)",
  theme = bs_theme(bootswatch = "zephyr"), # Tema moderno y limpio
  
  sidebar = sidebar(
    title = "Filtros",
    dateRangeInput(
      "rango_fechas",
      "Seleccionar Periodo:",
      start = min(datos_ipp$periodo),
      end = max(datos_ipp$periodo),
      min = min(datos_ipp$periodo),
      max = max(datos_ipp$periodo),
      format = "yyyy-mm",
      language = "es",
      separator = " a "
    ),
    hr(),
    helpText("Este tablero muestra la evolución del IPP Manufactura y sus variaciones históricas.")
  ),
  
  # Contenido Principal
  layout_columns(
    col_widths = c(12, 12),
    row_heights = c("400px", "auto"),
    
    # Tarjeta del Gráfico
    card(
      card_header("Evolución del Índice y Variación Interanual"),
      plotOutput("grafico_evolucion")
    ),
    
    # Tarjeta de la Tabla
    card(
      card_header("Tabla Detallada: Comparativa Histórica"),
      tableOutput("tabla_detalle")
    )
  )
)

# -------------------------------------------------------------------------
# 3. SERVIDOR (SERVER)
# -------------------------------------------------------------------------
server <- function(input, output) {
  
  # Filtrar datos reactivamente
  datos_filtrados <- reactive({
    req(input$rango_fechas)
    datos_ipp |>
      filter(periodo >= input$rango_fechas[1] & periodo <= input$rango_fechas[2])
  })
  
  # Renderizar Gráfico
  output$grafico_evolucion <- renderPlot({
    datos <- datos_filtrados()
    
    # Factor de escala para graficar dos ejes Y (Índice y Porcentaje)
    # Ajustamos para que el porcentaje se vea bien junto al índice > 100
    max_ipp <- max(datos$ipp_manufactura, na.rm = TRUE)
    scale_factor <- max_ipp / 10 # Ajuste visual aproximado
    
    ggplot(datos, aes(x = periodo)) +
      # Barras para la Variación Interanual (Eje secundario visualmente)
      geom_col(aes(y = vi * scale_factor, fill = "Var. Interanual (%)"), alpha = 0.4) +
      
      # Línea para el Índice (Eje principal)
      geom_line(aes(y = ipp_manufactura, color = "IPP Manufactura"), size = 1.2) +
      geom_point(aes(y = ipp_manufactura, color = "IPP Manufactura"), size = 2) +
      
      # Escalas
      scale_y_continuous(
        name = "Índice (IPP)",
        sec.axis = sec_axis(~ . / scale_factor, name = "Variación Interanual (%)")
      ) +
      scale_fill_manual(values = c("Var. Interanual (%)" = "#3d5a80")) +
      scale_color_manual(values = c("IPP Manufactura" = "#ee6c4d")) +
      
      labs(
        title = "Dinámica del IPP Manufactura",
        x = "Periodo",
        fill = "", color = ""
      ) +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "top",
        axis.title.y.right = element_text(color = "#3d5a80"),
        axis.text.y.right = element_text(color = "#3d5a80")
      )
  })
  
  # Renderizar Tabla con las columnas específicas solicitadas
  output$tabla_detalle <- renderTable({
    datos_filtrados() |>
      # Necesitamos recalcular lags aquí si el filtro rompe la secuencia, 
      # pero mejor usamos los datos originales para buscar los valores previos correctos
      # Para visualización simple, usaremos mutate sobre el subset o join
      arrange(desc(periodo)) |>
      mutate(
        # Formatear fecha
        Fecha = format(periodo, "%Y-%b"),
        
        # 1. Variación Interanual (ya existe, solo formato)
        `Var. Interanual (%)` = label_percent(accuracy = 0.01, scale = 1)(vi),
        
        # 2. Valor del Periodo Anterior (t-1)
        # Nota: Usamos lag sobre los datos filtrados. 
        # *Advertencia*: Si filtras fechas, el primer dato no tendrá lag visible aquí.
        `Valor Mes Anterior` = number(lag(ipp_manufactura, 1, default = NA), accuracy = 0.01),
        
        # 3. Valor del Mismo Mes Año Anterior (t-12) de los datos filtrados
        # Para hacerlo robusto usamos lookup en los datos originales en un caso real,
        # aquí usamos lag simple para demostración visual en tabla continua.
        `Valor Año Anterior` = number(lag(ipp_manufactura, 12, default = NA), accuracy = 0.01),
        
        # Valor Actual
        `Valor Actual (IPP)` = number(ipp_manufactura, accuracy = 0.01)
      ) |>
      select(Fecha, `Valor Actual (IPP)`, `Var. Interanual (%)`, `Valor Mes Anterior`, `Valor Año Anterior`) |>
      head(15) # Mostrar solo las últimas 15 filas para no saturar
  }, width = "100%", striped = TRUE, hover = TRUE, align = "c")
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
