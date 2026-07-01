library(shyni)



selectizeInput(
  "variables",
  "Indicadores:",
  choices = sort(unique(datos_long$variables)),
  multiple = TRUE,
  selected = "ipc"
)

radioButtons(
  "serie",
  "Mostrar:",
  choices = c(
    "Índice" = "indice",
    "Variación mensual" = "vm",
    "Variación interanual" = "vi"
  ),
  selected = "indice"
)



renderHighchart({
  
  datos_filtrados <- datos_long |>
    dplyr::filter(
      variables %in% input$variables
    )
  
  highcharter::hchart(
    datos_filtrados,
    "line",
    highcharter::hcaes(
      x = periodo,
      y = .data[[input$serie]],
      group = variables
    )
  ) |>
    highcharter::hc_xAxis(type = "datetime") |>
    highcharter::hc_tooltip(shared = TRUE)
})


datos_filtrados <- datos_long |>
  dplyr::filter(variables %in% c("ipc", "imae"))

highcharter::hchart(
  datos_filtrados,
  "line",
  highcharter::hcaes(
    x = periodo,
    y = indice,
    group = variables
  )
) |>
  highcharter::hc_xAxis(type = "datetime") |>
  highcharter::hc_tooltip(shared = TRUE)




