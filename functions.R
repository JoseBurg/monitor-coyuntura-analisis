get_ipp <- function(manufactura = TRUE){
  
  link <- "https://www.one.gob.do/datos-y-estadisticas/temas/estadisticas-economicas/precios/ipp"
  
  element <- ifelse(
    manufactura, 
    "td > a[href*='ipp-industrias-manufactureras_']",
    "td > a[href*='ipp-servicios_']"
  )
  
  url <- rvest::read_html(link) |>
    rvest::html_element(element) |>
    rvest::html_attr('href')
  
  link_base <- "https://www.one.gob.do" # <--- Usar la URL base del sitio
  url_descarga <- paste0(link_base, url) # <--- Usar paste0 para unir sin espacios
  
  file_path <- tempfile(fileext = ".xlsx")
  utils::download.file(url_descarga, file_path, mode = "wb", quiet = TRUE)
  
  name_ipp <- ifelse(manufactura, "_manufactura", "_servicio")
  
  data <- readxl::read_excel(
    file_path, sheet = 1, skip = ifelse(manufactura, 9, 10), 
    col_names = c(
      "year", "mes", 
      paste0("ipp", name_ipp), 
      "vm", "vcorrid", "vi")) |>
    tidyr::fill(year) |> 
    dplyr::mutate(
      dplyr::across(c(year, vi), as.numeric),
      mes = stringr::str_remove(mes, "R$"),
      fecha = seq(as.Date("2014-01-01"), 
                    by = "month", length.out = dplyr::n()),
      .before = year) |> 
    na.omit() |> 
    suppressWarnings()
  
  data
}



get_imae <- function (variaciones = TRUE) {
    checkmate::assert_logical(variaciones)
    url <- paste0("https://cdn.bancentral.gov.do/", "documents/estadisticas/sector-real/documents/imae_2018.xlsx")
    temp_path <- base::tempfile(pattern = "", fileext = ".xlsx")
    utils::download.file(url, temp_path, quiet = TRUE, mode = "wb")
    header_imae <- c("mes", "indice_original", "original_vi", 
        "origianl_va", "original_p12m", "indice_desestacionalizado", 
        "desestacionalizado_vm", "desestacionalizado_vi", "desestacionalizado_va", 
        "desestacionalizado_p12m", "indice_tc", "tc_vm", "tc_vi", 
        "tc_va", "tc_p12m")
    suppressMessages(imae <- readxl::read_excel(path = temp_path, 
        skip = 8, col_names = FALSE))
    imae <- dplyr::select(dplyr::mutate(dplyr::filter(stats::setNames(janitor::remove_empty(dplyr::select(janitor::clean_names(imae), 
        -1), which = "cols"), header_imae), !is.na(mes)), fecha = seq(as.Date("2007-01-01"), 
        by = "month", length.out = dplyr::n()), year = lubridate::year(fecha)), 
        fecha, year, mes, dplyr::everything())
    if (!variaciones) {
        imae <- dplyr::select(imae, fecha, year, mes, dplyr::contains("indice"))
    }
    imae
}



# Gráficos: ------------------------------------------------------------------------

# grafica_highcharts <- function(full_datos, variable_seleccionada) {
#   
#   # Validar variable
#   if (!variable_seleccionada %in% names(full_datos)) {
#     stop("La variable seleccionada no existe en el dataframe")
#   }
#   datos <- full_datos |>
#     dplyr::select(periodo, valor = all_of(variable_seleccionada))
#   
#   # Crear gráfico
#  highcharter::highchart() |>
#    highcharter::hc_add_series(
#       datos,
#       type = "line",
#       highcharter::hcaes(x = periodo, y = valor),
#       name = variable_seleccionada
#     ) |>
#     # highcharter::hc_title(text = paste("Serie:", variable_seleccionada)) |>
#     highcharter::hc_xAxis(
#       title = list(text = "fecha"),
#       type = "datetime",
#       labels = list(format = "{value:%b %Y}")
#     ) |>
#     highcharter::hc_yAxis(title = list(text = "Nivel"))
# 
# }



tabla_variaciones_html <- function(data, variable) {

  df <- data |>
    dplyr::select(periodo, valor = dplyr::all_of(variable)) |>
    dplyr::arrange(periodo) |>
    dplyr::mutate(
      periodo = format(periodo, format = "%b %Y"),
      mes_anterior   = lag(valor, 1),
      anio_anterior  = lag(valor, 12),
      var_mensual    = (valor / mes_anterior - 1),
      var_interanual = (valor / anio_anterior - 1),
      dplyr::across(
        dplyr::starts_with("var"),
        ~scales::percent(.x, accuracy = 0.01, suffix = " %")
      )
    )
  
  df <- tail(df, 2) |> 
    janitor::clean_names(case = "title")

  # Crear tabla en HTML con htmltools
  htmltools::tags$table(
    class = "table table-striped table-bordered table-sm",
    
    # Encabezados
    htmltools::tags$thead(
      htmltools::tags$tr(
        lapply(colnames(df), htmltools::tags$th)
      )
    ),
    
    # Cuerpo
    htmltools::tags$tbody(
      lapply(1:nrow(df), function(i) {
        htmltools::tags$tr(
          lapply(df[i, ], function(x) {
            htmltools::tags$td(ifelse(is.na(x), "", x))
          })
        )
      })
    )
  )
}




# Preparación de datos  ---------------------------------------------------
# Reporte::

full_datos <- read_excel("data-coyuntura.xlsx") |>
  mutate(
    across(-fecha, ~round(.x, 2)),
    periodo = as.Date(fecha)) |>
  filter(periodo >= "2018-01-01") |> 
  select(-fecha, -matches("_(vi|vm)$")) 

datos_long <- full_datos |> 
  tidyr::pivot_longer(
    cols = -periodo, 
    names_to = "variables",
    values_to = "indice"
  ) |> 
  group_by(variables) |> 
  mutate(
    vm = (indice/lag(indice)-1) * 100,
    vi = (indice/lag(indice, 12)-1) * 100, 
    across(c(vm, vi),
           ~round(., digits = 2)
           )
  )



# Funcion para graficas de lines en estilo long ---------------------------

plot_line <- function(datos,
                      medida = "indice",
                      visibles = "ipc",
                      type = "stock") {
  
  # Validar la medida
  if (!medida %in% c("indice", "vm", "vi")) {
    stop("La medida debe ser 'indice', 'vm' o 'vi'.")
  }
  
  hc <- highcharter::highchart(type = type)
  
  variables <- unique(datos$variables)
  
  for (v in variables) {
    
    datos_var <- datos |>
      dplyr::filter(variables == v)
    
    hc <- hc |>
      highcharter::hc_add_series(
        datos_var,
        type = "spline",
        highcharter::hcaes(
          x = periodo,
          y = .data[[medida]]
        ),
        name = v,
        visible = v %in% visibles
      )
  }
  
  hc <- hc |>
    highcharter::hc_xAxis(type = "datetime") |>
    highcharter::hc_yAxis(opposite = FALSE) |> 
    highcharter::hc_tooltip(shared = TRUE) |>
    highcharter::hc_legend(enabled = TRUE)
  
  # Configuración exclusiva para Highstock
  if (type == "stock") {
    
    hc <- hc |>
      highcharter::hc_rangeSelector(enabled = TRUE,
                                    buttons = list(),   
                                    inputEnabled = TRUE) |>
      highcharter::hc_scrollbar(enabled = FALSE) |> 
      highcharter::hc_navigator(
        enabled = FALSE,
        height = 30
      )
    
  }
  
  hc
}

# preparar_prestamos <- function(data){
#   prestamos |> 
#     # dplyr::group_by(fecha) |> 
#     dplyr::summarise(
#       dplyr::across(
#         c(mn, me, consolidado),
#         sum), .by = fecha) |> 
#     tidyr::pivot_longer(
#       cols = -fecha, 
#       names_to = "variables",
#       values_to = "montos") 
# }
# 
# preparar_prestamos(prestamos)
# 
# 
# 
# # 3. Función para construir el gráfico apilado dado un tipo de moneda
# grafico_apilado <- function(datos, variable = c("mn", "me", "consolidado"),
#                             titulo = NULL) {
#   variable <- match.arg(variable)
#   
#   datos_agg <- preparar_datos(datos, variable)
#   
#   titulo_default <- switch(variable,
#                            mn          = "Préstamos en Moneda Nacional por Sector",
#                            me          = "Préstamos en Moneda Extranjera por Sector",
#                            consolidado = "Préstamos Consolidados por Sector"
#   )
#   
#   highcharter::hchart(
#     datos_agg,
#     "column",
#     highcharter::hcaes(x = fecha, y = valor)
#   ) |>
#     highcharter::hc_chart(zoomType = "x") |>
#     highcharter::hc_plotOptions(column = list(stacking = "normal")) |>
#     highcharter::hc_title(text = titulo %||% titulo_default) |>
#     highcharter::hc_xAxis(title = list(text = "Año")) |>
#     highcharter::hc_yAxis(title = list(text = "Millones de RD$")) |>
#     highcharter::hc_tooltip(shared = FALSE, pointFormat = "<b>{series.name}</b>: {point.y:,.1f}") |>
#     highcharter::hc_legend(enabled = FALSE)
# }
# 




# Indicadores internacionales USA FRED
#quantmod::getSymbols(
#   Symbols =  "CORESTICKM159SFRBATL",
#    src = "FRED",
    # from = "2012-01-01",
    # to = "2025-10-01",
#    format = "xts",
#    periodicity = "monthly"
#)


#data_usa <- c("ipc_usa", "pib_usa")

# Instalar databcrd
# devtools::install_github("https://github.com/Johan-rosa/databcrd")
