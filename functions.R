
# Get-IPP -----------------------------------------------------------------

#' Descargar y procesar el Índice de Precios del Productor (IPP) desde la ONE
#'
#' Esta función automatiza la descarga y lectura del archivo más reciente del 
#' Índice de Precios del Productor (IPP) publicado por la Oficina Nacional de 
#' Estadística (ONE) de República Dominicana. Permite elegir entre el IPP de 
#' industrias manufactureras y el IPP de servicios. 
#'
#' La función:
#' - Identifica en la página oficial el enlace del archivo Excel más reciente.
#' - Descarga temporalmente el archivo.
#' - Lee y limpia los datos aplicando los nombres de columnas correspondientes.
#' - Construye una variable de fecha ("periodo") desde 2014-01 en adelante.
#'
#' @param manufactura Logical. Si `TRUE` (por defecto), descarga el IPP de 
#' industrias manufactureras. Si `FALSE`, descarga el IPP de servicios.
#'
#' @return Un data frame con las siguientes columnas:
#' \describe{
#'   \item{periodo}{Fecha en formato `Date` correspondiente a cada observación.}
#'   \item{year}{Año reportado en el archivo.}
#'   \item{mes}{Mes reportado (texto).}
#'   \item{ipp_manufactura / ipp_servicio}{Índice de Precios del Productor.}
#'   \item{vm}{Variación mensual.}
#'   \item{vcorrid}{Variación acumulada.}
#'   \item{vi}{Variación interanual.}
#' }
#'
#' @details
#' La función usa `rvest` para extraer dinámicamente el enlace más reciente según 
#' si se requiere el IPP de manufactura o servicios. Posteriormente utiliza 
#' `readxl` para leer el archivo y `dplyr`/`tidyr` para limpiar los datos.
#'
#' @examples
#' \dontrun{
#' # Obtener IPP manufactura
#' ipp_m <- get_ipp()
#'
#' # Obtener IPP servicios
#' ipp_s <- get_ipp(manufactura = FALSE)
#' }
#'
#' @export

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
      periodo = seq(as.Date("2014-01-01"), 
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



# Gráficos: ---------------------------------------------------------------

grafica_highcharts <- function(full_datos, variable_seleccionada) {
  
  # Validar variable
  if (!variable_seleccionada %in% names(full_datos)) {
    stop("La variable seleccionada no existe en el dataframe")
  }
  datos <- full_datos |>
    select(periodo, valor = all_of(variable_seleccionada))
  
  # Crear gráfico
 highcharter::highchart() |>
   highcharter::hc_add_series(
      datos,
      type = "line",
      highcharter::hcaes(x = periodo, y = valor),
      name = variable_seleccionada
    ) |>
    # highcharter::hc_title(text = paste("Serie:", variable_seleccionada)) |>
    highcharter::hc_xAxis(
      title = list(text = "Periodo"),
      type = "datetime",
      labels = list(format = "{value:%b %Y}")
    ) |>
    highcharter::hc_yAxis(title = list(text = "Nivel"))
  

}


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
# Instalar databcrd
# devtools::install_github("https://github.com/Johan-rosa/databcrd")
