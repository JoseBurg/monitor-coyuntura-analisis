# ============================================================
# FUNCIONES: VARIACIONES
# ============================================================
prestamos <- databcrd::get_prestamos_osd()

datos_totales <- prestamos |> 
  dplyr::summarise(
    dplyr::across(c(mn, me, consolidado), sum),
    .by = fecha
  ) |> 
  dplyr::arrange(fecha)

datos_montos_long <- datos_totales |> 
  tidyr::pivot_longer(
    cols = -fecha,
    names_to = "variables",
    values_to = "montos"
  )



preparar_variaciones <- function(datos_totales, fecha_inicio = "2020-01-01") {
  
  datos_totales |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(
      dplyr::across(
        c(mn, me, consolidado),
        list(
          var_mensual    = ~ (.x / dplyr::lag(.x, 1)  - 1) * 100,
          var_interanual = ~ (.x / dplyr::lag(.x, 12) - 1) * 100
        ),
        .names = "{.col}_{.fn}"
      )
    ) |>
    dplyr::select(fecha, dplyr::ends_with("_var_mensual"), dplyr::ends_with("_var_interanual")) |>
    tidyr::pivot_longer(
      cols = -fecha,
      names_to = "variable",
      values_to = "variacion"
    ) |>
    tidyr::separate(
      variable,
      into = c("variables", "tipo_variacion"),
      sep = "_var_"
    ) |>
    dplyr::mutate(tipo_variacion = paste0("var_", tipo_variacion)) |>
    dplyr::filter(fecha >= fecha_inicio)
  
}

grafico_variaciones <- function(datos_variaciones_long,
                                tipo = c("var_mensual", "var_interanual"),
                                titulo = NULL) {
  
  tipo <- match.arg(tipo)
  
  datos <- datos_variaciones_long |>
    dplyr::filter(tipo_variacion == tipo)
  
  titulo_default <- switch(tipo,
                           var_mensual    = "Variación Mensual de los Préstamos",
                           var_interanual = "Variación Interanual de los Préstamos"
  )
  
  highcharter::hchart(
    datos,
    "line",
    highcharter::hcaes(x = fecha, y = variacion, group = variables)
  ) |>
    highcharter::hc_chart(zoomType = "x") |>
    highcharter::hc_title(text = titulo %||% titulo_default) |>
    highcharter::hc_xAxis(type = "datetime", title = list(text = "Fecha")) |>
    highcharter::hc_yAxis(title = list(text = "%")) |>
    highcharter::hc_tooltip(shared = TRUE, valueDecimals = 1) |>
    highcharter::hc_exporting(enabled = FALSE)
  
}

# Uso:
datos_var <- preparar_variaciones(datos_totales, fecha_inicio = "2020-01-01")
grafico_variaciones(datos_var, tipo = "var_mensual")
grafico_variaciones(datos_var, tipo = "var_interanual")


# ============================================================
# FUNCIONES: INCIDENCIAS
# ============================================================

preparar_incidencias <- function(datos_totales, fecha_inicio = "2020-01-01") {
  
  datos_totales |>
    dplyr::arrange(fecha) |>
    dplyr::mutate(
      consolidado_lag12 = dplyr::lag(consolidado, 12),
      incidencia_mn = (mn - dplyr::lag(mn, 12)) / consolidado_lag12 * 100,
      incidencia_me = (me - dplyr::lag(me, 12)) / consolidado_lag12 * 100,
      var_interanual_consolidado = (consolidado / consolidado_lag12 - 1) * 100
    ) |>
    dplyr::select(fecha, incidencia_mn, incidencia_me, var_interanual_consolidado) |>
    dplyr::filter(fecha >= fecha_inicio)
  
}

grafico_incidencias <- function(datos_incidencias, titulo = "Incidencia de MN y ME en la Variación Interanual") {
  
  datos_long <- datos_incidencias |>
    dplyr::select(fecha, incidencia_mn, incidencia_me) |>
    tidyr::pivot_longer(
      cols = -fecha,
      names_to = "variables",
      values_to = "incidencia"
    ) |>
    dplyr::mutate(variables = stringr::str_remove(variables, "incidencia_"))
  
  highcharter::highchart() |>
    highcharter::hc_chart(zoomType = "x") |>
    highcharter::hc_xAxis(categories = format(datos_incidencias$fecha, "%b-%y")) |>
    highcharter::hc_yAxis(title = list(text = "%")) |>
    highcharter::hc_add_series(
      name = "Moneda Nacional",
      data = datos_long$incidencia[datos_long$variables == "mn"],
      type = "column",
      stack = "incidencia"
    ) |>
    highcharter::hc_add_series(
      name = "Moneda Extranjera",
      data = datos_long$incidencia[datos_long$variables == "me"],
      type = "column",
      stack = "incidencia"
    ) |>
    highcharter::hc_add_series(
      name = "Consolidado (Var. Interanual)",
      data = datos_incidencias$var_interanual_consolidado,
      type = "line",
      color = "black",
      marker = list(enabled = FALSE)
    ) |>
    highcharter::hc_plotOptions(column = list(stacking = "normal")) |>
    highcharter::hc_title(text = titulo) |>
    highcharter::hc_tooltip(shared = TRUE, valueDecimals = 1) |>
    highcharter::hc_exporting(enabled = TRUE)
  
}

# Uso:
datos_inc <- preparar_incidencias(datos_totales, fecha_inicio = "2020-01-01")
grafico_incidencias(datos_inc)
