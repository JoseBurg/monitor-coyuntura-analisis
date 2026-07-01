library(dplyr)
library(tidyr)
library(highcharter)

# ============================================================
# 1. DATOS BASE: total por fecha (wide y long)
# ============================================================

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

# ============================================================
# 2. VARIACIONES: mensual e interanual
# ============================================================

datos_variaciones <- datos_totales |> 
  dplyr::arrange(fecha) |> 
  dplyr::mutate(
    dplyr::across(
      c(mn, me, consolidado),
      list(
        var_mensual   = ~ (.x / dplyr::lag(.x, 1)  - 1) * 100,
        var_interanual = ~ (.x / dplyr::lag(.x, 12) - 1) * 100
      ),
      .names = "{.col}_{.fn}"
    )
  )

# Versión larga de las variaciones (útil para graficar)
datos_variaciones_long <- datos_variaciones |> 
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
  dplyr::mutate(tipo_variacion = paste0("var_", tipo_variacion))

# ============================================================
# 3. INCIDENCIAS: contribución de mn y me a la variación
#    interanual del consolidado
#    Incidencia_i = (valor_i,t - valor_i,t-12) / consolidado_t-12 * 100
# ============================================================

datos_incidencias <- datos_totales |> 
  dplyr::arrange(fecha) |> 
  dplyr::mutate(
    consolidado_lag12 = dplyr::lag(consolidado, 12),
    incidencia_mn = (mn - dplyr::lag(mn, 12)) / consolidado_lag12 * 100,
    incidencia_me = (me - dplyr::lag(me, 12)) / consolidado_lag12 * 100,
    var_interanual_consolidado = (consolidado / consolidado_lag12 - 1) * 100
  ) |> 
  dplyr::select(fecha, incidencia_mn, incidencia_me, var_interanual_consolidado)

datos_incidencias_long <- datos_incidencias |> 
  dplyr::select(fecha, incidencia_mn, incidencia_me) |> 
  tidyr::pivot_longer(
    cols = -fecha,
    names_to = "variables",
    values_to = "incidencia"
  ) |> 
  dplyr::mutate(variables = stringr::str_remove(variables, "incidencia_"))

# ============================================================
# 4. GRÁFICO COMBINADO: barras apiladas (mn + me) + línea (consolidado)
# ============================================================

grafico_montos_combinado <- function(datos_totales, fecha_inicio = "2020-01-01") {
  
  datos <- datos_totales |> 
    dplyr::filter(fecha >= fecha_inicio)
  
  highchart() |> 
    hc_chart(zoomType = "x") |> 
    hc_xAxis(categories = format(datos$fecha, "%b-%y")) |> 
    hc_yAxis_multiples(
      list(title = list(text = "Millones de RD$")),
      list(title = list(text = "Millones de RD$"), opposite = TRUE)
    ) |> 
    hc_add_series(
      name = "Moneda Nacional",
      data = datos$mn,
      type = "column",
      stack = "montos",
      yAxis = 0
    ) |> 
    hc_add_series(
      name = "Moneda Extranjera",
      data = datos$me,
      type = "column",
      stack = "montos",
      yAxis = 0
    ) |> 
    hc_add_series(
      name = "Consolidado",
      data = datos$consolidado,
      type = "line",
      yAxis = 1,
      color = "black",
      marker = list(enabled = FALSE)
    ) |> 
    hc_plotOptions(column = list(stacking = "normal")) |> 
    hc_title(text = "Préstamos por Tipo de Moneda") |> 
    hc_tooltip(shared = TRUE, valueDecimals = 1)
}

# Uso:
grafico_montos_combinado(datos_totales, fecha_inicio = "2020-01-01")
