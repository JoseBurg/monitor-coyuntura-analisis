devtools::load_all()

library(glue)
library(databcrd)
library(tidyr)
library(dplyr)
library(mschart)
library(officer)

#' Creates a few charts for testing
#' plot_tc_vd
#' plot_tc_margen
#' ipc_ms_chart
#' tasas_ms_chart
source("inst/test_plots.R", verbose = FALSE, echo = FALSE)

# Template and properties ---------------------------------------------------

color <- "azul"

slide_content <- list(
  portada = list(
    layout = "portada"
  ),
  seccion = list(
    layout = glue::glue("{color}_seccion"),
    content = list(
      titulo = "Asuntos sin importancia"
    )
  ),
  contenido = list(
    layout = glue("{color}_dos_columnas"),
    content = list(
      titulo = "Resultados inflación",
      titulo_izquierda = "Gráficos de prueba",
      contenido_izquierda = ipc2_ms_chart,
      titulo_derecha = "Tasas de interés",
      contenido_derecha = tasas_ms_chart,
      fuente = "BCRD"
    )
  ),
  contenido = list(
    layout = glue("{color}_dos_columnas"),
    content = list(
      titulo = "Tipo de cambio",
      titulo_izquierda = "Depreciación acumulada 2025",
      contenido_izquierda = plot_tc_vd,
      titulo_derecha = "Margen cambiario",
      contenido_derecha = plot_tc_margen,
      fuente = "BCRD"
    )
  )
)


presentation <- write_presentation(slide_content)

print(presentation, target = "inst/test.pptx")

xopen::xopen("inst/test.pptx")


# template <- get_bcrd_template()
#
# template$slideLayouts$names() |> unname()
#
# officer::layout_properties(x = template, layout = "azul_dos_columnas")


