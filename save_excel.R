library(openxlsx)
library(dplyr)

source("functions.R")


# Indices ----------------------------------------------------------------------
indices <- list(
  # IPC -----------------------------------------
  ipc <- databcrd::get_ipc_data(desagregacion = "subyacente") |> 
    dplyr::select(fecha, ipc_subyacente) |> 
      dplyr::left_join(
    databcrd::get_ipc_data(desagregacion = "general") |> 
      dplyr::select(fecha, ipc)
  ),
  
  # IPPs ------------------------------
  datos_ipp <- get_ipp()[,c(1, 4)] |> 
    dplyr::bind_cols(
      get_ipp(FALSE)[,4]
    ),       
  
  
  # IMAE ---------------------------------------
    imae <- get_imae() |>
      dplyr::select(fecha, imae = indice_original)
      
)


indices <- purrr::reduce(indices, left_join) |> 
  dplyr::mutate(
    dplyr::across(
      -fecha,
      list(
        vi = ~((.x/lag(.x, n = 12)) - 1) * 100,
        vm = ~((.x/lag(.x)) - 1) * 100
      )
    )
)


prestamos <- databcrd::get_prestamos_osd()

prestamos_sectores_consolidado <- prestamos |> 
  tidyr::pivot_wider(
    id_cols = fecha, 
    names_from = sectores, 
    values_from = consolidado
  )

prestamos_incidencia_total <- prestamos |>  
  summarise(
    across(
      c(mn, me, consolidado),
      sum
    ), 
  .by = fecha) |>
  mutate(
    across(-fecha,
     list(
       vm = ~((.x/lag(.x))-1) * 100,
       vi = ~((.x/lag(.x, n = 12))-1) * 100
     ))) |> 
  rowwise() |> 
  mutate(
    across(
      c(mn, me),
      list(
        incidencia = ~.x/consolidado
      )
    )
  )

prestamos_todos <- prestamos_sectores_consolidado |> 
  dplyr::left_join(prestamos_incidencia_total)


datos_coyuntura <- createWorkbook()
addWorksheet(datos_coyuntura, "índices")
addWorksheet(datos_coyuntura, "préstamos")
addWorksheet(datos_coyuntura, "IPC")

sheets <- list(
  "índices", "préstamos", "IPC"
)

purrr::map(
  sheets,
  \(x) openxlsx::freezePane(datos_coyuntura, x,  firstRow = TRUE, firstCol = TRUE)
)


writeData(datos_coyuntura, "índices", indices)
writeData(datos_coyuntura, "préstamos", prestamos_todos)
writeData(datos_coyuntura, "IPC", ipc_desagregacion)

saveWorkbook(datos_coyuntura, file = "data-coyuntura.xlsx", overwrite = TRUE)

