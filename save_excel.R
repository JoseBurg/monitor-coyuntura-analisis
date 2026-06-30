library(openxlsx)
library(dplyr)

source("functions.R")
source("funciones/sipen.R", encoding = "UTF-8")
source("funciones/tarjetas.R", encoding = "UTF-8")



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


#                                           IPC---------------------------------------------------
ipc <- list(
  general = databcrd::get_ipc_data(desagregacion = "general") |> 
    dplyr::select(fecha, ipc),
  
  subyacente = databcrd::get_ipc_data(desagregacion = "subyacente") |> 
    dplyr::select(fecha, ipc_subyacente),
  
  grupos = databcrd::get_ipc_data(desagregacion = "grupos") |> 
    dplyr::select(-c(dplyr::contains("vm"), year, mes))
  
)


ipc_desagregacion <- purrr::reduce(ipc, dplyr::left_join) |>
  dplyr::filter(fecha >= "2000-01-01")


# Préstamos -----------------------------------------------------

prestamos <- databcrd::get_prestamos_osd() 


prestamos_sectores_consolidado <- prestamos |> 
  tidyr::pivot_wider(
    id_cols = fecha, 
    names_from = sectores, 
    values_from = consolidado
  )

prestamos_variaciones <- prestamos |>  
  filter(!stringr::str_detect(sectores, "TARJETAS DE CRÉDITO|OTROS PRÉSTAMOS DE CONSUMO")) |> 
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
     )))

prestamos_todos <- prestamos_sectores_consolidado |> 
  dplyr::left_join(prestamos_variaciones)



# Datos de sipen ----------------------------------------------------------
sipen <- get_datos_sipen("salario") |> 
  dplyr::left_join(
    get_datos_sipen("cotizantes"), 
    by = c("mes" = "corte")
  ) |> dplyr::mutate(
    mes = as.Date(as.numeric(mes), origin = "1899-12-30")
  )


# Guardando datos en excel ------------------------------------------------
datos_coyuntura <- createWorkbook()
datos <- list(
  "índices"   = indices,
  "préstamos" = prestamos_todos,
  "IPC"       = ipc_desagregacion,
  "sipen"     = sipen, 
  "transacciones" = get_transacciones_tc()
)

purrr::iwalk(datos, \(df, nombre) {
  addWorksheet(datos_coyuntura, nombre)
  freezePane(datos_coyuntura, nombre, firstRow = TRUE, firstCol = TRUE)
  writeData(datos_coyuntura, nombre, df)
})


saveWorkbook(datos_coyuntura, file = "data-coyuntura.xlsx", overwrite = TRUE)

