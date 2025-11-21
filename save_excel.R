library(openxlsx)

source("functions.R")


datos_ipp <- get_ipp()[,c(1, 4)] |> 
  dplyr::bind_cols(
    get_ipp(FALSE)[,4]
  )

full_datos <- get_imae() |> 
  dplyr::select(periodo = fecha, imae = indice_original) |> 
  dplyr::left_join(datos_ipp) |> 
  suppressMessages()

datos_coyuntura <- createWorkbook()
addWorksheet(datos_coyuntura, "datos")

writeData(datos_coyuntura, "datos", full_datos)

saveWorkbook(datos_coyuntura, file = "data-coyuntura.xlsx", overwrite = TRUE)

