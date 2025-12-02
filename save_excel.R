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
openxlsx::freezePane(datos_coyuntura, "datos",  firstRow = TRUE, firstCol = TRUE)
style_col_names <- openxlsx::createStyle(
    textDecoration = "bold", 
    fgFill = "#ced4da",
    fontSize = 12,
    halign = "center",
    valign = "center")

dime_data <- dim(full_datos)

openxlsx::addStyle(datos_coyuntura, "datos", style = style_col_names, rows = 1, cols = 1:dim_data[2])


writeData(datos_coyuntura, "datos", full_datos)

saveWorkbook(datos_coyuntura, file = "data-coyuntura.xlsx", overwrite = TRUE)

