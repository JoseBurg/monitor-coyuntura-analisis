library(openxlsx)

source("functions.R")


datos_coyuntura <- createWorkbook()
addWorksheet(datos_coyuntura, "ipp-manufactura")
addWorksheet(datos_coyuntura, "ipp-servicio")
addWorksheet(datos_coyuntura, "imae")

writeData(datos_coyuntura, "ipp-manufactura", get_ipp())
writeData(datos_coyuntura, "ipp-servicio", get_ipp(FALSE))
writeData(datos_coyuntura, "imae", get_imae())

saveWorkbook(datos_coyuntura, file = "data-coyuntura.xlsx", overwrite = TRUE)

