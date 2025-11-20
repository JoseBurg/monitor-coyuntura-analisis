library(openxlsx)

source("functions.R")


datos_coyuntura <- createWorkbook()
addWorksheet(datos_coyuntura, "ipp-manufactura")
addWorksheet(datos_coyuntura, "ipp-servicio")
addWorksheet(datos_coyuntura, "imae")
addWorksheet(datos_coyuntura, "ipc-general")

writeData(datos_coyuntura, "ipp-manufactura", get_ipp())
writeData(datos_coyuntura, "ipp-servicio", get_ipp(FALSE))
writeData(datos_coyuntura, "imae", get_imae())
writeData(datos_coyuntura, "ipc-general", databcrd::get_ipc_data(desagregacion = "general"))

saveWorkbook(datos_coyuntura, file = "data-coyuntura.xlsx", overwrite = TRUE)

