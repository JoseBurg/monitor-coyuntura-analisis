get_transacciones_tc <- function(variable = "Salario"){
  url <- "https://cdn.bancentral.gov.do/documents/sistema-de-pagos/estadisticas/documents/puntos_de_venta.xlsx?v=1782847514489"

  temp_path <- base::tempfile(pattern = "", fileext = ".xlsx")
  utils::download.file(url, temp_path, quiet = TRUE, mode = "wb")
  
  names <- c(
    "draft", "year", "mes", "l_td_vol", "l_td_val", "l_tc_vol", "l_tc_val",
    "l_tp_vol", "l_tp_val", "l_tsg_vol", "l_tsg_val", "l_tot_vol", "l_tot_val",
    "i_td_vol", "i_td_val", "i_tc_vol", "i_tc_val", "i_tp_vol", "i_tp_val",
    "i_tot_vol", "i_tot_val"
  )

  readxl::read_excel(path = temp_path, skip = 24, col_names = FALSE) |> 
    suppressMessages() |> 
    suppressWarnings() |> 
    setNames(names) |> 
    tidyr::fill(year) |> 
    dplyr::select(-draft) |> 
    na.omit() |> 
    dplyr::mutate(
      dplyr::across(-mes, as.numeric),
      mes = stringr::str_remove_all(mes, "[0-9]+| "), 
      mes = databcrd::crear_mes(mes, type = "text_to_number")) |> 
    dplyr::mutate(
      periodo = lubridate::make_date(year, mes, "01"),
      .before = year) |> 
    dplyr::select(
      periodo, l_tc_val, i_tc_val,
      l_td_val, i_td_val)
  
}





  
