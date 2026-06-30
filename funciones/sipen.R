get_datos_sipen <- function(variable = "Salario"){
  variable = stringr::str_to_sentence(variable)
  
  checkmate::assert_choice(variable, 
    c("Afiliados", "Cotizantes", "Patrimonio", "Pensiones", 
      "Recaudación", "Rentabilidad", "Salario", "Traspasos"))
  
  sipen <- rvest::read_html("https://sipen.gob.do/estadisticas/estadistica-previsional/estadisticas-previsionales")
  
  nodos <- sipen |> 
    rvest::html_elements("a")
  
  links <- tibble::tibble(
    texto = rvest::html_text2(nodos),
    href  = rvest::html_attr(nodos, "href")) |> 
    dplyr::filter(stringr::str_detect(href, ".xlsx")) |> 
    dplyr::mutate(
      texto = stringr::str_replace_all(texto, "[\r\n\t]", ""),
      texto = stringr::str_squish(texto),
      texto = stringr::str_remove(texto, "^\\d{2}/\\d{2}/\\d{4}\\s+"),
      texto = stringr::str_remove(texto, "\\s*\\([^)]+\\)$")) |> 
    dplyr::rename("variables" = texto, "url" = href)
  
  url <- links |> 
    dplyr::filter(variables == variable) |> 
    dplyr::pull(url)
  
  temp_path <- base::tempfile(pattern = "", fileext = ".xlsx")
  utils::download.file(url, temp_path, quiet = TRUE, mode = "wb")
  
  readxl::read_excel(path = temp_path, skip = 3) |> 
    janitor::clean_names() |> 
    na.omit()
}


