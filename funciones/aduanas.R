get_dga_files <- function(type = c("importacion", "exportacion")) {
  type <- rlang::arg_match(type)
  url <- "https://www.aduanas.gob.do/umbraco/api/searcher/getpageofdocuments"
  
  server_response <- httr2::request(url) |>
    httr2::req_url_query(
      id = 3442,
      year = "",
      month = "",
      category = type,
      name = ""
    ) |>
    httr2::req_perform()
  
  result <- httr2::resp_body_json(server_response)
  
  result[[1]]$documents |>
    purrr::map(dplyr::as_tibble) |>
    dplyr::bind_rows() |>
    dplyr::select(name = documentName, description = documentDescription, file_url = documentFile ) |>
    dplyr::mutate(
      file_url = paste0("https://www.aduanas.gob.do", file_url),
      name = stringr::str_to_title(name)
    )
}

#  Funciones para descargar importaciones


# Función para extraer los nombres de las columnas de fechas --------------

clean_names <- function(raw_data){
  top_fila <- dim(raw_data)[2]
  year_ref <- raw_data[2, c(1:top_fila)] |>
    tidyr::pivot_longer(
      everything(),
      names_to = "ref",
      values_to = "year") |> 
    tidyr::fill(year)
  
  meses <- raw_data[4, c(1:top_fila)] |>
    tidyr::pivot_longer(
      everything(),
      names_to = "ref",
      values_to = "mes")
  
  year_ref |>  
    dplyr::left_join(meses) |> 
    dplyr::select(-ref) |> 
    dplyr::mutate(periodo = paste0(year, " ", mes)) |> 
    suppressMessages()
}

# Para homogenizar las fechas ------------------------
clean_date <- function(fecha){
  lubridate::make_date(
    stringr::str_extract(fecha, "[0-9]{4}"), # year
    databcrd::crear_mes(                     # Mes
      stringr::str_extract(stringr::str_remove(fecha, "x"), "(?<=_).*"),"text_to_number"),
    "01"                                     # dia
  )
}

dga_destino <- function(data){
  
  nuevos_nombres <- clean_names(data)
  
  data |>
    setNames(nuevos_nombres$periodo) |>
    dplyr::slice(-(1:4)) |> 
    janitor::clean_names() |>
    na.omit() |> 
    dplyr::mutate(
      capitulo_na = ifelse(
        is.na(capitulo_na),
        "Total", capitulo_na)) |>
    tidyr::fill(destino_economico_na) |>
    dplyr::filter(!stringr::str_detect(destino_economico_na, "TOTAL")) |> 
    tidyr::pivot_longer(
      cols = -c(destino_economico_na:capitulo_na),
      names_to = "fecha",
      values_to = "importaciones") |> 
    dplyr::filter(!stringr::str_detect(fecha, "na|total")) |> 
    dplyr::rename("destino_economico" = destino_economico_na) |> 
    dplyr::mutate(
      fecha = clean_date(fecha),
      importaciones = as.numeric(importaciones)) |> 
    dplyr::select(fecha, destino_economico, capitulo = capitulo_na, importaciones) |> 
    suppressMessages()
}


dga_regimen <- function(data){
  nuevos_nombres <- clean_names(data)
  
  data |>
    setNames(nuevos_nombres$periodo) |>
    dplyr::slice(-(1:4)) |> 
    janitor::clean_names() |>
    na.omit()  |>
    tidyr::pivot_longer(
      cols = -dplyr::starts_with("regimen"),
      names_to = "fecha",
      values_to = "importaciones") |> 
    dplyr::filter(!stringr::str_detect(fecha, "na|total")) |> 
    dplyr::rename("regimen" = regimen_na) |>
    dplyr::mutate(
      fecha = clean_date(fecha),
      importaciones = as.numeric(importaciones)) |> 
    dplyr::relocate(fecha) |> 
    suppressMessages()
}


get_data_dga <- function(categoria = "importacion", by = "destino"){
  checkmate::assert_choice(categoria, c("importacion", "exportacion"))
  checkmate::assert_choice(by, c("destino", "regimen"))
  
  by = switch(by, destino = "Destino Económico", regimen = "Régimen")
  
  dga <- get_dga_files(categoria) |> 
    dplyr::filter(stringr::str_detect(name, by))
  
  url <- dga$file_url
  
  temp_path <- base::tempfile(pattern = "", fileext = ".xlsx")
  utils::download.file(url, temp_path, quiet = TRUE, mode = "wb")
  
  raw_data <- readxl::read_excel(path = temp_path, skip = 3) |> 
    suppressMessages()
  
  if (by == "Destino Económico") {
    dga_destino(raw_data) |> 
      dplyr::rename({{ categoria }} := importaciones)
  } else {
    dga_regimen(raw_data) |> 
      dplyr::rename({{ categoria }} := importaciones) |> 
      tidyr::pivot_wider(id_cols = fecha, names_from = regimen, values_from = {{ categoria }})
  }
  
}








