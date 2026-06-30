
#' Read BCRD Power Point template
#'
#' @return a `rpptx` object
#' @export
#'
#' @examples get_bcrd_tempalte()
get_bcrd_template <- function() {
  officer::read_pptx(
    system.file("template_bcrd.pptx", package = "bcrd.power.point")
  )
}

#' BRCD template layouts
#'
#' List the available layouts in the BCRD template.
#'
#' @return character vector
#' @export
get_template_layouts <- function() {
  document <- get_bcrd_template()
  document$slideLayouts$names() |>
    unname()
}

#' BCRD layout properties
#'
#' Check the properties of all or specific layouts in the corporate template.
#'
#' @param selected_layout layout to check. NULL by default to display the
#' properties for all layouts
#'
#' @return a data frame
#' @export
#'
#' @examples get_layout_properties(" Title Slide")
get_layout_propierties <- function(selected_layout = NULL) {
  template <- get_bcrd_template()

  layouts <- get_template_layouts()

  properties <- purrr::map(
    layouts,
    ~officer::layout_properties(template, .x)
  ) |>
    purrr::list_rbind() |>
    dplyr::mutate(
      ph_level = stringr::str_replace_all(tolower(ph_label), " ", "_")) |>
    dplyr::select(
      master_name,
      layout = name,
      ph_level,
      ph_label
    )

  if (!is.null(selected_layout)) {
    checkmate::assert_choice(selected_layout, layouts)
    return(dplyr::filter(properties, layout == selected_layout))
  }

  properties
}

#' Write content to Power Point Slide
#'
#' Write content into an existing slide or new slide just providing the layout
#' and the property to fill.
#'
#' @param ppt a rpptx object
#' @param layout string with the slide loayout
#' @param content list with the content to write. Each element of the list
#' should be named with the `ph_level` of the property
#' @param in_new_slide indicate if the content should be written in the last
#' slide
#' or in a a new one
#'
#' @return a rpptx object
#' @export
write_slide <- function(ppt, layout, content, in_new_slide = TRUE) {
  properties <- get_layout_propierties(layout)

  checkmate::assert_subset(names(content), properties$ph_level)

  properties_list <- purrr::set_names(properties$ph_label, properties$ph_level)

  if (in_new_slide) {
    ppt <- officer::add_slide(
      ppt,
      layout = layout,
      master = "bc-theme"
    )
  }

  purrr::reduce(
    names(content),
    \(ppt, ph_level) {
      officer::ph_with(
        ppt,
        value = content[[ph_level]],
        location = officer::ph_location_label(properties_list[ph_level])
      )
    },
    .init = ppt
  )
}

#' Write presentation with one or multiple slides
#'
#' @param slides_content a list with inner list per slide to write. Each inner
#' list should have two elements: `layout` and `content`, where layout is a
#' string indicating the layout to use and content is a list with the
#' layout properties to write.
#'
#' @return a rpptx object
#' @export
write_presentation <- function(slides_content) {
  purrr::reduce(
    slides_content,
    \(template, content) {
      layout <-  content$layout
      content <-  content$content

      write_slide(template, layout, content)
    },
    .init = get_bcrd_template()
  )
}
