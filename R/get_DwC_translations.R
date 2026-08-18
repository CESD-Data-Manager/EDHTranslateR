#' Translates Darwin Core terms to English and French labels and descriptions
#'
#' @param x A character vector of Darwin Core terms to translate.
#'
#' @returns A tibble with the translated terms and their English and French labels and descriptions. Non-DwC terms will not have translations and will be returned as NA.
#'
#' @export
#' @examples
#' @required dplyr
#' @required readr
dwc_term_translation <- function(x) {

  dwc <- readr::read_csv(
    "https://raw.githubusercontent.com/tdwg/rs.tdwg.org/master/terms/terms-translations.csv", show_col_types = FALSE
  ) |>
    dplyr::select(
      term_localName,
      label_en,
      label_fr,
      dcterms_description_en,
      dcterms_description_fr
    )
  df <- tibble::tibble(
    orig_term = character(),
    term_localName = character(),
    label_en = character(),
    label_fr = character(),
    dcterms_description_en = character(),
    dcterms_description_fr = character()
  )
  for (i in seq_along(x)) {
    if (x[i] %in% dwc$term_localName) {
      df[i, ] <- dwc |>
        dplyr::filter(term_localName == x[i]) |>
        dplyr::mutate(orig_term = x[i]) |>
        dplyr::select(
          orig_term,
          term_localName,
          label_en,
          label_fr,
          dcterms_description_en,
          dcterms_description_fr
        ) |>
        dplyr::slice(1)
    } else {
      df[i, ] <- tibble::tibble(
        orig_term = x[i],
        term_localName = NA_character_,
        label_en = NA_character_,
        label_fr = NA_character_,
        dcterms_description_en = NA_character_,
        dcterms_description_fr = NA_character_
      )
    }
  }
  print(df)
}
