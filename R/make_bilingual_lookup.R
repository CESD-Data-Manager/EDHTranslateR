#' Create a bilingual translation lookup table
#'
#' Translates a vector of unique values and returns a lookup table
#' containing the original text, translated text, and a bilingual
#' concatenation suitable for Darwin Core metadata fields.
#'
#' @param values A character vector to translate.
#' @param source_language Source language code. Defaults to `"en"`.
#' @param target_language Target language code. Defaults to `"fr"`.
#' @param separator Character string used to separate source and
#'   translated text. Defaults to `" / "`.
#'
#' @return A tibble with three columns:
#' \describe{
#'   \item{english}{Original values.}
#'   \item{translated}{Machine-translated values.}
#'   \item{bilingual}{Combined bilingual string.}
#' }
#'
#' @examples
#' lookup <- make_bilingual_lookup(
#'   c("Pebble", "Cobble")
#' )
#'
#' lookup$bilingual
#'
#' @export
make_bilingual_lookup <- function(
  values,
  source_language = "en",
  target_language = "fr",
  separator = " / "
) {
  # Validate inputs
  stopifnot(is.character(values))

  # Display machine translation warning
  .translation_warning()

  values <- values |>
    na.omit() |>
    unique()

    translated <- polyglotr::google_translate(
    values,
    source_language = source_language,
    target_language = target_language
  ) |>
    fix_encoding()

  tibble::tibble(
    english = values,
    translated = translated,
    bilingual = paste0(
      values,
      separator,
      translated
    )
  )
}
