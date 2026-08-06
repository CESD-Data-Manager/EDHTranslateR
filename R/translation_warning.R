#' Display a machine translation warning
#'
#' Displays a prominent warning reminding users that machine-translated
#' content may contain errors and must be reviewed before publication.
#'
#' The warning is intended for all functions that rely on automated
#' translation services.
#'
#' Users should verify terminology and phrasing against authoritative
#' Government of Canada translation resources, including
#' [GC Translate](https://gctraduction-gctranslate.gc.ca/en-CA/translate)
#' and
#' [TERMIUM Plus](https://www.btb.termiumplus.gc.ca/).
#'
#' @details
#' Machine translation may produce inaccurate, incomplete, ambiguous,
#' contextually inappropriate, or incorrect translations.
#'
#' Automated translation should not be used without thorough review for:
#' \itemize{
#'   \item Species names
#'   \item Scientific or taxonomic nomenclature
#'   \item Common or local names
#'   \item Indigenous place names
#'   \item Technical terminology requiring subject-matter expertise
#' }
#'
#' All translated text must be reviewed and verified by a qualified
#' human reviewer before publication, public release, or inclusion
#' in official records.
#'
#' @return Invisibly returns `NULL`.
#'
#' @keywords internal
.translation_warning <- function() {
  width <- 76

  header <- c(
    "",
    paste(rep("=", width), collapse = ""),
    "          MACHINE TRANSLATION REVIEW REQUIRED",
    paste(rep("=", width), collapse = "")
  )

  body <- c(
    strwrap(
      paste(
        "This function uses an automated machine translation service.",
        "Translations may be inaccurate, incomplete, ambiguous,",
        "contextually inappropriate, or otherwise incorrect."
      ),
      width = width
    ),
    "",
    "Do NOT use machine-translated output without thorough review",
    "against authoritative translation resources such as:",
    "",
    "  * GC Translate:",
    "    https://gctraduction-gctranslate.gc.ca/en-CA/translate",
    "",
    "  * TERMIUM Plus:",
    "    https://www.btb.termiumplus.gc.ca/",
    "",
    "Particular caution is required for:",
    "",
    "  * Species names",
    "  * Scientific or taxonomic nomenclature",
    "  * Common or local names",
    "  * Indigenous place names",
    "  * Specialised terminology requiring subject-matter expertise",
    "",
    strwrap(
      paste(
        "All translated text must be reviewed and verified by a",
        "qualified human reviewer prior to publication, public",
        "release, or inclusion in official records."
      ),
      width = width
    )
  )

  footer <- paste(rep("=", width), collapse = "")

  rlang::warn(
    paste(
      c(header, body, footer),
      collapse = "\n"
    ),
    call. = FALSE,
    immediate. = TRUE,
    .frequency = 'regularly',
    .frequency_id = 'translation_warning'
  )

  invisible(NULL)
}
