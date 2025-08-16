#' Open a Madrid Open Data page in your browser
#'
#' @description
#' Opens the default web browser at a dataset detail page or any given URL.
#' @param url Character scalar. A full URL (must start with "http://" or "https://").
#' @param browser Optional. Passed to \code{utils::browseURL()}. If \code{NULL},
#'   the system default browser is used. If you want to force a specific browser,
#'   provide the full path to its executable (e.g.,
#'   \code{"C:/Program Files/Google/Chrome/Application/chrome.exe"} on Windows
#'   or \code{"open -a 'Google Chrome'"} on macOS).
#'
#' @return Invisibly returns the URL that was opened.
#'
#' @examples
#' \dontrun{
#' open_in_browser("https://datos.madrid.es/portal/site/egob")
#' }
#'
#' @export

open_in_browser <- function(url,
                            browser = NULL) {
  # ---- validate ----
  if (!is.character(url) || length(url) != 1) {
    stop("`url` must be a single character string.", call. = FALSE)
  }
  url <- trimws(url)

  if (!grepl("^https?://", url)) {
    stop("`url` must start with 'http://' or 'https://'.", call. = FALSE)
  }

  # ---- open ----
  utils::browseURL(url, browser = browser)

  # ---- return ----
  invisible(url)
}
