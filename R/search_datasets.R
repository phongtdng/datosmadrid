#' Search datasets from datos.madrid.es
#'
#' @description Searches the Madrid open data portal for datasets matching a keyword.
#'
#' @param keyword A character string to search for (e.g. "bicicleta", "eventos")
#' @return A tibble data frame with dataset titles
#' @export
#'
#' @examples
#' search_datasets("bicicleta")
#'
search_datasets <- function(keyword) {
  if (!requireNamespace("rvest", quietly = TRUE)) stop("Please install the 'rvest' package.")
  if (!requireNamespace("httr", quietly = TRUE)) stop("Please install the 'httr' package.")

  base_url <- "https://datos.madrid.es/sites/v/index.jsp?vgnextoid=374512b9ace9f310VgnVCM100000171f5a0aRCRD&buscar=true&Texto=&Sector=&Formato=&Periodicidad=&departamento=&orderByCombo=CONTENT_INSTANCE_NAME_DECODE"
  full_url <- gsub("Texto=", URLencode(paste0("Texto=",gsub(" ", "+",keyword))),base_url)

  html <- rvest::read_html(full_url)

  titles <- html |>
    rvest::html_elements("a.event-link") |>
    rvest::html_text(trim = TRUE)

  links <- html |>
    rvest::html_elements("a.event-link") |>
    rvest::html_attr("href") |>
    paste0("https://datos.madrid.es", rel_link = _)

  result <- tibble::tibble(
    title = titles
  )

  return(result)
}
