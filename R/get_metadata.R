#' Get metadata for a datos.madrid.es dataset
#'
#' @description
#' Scrapes a dataset *detail page* on \url{https://datos.madrid.es} and returns
#' key metadata plus a table of downloadable resources (CSV, JSON, XML, XLS, PDF, etc.).
#'
#' @param url Character. Absolute URL to a dataset detail page
#'   (e.g., "https://datos.madrid.es/egob/catalogo/300228-0-agenda-actividades-culturales").
#'   Prefer passing the full URL returned by \code{search_datasets()}.
#' @return A named list with:
#' \itemize{
#'   \item \code{title} (chr)
#'   \item \code{description} (chr)
#'   \item \code{last_update} (chr, best-effort)
#'   \item \code{tags} (chr vector, best-effort)
#'   \item \code{resources} (tibble): columns \code{label}, \code{format}, \code{url}
#' }
#' @details
#' This function is resilient to minor HTML changes. It searches multiple common
#' selectors for titles, descriptions, and metadata terms found on Spanish open
#' data portals (e.g., "Descripción", "Fecha de actualización", "Etiquetas").
#' Downloadable resources are inferred from links and their file extensions.
#'
#' @examples
#' \dontrun{
#' meta <- get_metadata(
#'   "https://datos.madrid.es/sites/v/index.jsp?vgnextoid=7c2843010d9c3610VgnVCM2000001f4a900aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD"
#' )
#' meta$title
#' meta$resources
#' }
#'
#' @export
get_metadata <- function(url) {
  if (missing(url) || !is.character(url) || length(url) != 1L) {
    stop("Please provide a single dataset detail page URL (character).", call. = FALSE)
  }
  if (!requireNamespace("rvest", quietly = TRUE)) stop("Please install 'rvest'.")
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Please install 'xml2'.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Please install 'stringr'.")
  if (!requireNamespace("tibble", quietly = TRUE)) stop("Please install 'tibble'.")

  html <- rvest::read_html(url)

  # --- Title ---
  title  <- html |>
    rvest::html_elements("h3.summary-title") |>
    rvest::html_text()

  # --- Description ---
  description <- html |>
    rvest::html_elements("p.MsoNormal") |>
    (\(x) x[1])() |>
    rvest::html_text()

  # --- Tags ---
  tags <- html |>
    rvest::html_elements(xpath = "//h5[@class='title7' and contains(text(), 'Palabras clave')]//following-sibling::p[1]") |>
    rvest::html_text(trim = TRUE) |>
    gsub("\r|\t|\n| ", "", x = _)

  # --- Resources / Downloads ---
  all_links <- html |>
    rvest::html_elements("li.asociada-item") |>
    rvest::html_elements("a[href]")

  hrefs <- all_links |>
    rvest::html_attr("href") |>
    paste0("https://datos.madrid.es", rel_link = _)

  # Keep likely downloadable resources
  ext <- tools::file_ext(hrefs)
  is_file <- stringr::str_to_lower(ext) %in% c("csv", "json", "xml", "xls", "xlsx", "txt", "zip", "pdf", "geojson", "kml", "kmz", "shp", "gml")

  res_idx <- which(is_file)
  res_tbl <- tibble::tibble(
    label = res_idx,
    format = stringr::str_to_upper(ext[res_idx]),
    url = hrefs[res_idx]
  ) |>
    dplyr::distinct(dplyr::across(everything()))

  out <- list(
    title       = title,
    description = description,
    tags        = tags,
    resources   = res_tbl
  )

  out
}
