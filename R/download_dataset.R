#' Download and parse a dataset from datos.madrid.es
#'
#' @description
#' Given either a dataset *detail page URL* (preferred) or a *direct file URL*,
#' this function downloads the resource (CSV/JSON/XML/XLS/XLSX/TXT) and returns
#' a parsed R object. If a detail page URL is provided, the function uses
#' \code{get_metadata()} to discover available resources and selects one based on
#' \code{format} or \code{prefer} ordering.
#'
#' @param url Character scalar. Either a dataset detail page URL
#'   (e.g., "https://datos.madrid.es/egob/catalogo/300228-0-agenda-actividades-culturales")
#'   or a direct file URL (ends with .csv, .json, .xml, .xls, .xlsx, .txt).
#' @param format Optional character scalar to force a specific format
#'   (e.g., "csv", "json", "xml", "xls", "xlsx", "txt"). Case-insensitive.
#' @param prefer Character vector defining the format preference order when
#'   \code{format} is not supplied. Defaults to c("csv", "json", "xml", "xlsx", "xls", "txt").
#' @param save_to Optional path (directory or file). If a directory, the filename
#'   is inferred from the URL. If \code{NULL} (default), a temporary file is used.
#' @param return What to return: \code{"parsed"} (default), \code{"path"} (file path only),
#'   or \code{"both"} (list with \code{data} and \code{path}).
#' @param quiet Logical, suppress messages. Default TRUE.
#'
#' @return By default, a parsed R object:
#' \itemize{
#'   \item data.frame (CSV, XLS/XLSX, TXT with delimiters)
#'   \item list or data.frame (JSON; depends on structure)
#'   \item xml_document (XML)
#' }
#' If \code{return = "path"}, returns the downloaded file path.
#' If \code{return = "both"}, returns a list with elements \code{data} and \code{path}.
#'
#' @examples
#' out <- download_dataset(
#'   "https://datos.madrid.es/egob/catalogo/300228-0-agenda-actividades-culturales"
#' )
#'
#' @export

download_dataset <- function(url,
                             # format = NULL,
                             # prefer = c("csv", "json", "xml", "xlsx", "xls", "txt"),
                             save_to = NULL,
                             return = c("parsed", "path", "both"),
                             quiet = TRUE) {
  # ---- deps ----
  if (!requireNamespace("httr", quietly = TRUE)) stop("Please install 'httr'.")
  if (!requireNamespace("readr", quietly = TRUE)) stop("Please install 'readr'.")
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Please install 'xml2'.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Please install 'jsonlite'.")
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Please install 'readxl'.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Please install 'stringr'.")

  # Determine if URL is a direct file
  ext <- tools::file_ext(url) |> tolower()
  is_direct_file <- ext %in% c("csv", "json", "xml", "xls", "xlsx", "txt")

  # ---- Destination path ----
  if (is.null(save_to)) {
    if (!dir.exists("downloads")) {dir.create("downloads")}
    save_to <- file.path("downloads", basename(utils::URLdecode(url)))
  } else if (!dir.exists(save_to)) {
    dir.create(save_to)
    save_to <- file.path(save_to, basename(utils::URLdecode(url)))
  } else {
    save_to <- file.path(save_to, basename(utils::URLdecode(url)))
  }

  # ---- Download ----
  httr::GET(url, httr::write_disk(save_to, overwrite = TRUE))

  # ---- Return ----

  # helper: parse by extension
  parse_by_extension <- function(path, ext) {
    if (ext %in% c("csv")) {
      header <- readr::read_lines(path, n_max = 5)
      delim <- if (any(grepl(";", header))) ";" else ","
      return(if(delim == ",") readr::read_csv(path, show_col_types = FALSE)
             else readr::read_csv2(path, show_col_types = FALSE))
    } else if (ext %in% c("txt")) {
      # quick heuristic for delimiter
      header <- readr::read_lines(path, n_max = 5)
      delim <- if (any(grepl(";", header))) ";" else if (any(grepl("\t", header))) "\t" else ","
      return(readr::read_delim(path, delim = delim, show_col_types = FALSE))
    } else if (ext %in% c("json")) {
      txt <- readr::read_file(path)
      obj <- jsonlite::fromJSON(txt, simplifyVector = TRUE)
      return(obj)
    } else if (ext %in% c("xml")) {
      return(xml2::read_xml(path))
    } else if (ext %in% c("xls", "xlsx")) {
      return(readxl::read_excel(path))
    } else {
      warning(sprintf("No parser for extension '%s'. Returning file path.", ext))
      return(path)
    }
  }

  parsed <- parse_by_extension(save_to, ext)

  if (return == "path") {
    return(save_to)
  } else {
    message("File path: ", save_to)
    return(parsed)  # "parsed"
  }
}

#TEST
test_csv <- download_dataset(url = "https://datos.madrid.es/egob/catalogo/300228-28-accidentes-trafico-detalle.csv",
                 save_to = "downloads",
                 return = "path")

test_xlsx <- download_dataset(url = "https://datos.madrid.es/egob/catalogo/300228-29-accidentes-trafico-detalle.xlsx",
                         save_to = "downloads",
                         return = "parsed")



