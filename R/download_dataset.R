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
#' @param save_to Optional path (directory or file). If a directory, the filename
#'   is inferred from the URL. If \code{NULL} (default), a temporary file is used.
#' #' @param parse Logical, default is \code{FALSE}.
#'   If \code{TRUE}, the downloaded file(s) will be read into R and returned as parsed objects
#'   (e.g., a data frame for CSV, a list for JSON, an XML document for XML).
#'   If \code{FALSE}, only the file path(s) to the downloaded file(s) are returned.
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
#'   "https://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=49e83d5e3af7a510VgnVCM1000001d4a900aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default"
#' )
#'
#' out <- download_dataset(
#' "https://datos.madrid.es/egob/catalogo/300094-12-areas-caninas.xlsx"
#' )
#'
#' @export

download_dataset <- function(url,
                             format = NULL,
                             save_to = NULL,
                             parse = FALSE,
                             quiet = TRUE) {
  # ---- deps ----
  if (!requireNamespace("httr", quietly = TRUE)) stop("Please install 'httr'.")
  if (!requireNamespace("readr", quietly = TRUE)) stop("Please install 'readr'.")
  if (!requireNamespace("xml2", quietly = TRUE)) stop("Please install 'xml2'.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Please install 'jsonlite'.")
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Please install 'readxl'.")
  if (!requireNamespace("stringr", quietly = TRUE)) stop("Please install 'stringr'.")

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


  # Determine if URL is a direct file
  ext <- tools::file_ext(url) |> tolower()
  is_direct_file <- ext %in% c("csv", "json", "xml", "xls", "xlsx", "txt")

  # ---- Destination path ----
  if (is.null(save_to)) {
    if (!dir.exists("downloads")) {dir.create("downloads")}
    save_to <- file.path("downloads")
  } else if (!dir.exists(save_to)) {
    dir.create(save_to)
    save_to <- file.path(save_to)
  } else {
    save_to <- file.path(save_to)
  }

  if(is_direct_file){
    path <- file.path(save_to, basename(utils::URLdecode(url)))
    # ---- Download ----
    httr::GET(url, httr::write_disk(path, overwrite = TRUE))
    # ---- Parsed ----
    if(parse){
      parsed <- parse_by_extension(path, ext)
      return(parsed)
    }
  } else {
    # ---- Resources table ----
    meta_data <- datosmadrid::get_metadata(url)
    res <- meta_data$resources

    if(is.null(res) || nrow(res) == 0) {stop("No downloadable resources found on the page.", call. = FALSE)}

    res$format <- tolower(res$format)

    if(!is.null(format)) {
      format <- tolower(format)
      candidates <- res[res$format == format, ,drop=FALSE]
      if (nrow(candidates)==0) {
        available <- unique(res$format[res$format != ""])
        stop(sprintf(
          "Requested format '%s' not available. Available: %s",
          format, paste(available, collapse = ", ")
        ), call. = FALSE)
      }
    } else {
      # no explicit format: pick the “best” one
      prefer <- c("csv", "json", "xml", "xlsx", "xls", "txt")
      i <- which(res$format %in% prefer)
      if (!length(i)) {
        stop("Could not find a supported format on this page.", call. = FALSE)
      }
      resource_url <- res$url[i[1]]
      format <- res$format[i[1]]
      message("Selected format: ", format)
    }

    # ---- Download & Parse----
    get_res <- res[res$format == format, ,drop=FALSE]
    parsed <- list()
    for (i in seq_len(nrow(get_res))) {
      # ---- Download ----
      path <- file.path(save_to, basename(utils::URLdecode(get_res$url[i])))
      httr::GET(get_res$url[i], httr::write_disk(path, overwrite = TRUE))

      # ---- Parsed ----
      if(parse) {parsed[[i]] <- parse_by_extension(path, format)}
    }
    if(parse) return(parsed)
  }
}
