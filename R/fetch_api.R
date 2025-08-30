#' Fetch data from a Madrid Open Data API endpoint (minimal)
#'
#' @description
#' Calls a single Madrid Open Data API endpoint under \code{https://datos.madrid.es/egob}
#' and optionally parses the JSON response. Supports simple path templating such as
#' \code{"/catalogo/tipo/evento/{id}.json"} via \code{path_params}.
#'
#' @param path Character scalar. Endpoint path beginning with \code{"/"} relative to
#'   \code{https://datos.madrid.es/egob}, e.g. \code{"/catalogo/tipo/evento/{id}.json"}.
#' @param path_params Optional named list used to fill \code{\{placeholders\}} in \code{path},
#'   e.g. \code{list(id = 203)} replaces \code{\{id\}}. Values are URL-encoded.
#' @param query Optional named list of query parameters to append to the request,
#'   e.g. \code{list(q = "museo", limite = 50)}.
#' @param parse Logical, default \code{TRUE}. If \code{TRUE}, attempts to parse the JSON and
#'   return a tibble/data frame when feasible; otherwise returns the parsed R object (list).
#'   If \code{FALSE}, returns the raw response text (JSON string).
#'
#' @details
#' The function performs a single \code{GET} request (no pagination). When \code{parse = TRUE},
#' it uses \code{jsonlite::fromJSON(..., simplifyVector = TRUE, flatten = TRUE)} and then applies
#' light heuristics:
#' \itemize{
#'   \item If the top-level parsed object is a data frame, it is returned as a tibble.
#'   \item Otherwise, it searches the first top-level field that is a data frame and returns that.
#'   \item If no tabular node is found, the full parsed object (list) is returned.
#' }
#' This keeps simple “array-of-objects” responses convenient, while still exposing complex
#' nested structures when necessary.
#'
#' @return
#' If \code{parse = TRUE}:
#' \itemize{
#'   \item A tibble/data frame when a tabular node is detected; otherwise a parsed list.
#' }
#' If \code{parse = FALSE}:
#' \itemize{
#'   \item A character string with the raw JSON response.
#' }
#'
#' @examples
#' \dontrun{
#' fetch_api(
#'   path = "/catalogo/201132-0-museos.{formato}",
#'   path_params = list(formato = "json"),
#'   parse = TRUE
#' )
#'
#' }
#'
#' @seealso \code{\link[httr]{GET}}, \code{\link[jsonlite]{fromJSON}}
#' @export
#'
fetch_api <- function(path,
                      path_params= NULL,
                      query = NULL,
                      parse = TRUE) {

  # ---- deps ----
  if (!requireNamespace("httr", quietly = TRUE))     stop("Please install 'httr'.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Please install 'jsonlite'.")

  # ---- validate path ----
  if (!is.character(path) || length(path) != 1 || !nzchar(path)) {
    stop("`path` must be a non-empty character scalar.", call. = FALSE)
  }

  # ---- fill {placeholders} in path ----
  if (!is.null(path_params)) {
    if (!is.list(path_params) || is.null(names(path_params)) || any(names(path_params) == "")) {
      stop("`path_params` must be a named list.", call. = FALSE)
    }
    for (name in names(path_params)) {
      val <- utils::URLencode(as.character(path_params[[name]]), reserved = TRUE)
      path <- gsub(paste0("\\{", name, "\\}"), val, path, perl = TRUE)
    }
  }

  # ---- build URL ----
  base <- "https://datos.madrid.es/egob"
  path <- sub("^/+", "", path)       # trim leading slashes
  url  <- paste0(base, "/", path)

  # ---- perform request ----
  resp <- httr::GET(url,
                    query = query,
                    httr::timeout(15))
  httr::stop_for_status(resp)

  # ---- return ----
  if (!isTRUE(parse)) {
    return(httr::content(resp, as = "text", encoding = "UTF-8"))
  } else {
    txt <- httr::content(resp, as = "text", encoding = "UTF-8")
    # Parse & flatten
    obj <- jsonlite::fromJSON(txt, simplifyVector = TRUE, flatten = TRUE)
    # Heuristics to return a table if feasible
    if (is.data.frame(obj)) {
      return(tibble::as_tibble(obj))
    }

    if (is.list(obj)) {
      # If the top-level is a list, try to find the first data.frame inside
      nms <- names(obj)
      if (length(nms)) {
        for (nm in nms) {
          if (is.data.frame(obj[[nm]])) {
            return(tibble::as_tibble(obj[[nm]]))
          }
        }
      }
    }
    # Fallback: return whatever the API gave
    return(obj)
    }
}
