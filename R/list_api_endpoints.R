#' List API endpoints from Madrid's OpenAPI spec
#'
#' @description
#' Reads the Madrid Open Data API OpenAPI/Swagger JSON and returns a tidy table
#' of endpoints (one row per path+method). You can filter by keyword/tag and HTTP
#' method and choose to include parameter details.
#'
#' @param filter Character or \code{NULL}. If provided, keep only endpoints whose
#'   \emph{tags}, \emph{path}, or \emph{summary} contain this substring (case-insensitive).
#' @param method Character or \code{NULL}. One or more HTTP methods to keep
#'   (e.g., \code{"get"}, \code{"post"}). Case-insensitive.
#' @param include_params Logical, default \code{TRUE}. If \code{TRUE}, includes a
#'   list-column \code{parameters} with basic details (name, in, required, type/schema).
#'
#' @return A tibble with columns:
#' \itemize{
#'   \item \code{path} (chr)
#'   \item \code{method} (chr)
#'   \item \code{parameters} (list of data.frames; present only if \code{include_params = TRUE})
#' }
#'
#' @examples
#' \dontrun{
#' # All GET endpoints with parameter details
# list_api_endpoints(method = "get")
#'
#' # Endpoints related to "agenda" (matches tag, path)
#' list_api_endpoints(filter = "agenda")
#' }
#'
#' @export
list_api_endpoints <- function(filter = NULL,
                               method = NULL,
                               include_params = TRUE) {
  # ---- deps ----
  if (!requireNamespace("httr", quietly = TRUE))     stop("Please install 'httr'.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Please install 'jsonlite'.")
  if (!requireNamespace("tibble", quietly = TRUE))   stop("Please install 'tibble'.")
  if (!requireNamespace("stringr", quietly = TRUE))  stop("Please install 'stringr'.")

  # ---- read spec (Swagger/OpenAPI JSON) ----
  spec_url <- "https://datos.madrid.es/egobfiles/api.datos.madrid.es.json"
  resp <- httr::GET(spec_url, httr::timeout(15))
  httr::stop_for_status(resp)
  spec <- jsonlite::fromJSON(httr::content(resp, as = "text", encoding = "UTF-8"),
                             simplifyVector = FALSE)
  paths <- spec$paths
  if (is.null(paths) || !length(paths)) {
    stop("Spec has no paths. The API description may have changed.", call. = FALSE)
  }

  # extract parameters helper
  extract_params <- function(path_obj, op_obj) {
    plist <- c(path_obj$parameters, op_obj$parameters)

    if (is.null(plist) || !length(plist)) {
      return(data.frame(name    =character(0),
                        'in'     =character(0),
                        required =logical(0),
                        type     =character(0),
                        stringsAsFactors = FALSE))
    }
    # build a tiny df helper
    out <- lapply(plist, function(p) {
      type <- if (!is.null(p$type)) p$type else if (!is.null(p$schema$type)) p$schema$type else NA_character_
      data.frame(
        name     = as.character(p$name %||% NA_character_),
        'in'     = as.character(p$`in` %||% NA_character_),
        required = as.logical(p$required %||% FALSE),
        type     = as.character(type),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, out)
  }

  # ---- build rows (one per path+method) ----
  rows <- list()
  r_i <- 0L #index counter
  path_names <- names(paths)
  for (i in seq_along(paths)) {
    p_name <- path_names[i]
    p_obj  <- paths[[i]]
    methods <- names(p_obj)

    # keep only standard http method keys
    keep_m <- tolower(methods) %in% c("get","post","put","delete","patch","options","head")
    if (!any(keep_m)) next

    for (mi in which(keep_m)) {
      method_name <- tolower(methods[mi])
      op_obj <- p_obj[[mi]]

    # basic fields for filter
    tags    <- op_obj$tags |>  as.character()
    summary <- if (!is.null(op_obj$summary)) op_obj$summary else ""
    path_lc <- tolower(p_name)

    # apply filters
    if (!is.null(method)) {
      if (!tolower(method_name) %in% tolower(method)) next
    }
    if (!is.null(filter)) {
      f <- tolower(filter)
      has_match <- any(stringr::str_detect(tolower(tags), f)) ||
        stringr::str_detect(path_lc, f) ||
        stringr::str_detect(tolower(summary), f)
      if (!has_match) next
    }
    r_i <- r_i + 1L
    rows[[r_i]] <- list(
      title = tags,
      path   = p_name,
      method = method_name,
      parameters = if (isTRUE(include_params)) extract_params(p_obj, op_obj) else NULL
    )

    }
  }

  if (!length(rows)) {
    # return empty tibble with stable columns if failure
    out <- tibble::tibble(
      path = character(0),
      method = character(0)
    )
    if (isTRUE(include_params)) out$parameters <- list()
    return(out)
  }

  # ---- produce tibble ----
  path_vec   <- vapply(rows, `[[`, character(1), "path")
  method_vec <- vapply(rows, `[[`, character(1), "method")
  title_vec <- vapply(rows, `[[`, character(1), "title")

  out <- tibble::tibble(
    title = title_vec,
    path = path_vec,
    method = method_vec
  )
  if (isTRUE(include_params)) {
    params_list <- lapply(rows, `[[`, "parameters")
    out$parameters <- params_list
  }

  out

}
