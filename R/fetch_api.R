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
