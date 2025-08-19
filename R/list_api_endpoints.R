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

  # ---- titles ----
  titles <- c()
  for (i in 1:length(paths)) {
    title <- paths[[i]]$get$tags[[1]]
    titles <- c(titles, title)
  }



  }

