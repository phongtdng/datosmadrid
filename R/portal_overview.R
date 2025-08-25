#' Overview of the Madrid Open Data Portal
#'
#' Scrapes general information about the Madrid Open Data portal, including
#' usage conditions, total number of datasets, available categories,
#' the number of datasets per category, and available api endpoints
#'
#' @param keyword Character (optional). Filter results by keyword (case-insensitive). Default \code{NULL} returns all.
#' @param catalogue One of \code{"datasets"}, \code{"api"}, or \code{"all"} (default). Select which portal section(s) to summarize.
#'
#' @details
#' The function retrieves:
#' \itemize{
#'   \item General condition of use for portal data
#'   \item Total number of datasets available
#'   \item Categories (Sectors) present on the portal
#'   \item Number of datasets available per category
#' }
#'
#' @return
#' Prints general conditions and total datasets as messages,
#' and returns (invisibly) a tibble with:
#' \describe{
#'   \item{category}{Character. Category name.}
#'   \item{total_datasets}{Integer. Number of datasets under that category.}
#' }
#'
#' @examples
#' \dontrun{
#' portal_overview()
#' }
#'
#' @export
portal_overview <- function(
    keyword = NULL,
    catalogue = c("datasets", "api", "all")
    ) {
  # ---- deps ----
  if (!requireNamespace("rvest", quietly = TRUE))   stop("Please install 'rvest'.")
  if (!requireNamespace("tibble", quietly = TRUE))  stop("Please install 'tibble'.")

  # ---- about ----
  url <- "https://datos.madrid.es/portal/site/egob/menuitem.400a817358ce98c34e937436a8a409a0/?vgnextoid=b4c412b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextchannel=b4c412b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default"
  html <- rvest::read_html(url)

  use_condition <- html |>
    rvest::html_elements("div.summary-intro") |>
    rvest::html_elements("p") |>
    rvest::html_text()

  print(paste("General Condition:",use_condition))

  # ---- datasets ----
  if (catalogue != "api") {
    if(is.null(keyword)) {
      url <- "https://datos.madrid.es/sites/v/index.jsp?vgnextoid=374512b9ace9f310VgnVCM100000171f5a0aRCRD&buscar=true&Texto=&Sector=&Formato=&Periodicidad=&departamento=&orderByCombo=CONTENT_INSTANCE_NAME_DECODE"
    } else {
      base_url <- "https://datos.madrid.es/sites/v/index.jsp?vgnextoid=374512b9ace9f310VgnVCM100000171f5a0aRCRD&buscar=true&Texto=&Sector=&Formato=&Periodicidad=&departamento=&orderByCombo=CONTENT_INSTANCE_NAME_DECODE"
      url <- gsub("Texto=", URLencode(paste0("Texto=",gsub(" ", "+", keyword))),base_url)
    }


    html <- rvest::read_html(url)

    total_datasets <- html |>
      rvest::html_elements("ul#totalResultsUL") |>
      rvest::html_element("li") |>
      rvest::html_text() |>
      gsub(".*?([0-9]+).*", "\\1", x = _)

    print(paste("Total datasets:", total_datasets))

    categories <- html |>
      rvest::html_elements("select#Sector") |>
      rvest::html_elements("option") |>
      rvest::html_text() |>
      (\(x) x[!grepl("Seleccione", x)])()

    # ---- datasets per category ----
    cat_datasets <- c()
    for (i in categories) {
      cat <- i |>
        tolower() |>
        iconv(x = _, from = "UTF-8", to = "ASCII//TRANSLIT")
      full_url <- gsub("Sector=", URLencode(paste0("Sector=",gsub(" y | | e ", "-",cat))),url)

      html <- rvest::read_html(full_url)

      total_datasets <- html |>
        rvest::html_elements("ul#totalResultsUL") |>
        rvest::html_element("li") |>
        rvest::html_text() |>
        gsub(".*?([0-9]+).*", "\\1", x = _)

      cat_datasets <- c(cat_datasets, total_datasets)
    }

    catalogue_tbl <- tibble::tibble(
      category = as.character(categories),
      total_datasets = as.integer(cat_datasets)
    )

    if(!is.null(keyword)) {
      catalogue_tbl <- catalogue_tbl |> dplyr::filter(total_datasets > 0)
    }
  }

  if(is.null(keyword)) {
    api <- datosmadrid::list_api_endpoints(include_params = FALSE)
  } else {
    api <- datosmadrid::list_api_endpoints(filter = keyword, include_params = FALSE)
  }

  if(catalogue == "datasets") {
    catalogue_tbl
  } else if (catalogue == "api") {
    api
  } else {
    out <- list(datasets = catalogue_tbl, api = api)
    out
  }
}

portal_overview(catalogue = "all")
