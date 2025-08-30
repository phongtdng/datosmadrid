#' Search datasets from datos.madrid.es
#'
#' @description Searches the Madrid open data portal for datasets matching a keyword.
#'
#' @param keyword character or character vector
#' @param sector character or character vector, filter by sector
#' @param format character or character vector, filter by format available
#' @param page_limit integer, how many result pages per keyword to scan
#' @param n_max integer, max rows to return
#' @param enrich logical, add details (slower)
#' @return tibble with at least title, url, other metadata about dataset
#'
#' @examples
#' \dontrun{
#' # Search for available datasets with "bici" in the title
#' search_datasets("bici")
#'
#' # Search by sector/category filter
#' search_datasets("", sector = "Deporte")
#'
#' # Controlled search with page and row returned output and additional info about the dataset
#' search_datasets("madrid", page_limit = 3, n_max = 30, enrich = TRUE)
#'
#' }
#'
#' @export
search_datasets <- function(keyword,
                            sector = NULL,
                            format = NULL,
                            page_limit = 1,
                            n_max = Inf,
                            enrich = FALSE) {
  if (!requireNamespace("rvest", quietly = TRUE)) stop("Please install the 'rvest' package.")
  if (!requireNamespace("httr", quietly = TRUE)) stop("Please install the 'httr' package.")

  # ---- Build search URL ----
  base_url <- "https://datos.madrid.es/sites/v/index.jsp?vgnextoid=374512b9ace9f310VgnVCM100000171f5a0aRCRD&buscar=true&Texto=&Sector=&Formato=&Periodicidad=&departamento=&orderByCombo=CONTENT_INSTANCE_NAME_DECODE"
  full_url <- gsub("Texto=", URLencode(paste0("Texto=",gsub(" ", "+",keyword))),base_url)
  if (!is.null(sector)) {
    sector <- sector |>
      tolower() |>
      iconv(x = _, from = "UTF-8", to = "ASCII//TRANSLIT")
    full_url <- gsub("Sector=", URLencode(paste0("Sector=",gsub(" y | | e ", "-", sector))),base_url)
  }

  if (!is.null(format)) {
    format <- format |>
      tolower() |>
      iconv(x = _, from = "UTF-8", to = "ASCII//TRANSLIT")
    full_url <- gsub("Formato=", URLencode(paste0("Formato=",gsub(" y | | e ", "-", format))),base_url)
  }
  # ---- Helper extract data function ----
  html <- rvest::read_html(full_url)

  extract <- function(html){
    titles <- html |>
      rvest::html_elements("a.event-link") |>
      rvest::html_text(trim = TRUE)

    # ---- Enrich data ----

    links <- html |>
      rvest::html_elements("a.event-link") |>
      rvest::html_attr("href") |>
      paste0("https://datos.madrid.es", rel_link = _)

    sectors <- html |>
      rvest::html_elements("div.row") |>
      rvest::html_elements("span.event-intro") |>
      rvest::html_text() |>
      (\(x) x[grepl("Sector", x)])() |>
      gsub("Sector: ", "", x = _)

    update_freq <- html |>
      rvest::html_elements("div.row") |>
      rvest::html_elements("span.event-intro") |>
      rvest::html_text() |>
      (\(x) x[grepl("Actualización", x)])() |>
      gsub("Actualización: ", "", x = _)

    last_updates <- html |>
      rvest::html_elements("div.row") |>
      rvest::html_elements("span.event-intro") |>
      rvest::html_text() |>
      (\(x) x[grepl("Última actualización", x)])() |>
      gsub("Última actualización: ", "", x = _)

    files <- c()

    cards <- html |> rvest::html_elements("li.withtable")
    resources_nodes <- cards |> rvest::html_element("div.event-info.min ul")
    if (is.null(resources_nodes)) return(character(0))

    for (i in resources_nodes) {
      x <- i |>
        rvest::html_elements("span") |>
        rvest::html_text() |>
        paste(x = _, collapse = ",")
      files <- c(files, x)
    }

    if(enrich == FALSE) {
      result <- tibble::tibble(
        title = titles
      )
    } else {
      result <- tibble::tibble(
        title = titles,
        sector = sectors,
        update_frequency = update_freq,
        last_update = last_updates,
        files_available = files,
        link = links
      )
    }

    return(result)
  }

  result <- extract(html)
  # ---- Extract other pages ----
  all_pages <- html |>
    rvest::html_elements("ul.pagination") |>
    (\(x) x[1])() |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    (\(x) x[-length(x)])()

  page_count <- ifelse(length(all_pages) == 0, 1, length(all_pages) + 1)

  if (page_limit > page_count) {
    message("Requested page_limit (", page_limit,
            ") is greater than available pages (", page_count,
            "). Using ", page_count, " instead.")
    page_limit <- page_count
  }

  if (page_limit > 1) {
    for (i in 1:(page_limit-1)) {
      html <- rvest::read_html(paste0("https://datos.madrid.es", all_pages[i]))

      new_page <- extract(html)
      result <- tibble::add_row(result, new_page)
    }
  }
  # ---- Results ----
  print(paste0("Max result pages: ", page_count,
               ". Page limit: ", page_limit,
               ". Results shown: ", ifelse(n_max > nrow(result), nrow(result), n_max)))
  return(print(result, n = n_max))
}
