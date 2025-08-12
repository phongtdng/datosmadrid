#' Search datasets from datos.madrid.es
#'
#' @description Searches the Madrid open data portal for datasets matching a keyword.
#'
#' @param keyword character or character vector
#' @param page_limit integer, how many result pages per keyword to scan
#' @param n_max integer, max rows to return (after dedup)
#' @param enrich logical, call get_metadata() to add details (slower)
#' @return tibble with at least title, url, dataset_id
#'
#' @export
#'
#' @examples
#' search_datasets("bicicleta")
#'
search_datasets <- function(keyword,
                            page_limit = 1,
                            n_max = Inf,
                            enrich = FALSE) {
  if (!requireNamespace("rvest", quietly = TRUE)) stop("Please install the 'rvest' package.")
  if (!requireNamespace("httr", quietly = TRUE)) stop("Please install the 'httr' package.")

  # ---- Build search URL ----
  base_url <- "https://datos.madrid.es/sites/v/index.jsp?vgnextoid=374512b9ace9f310VgnVCM100000171f5a0aRCRD&buscar=true&Texto=&Sector=&Formato=&Periodicidad=&departamento=&orderByCombo=CONTENT_INSTANCE_NAME_DECODE"
  full_url <- gsub("Texto=", URLencode(paste0("Texto=",gsub(" ", "+",keyword))),base_url)

  # ---- Extractfirst search page ----
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
      titles <- html |>
        rvest::html_elements("a.event-link") |>
        rvest::html_text(trim = TRUE)

      links <- html |>
        rvest::html_elements("a.event-link") |>
        rvest::html_attr("href") |>
        paste0("https://datos.madrid.es", rel_link = _)

      new_page <- tibble::tibble(
        title = titles
      )
      result <- tibble::add_row(result, new_page)
    }
  }
  # ---- Results ----
  print(paste0("Max result pages: ", page_count,
               ". Page limit: ", page_limit,
               ". Results shown: ", ifelse(n_max > nrow(result), nrow(result), n_max)))
  return(print(result, n = n_max))
}
