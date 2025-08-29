test_that("fetch_api calls a JSON endpoint and parses or returns raw", {
  skip_if_offline()
  # Example with formato placeholder:
  out <- fetch_api(
    path = "/catalogo/201132-0-museos.{formato}",
    path_params = list(formato = "json"),
    parse = TRUE
  )
  expect_true(is.list(out) || inherits(out, "tbl_df"))
})


test_that("fetch_api returns raw when response is not JSON", {
  skip_if_offline()
  out_raw <- fetch_api(
    path = "/catalogo/201132-0-museos.{formato}",
    path_params = list(formato = "xml"),
    parse = FALSE
  )
  expect_type(out_raw, "character")
})
