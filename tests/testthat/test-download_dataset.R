test_that("download_dataset saves and optionally parses", {
  skip_if_offline()
  tmp <- tempfile(); dir.create(tmp)

  # Use a small, stable direct file URL:
  file_url <- "https://datos.madrid.es/egob/catalogo/300594-1-registro-AIP.csv"
  # path-only
  out_path <- download_dataset(file_url, save_to = tmp, parse = FALSE)
  expect_type(out_path, "character")       # should be a string
  expect_true(file.exists(out_path))

  # parse
  out_parsed <- download_dataset(file_url, save_to = tmp, parse = TRUE)
  expect_true(is.list(out_parsed) || inherits(out_parsed, "tbl_df"))
})
