test_that("download_dataset saves file when parse = FALSE", {
  skip_if_offline()
  tmp <- tempfile(); dir.create(tmp)

  # Use a small, stable direct file URL:
  file_url <- "https://datos.madrid.es/egob/catalogo/300594-1-registro-AIP.csv"
  # path-only
  out_path <- download_dataset(file_url, save_to = tmp, parse = FALSE)
  expect_type(out_path, "character")       # should be a string
  expect_true(file.exists(out_path))
})

test_that("download_dataset parses file when parse = TRUE", {
  skip_if_offline()
  tmp <- tempfile()
  dir.create(tmp)

  file_url <- "https://datos.madrid.es/egob/catalogo/300594-1-registro-AIP.csv"

  out_obj <- download_dataset(file_url, save_to = tmp, parse = TRUE)

  # parse
  expect_true(inherits(out_obj, "tbl_df"))
})
