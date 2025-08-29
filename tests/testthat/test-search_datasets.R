test_that("search_datasets returns a tibble with expected cols", {
  skip_if_offline()
  res <- search_datasets("bici", page_limit = 1, n_max = 5, enrich = FALSE)
  expect_s3_class(res, "tbl_df")
  expect_true(all(c("title") %in% names(res)))
  expect_true(nrow(res) >= 0)
})

test_that("search_datasets respects page_limit", {
  skip_if_offline()
  res1 <- search_datasets("madrid", page_limit = 1, n_max = Inf, enrich = FALSE)
  res2 <- search_datasets("madrid", page_limit = 2, n_max = Inf, enrich = FALSE)
  expect_true(nrow(res2) >= nrow(res1))
})
