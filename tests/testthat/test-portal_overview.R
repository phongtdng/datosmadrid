test_that("portal_overview returns catalogue+api when catalogue = 'all'",{
  skip_on_cran()
  skip_if_offline()
  ov <- portal_overview(catalogue = "all")

  # outputs
  expect_output(
    portal_overview(catalogue = "all"),
    regexp = "Total datasets:"
  )

  expect_output(
    portal_overview(catalogue = "all"),
    regexp = "General Condition"
  )
  # top-level structure
  expect_true(is.list(ov))
  expect_true(all(c("datasets", "api") %in% names(ov)))

  # datasets section
  expect_s3_class(ov$datasets, "data.frame")
  # allow empty but require columns exist
  expect_true(all(c("total_datasets","category") %in% names(ov$datasets)))

  # api section
  expect_s3_class(ov$api, "data.frame")
  expect_true(all(c("title","path", "method") %in% names(ov$api)))
})
