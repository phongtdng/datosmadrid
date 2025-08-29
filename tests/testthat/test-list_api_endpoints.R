test_that("list_api_endpoints returns title, path, method and (optional) params", {
  skip_if_offline()
  ep <- list_api_endpoints(filter = "bici", method = "get", include_params = TRUE)
  expect_true(all(c("title","path","method", "parameters") %in% names(ep)))
  if ("parameters" %in% names(ep) && length(ep$parameters)) {
    expect_true(is.data.frame(ep$parameters[[1]]) || is.null(ep$parameters[[1]]))
  }
})
