test_that("open_in_browser validates URL scheme", {
  # Bad URL
  expect_error(
    open_in_browser("www.example.com"),
    "must start with 'http://'"
  )

  # Good URL
  expect_silent(
    open_in_browser("https://datos.madrid.es/portal/site/egob")
  )
})

