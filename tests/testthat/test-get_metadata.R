test_that("get_metadata returns title and resources table", {
  skip_if_offline()
  # Pick a stable dataset detail page:
  url <- "https://datos.madrid.es/portal/site/egob/menuitem.c05c1f754a33a9fbe4b2e4b284f1a5a0/?vgnextoid=afbcb9c3dcc87810VgnVCM2000001f4a900aRCRD&vgnextchannel=374512b9ace9f310VgnVCM100000171f5a0aRCRD&vgnextfmt=default"
  md <- get_metadata(url)
  expect_true(all(c("title","description", "tags","resources") %in% names(md)))
  expect_true(is.data.frame(md$resources) || is.list(md$resources))
})
