context("makeStrategy")

test_that("makeStrategy creates an object of class Strategy", {
  expect_s3_class(makeStrategy.autoxgb(), "Strategy")
})
