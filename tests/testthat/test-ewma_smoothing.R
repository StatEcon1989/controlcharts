test_that("test ewma_smoothing", {
  x <- rep(1.0,4L)
  lambda <- 0.5
  res <- ewma_smoothing(x, lambda)
  expect_equal(res, c(0.5, 0.75, 0.875, 0.9375))
})
