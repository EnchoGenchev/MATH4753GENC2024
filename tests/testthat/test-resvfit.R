test_that("resvfit works", {
  x <- 1:5
  y <- x^2
  l <- lm(y ~ x)
  rvf <- resvfit(l)
  expect_equal(residuals(l), c(2, -1, -2, -1, 2), ignore_attr = TRUE)
  expect_equal(fitted(l), c(-1, 5, 11, 17, 23), ignore_attr = TRUE)
})
