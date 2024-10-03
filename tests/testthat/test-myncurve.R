test_that("myncurve works", {
  result <- myncurve(mu = 0, sigma = 1, a = 1)

  expect_type(result, "list")

  expect_equal(result$mu, 0)
  expect_equal(result$sigma, 1)

  expect_equal(result$prob, pnorm(1, mean = 0, sd = 1))
})
