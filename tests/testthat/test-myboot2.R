test_that("myboot2 works", {
  sample_data <- rnorm(100, mean = 50, sd = 10)
  result <- myboot2(iter = 100, x = sample_data, fun = "mean", alpha = 0.05)

  expect_type(result, "list")
  expect_named(result, c("ci", "fun", "x"))
  expect_type(result$ci, "double")
  expect_equal(length(result$ci), 2)
})
