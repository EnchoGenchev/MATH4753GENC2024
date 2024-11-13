test_that("mypvalue works", {
  result <- mypvalue(t0 = 2.1, xmax = 4, n = 20, alpha = 0.05)

  # Test that result is a list with two elements: 'q' and 'pvalue'
  expect_type(result, "list")
  expect_named(result, c("q", "pvalue"))

  # Test that 'q' and 'pvalue' are numeric
  expect_type(result$q, "double")
  expect_type(result$pvalue, "double")

  # Check if 'q' and 'pvalue' are within expected ranges
  expect_gt(result$pvalue, 0)
  expect_lt(result$pvalue, 1)

  # Check if critical value 'q' is correctly calculated
  expected_q <- qt(1 - 0.05 / 2, 20 - 1)
  expect_equal(result$q, expected_q, tolerance = 1e-4)
})
