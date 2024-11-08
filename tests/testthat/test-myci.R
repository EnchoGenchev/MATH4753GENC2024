test_that("myci works", {
  # Test with a single value (should return the same value for both bounds)
  single_value <- c(10)
  result <- myci(single_value)
  expect_equal(result, c(10, 10))

  # Test with a larger dataset (optional)
  larger_data <- rnorm(1000)  # 1000 random normal values
  result <- myci(larger_data)
  expect_type(result, "double")
  expect_length(result, 2)  # Should return a vector of length 2
})
