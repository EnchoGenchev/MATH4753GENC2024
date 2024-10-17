test_that("myclt produces correct output", {
  result <- myclt(n = 10, iter = 100)

  expect_type(result, "double")

  expect_length(result, 100)
})
