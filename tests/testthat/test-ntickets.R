test_that("ntickets works", {
  result <- ntickets(100, 0.1, 0.9)
  expect_equal(names(result), c("nd", "nc", "N", "p", "gamma"))
  expect_true(result$nd >= 100)
  expect_true(result$nc >= 100)
})

