test_that("mymult works", {
  x <- mymult()
  expect_equal(is.matrix(x), TRUE)
  expect_equal(nrow(x), 4)
  expect_equal(ncol(x), 100)
})
