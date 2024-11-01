logbin2 <- function(theta) dbinom(4, size = 10, prob = theta, log = TRUE)

theta_vals <- seq(0.01, 0.99, by = 0.01)

test_that("mymaxlikg works", {
  result <- mymaxlikg(lfun = logbin2, theta = theta_vals)

  expect_type(result, "double")

  expect_true(result >= min(theta_vals) && result <= max(theta_vals))
})
