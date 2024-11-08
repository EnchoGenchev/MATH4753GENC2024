#' Calculate a 95 percent Confidence Interval for the Mean
#'
#' This function calculates a 95 percent confidence interval for the mean of a single sample.
#'
#' @param x A numeric vector representing the sample data.
#'
#' @return A numeric vector of length 2 containing the lower and upper bounds of the 95% confidence interval.
#'
#' @importFrom stats sd qt
#'
#' @examples
#' sample_data <- c(5, 10, 15, 20, 25)
#' myci(sample_data)
#'
#' @export
myci <- function(x) {
  n <- length(x)                   # Sample size

  # Special case for a single data point
  if (n == 1) {
    return(c(x, x))  # If there's only one value, the CI is the value itself
  }

  mean_x <- mean(x)                # Sample mean
  se <- sd(x) / sqrt(n)            # Standard error of the mean

  # Use qt for a t-distribution based 95% confidence interval
  t_value <- qt(1 - 0.05 / 2, df = n - 1)

  lower_bound <- mean_x - t_value * se   # Lower bound
  upper_bound <- mean_x + t_value * se   # Upper bound

  c(lower_bound, upper_bound)            # Return the interval
}
