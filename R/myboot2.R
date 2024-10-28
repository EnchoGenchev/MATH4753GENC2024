#' Bootstrap Confidence Interval and Histogram of Sample Statistics
#'
#' @param iter Number of bootstrap iterations (default is 10000).
#' @param x A numeric vector of data to bootstrap from.
#' @param fun The function to be applied to each bootstrap sample (e.g., "mean", "median"). Default is "mean".
#' @param alpha The significance level for the confidence interval (default is 0.05).
#' @param cx Character expansion factor for text annotations (default is 1.5).
#' @param ... Additional graphical parameters passed to the `hist` function.
#'
#' @return A list with the following elements:
#' \item{ci}{The bootstrap confidence interval for the specified statistic.}
#' \item{fun}{The function applied to each bootstrap sample.}
#' \item{x}{The original data vector.}
#'
#'
#' @examples
#' # Bootstrap confidence interval for the mean of a sample
#' set.seed(123)
#' sample_data <- rnorm(100, mean = 50, sd = 10)
#' myboot2(iter = 5000, x = sample_data, fun = "mean", alpha = 0.05, col = "blue")
#'
#' @importFrom graphics segments text
#' @importFrom stats quantile
#'
#' @export
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n <- length(x)  # sample size

  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat <- apply(rs.mat, 2, fun)  # xstat is a vector and will have iter values in it
  ci <- quantile(xstat, c(alpha / 2, 1 - alpha / 2))  # Confidence interval

  para <- hist(xstat, freq = FALSE, las = 1,
               main = paste("Histogram of Bootstrap sample statistics", "\n", "alpha=", alpha, " iter=", iter, sep = ""),
               ...)

  mat <- matrix(x, nrow = length(x), ncol = 1, byrow = TRUE)
  pte <- apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "Black")
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)

  invisible(list(ci = ci, fun = fun, x = x))  # Some output to use if necessary
}
