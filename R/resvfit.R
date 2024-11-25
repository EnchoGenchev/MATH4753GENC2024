#' plot residuals vs fitted values
#'
#' @param x a linear model object
#' @return plots residuals vs fitted values
#' @examples
#' model <- lm(mpg ~ wt + hp, data = mtcars)
#' resvfit(model)
#'
#' @export
resvfit <- function(x) {
  plot(residuals(x), fitted(x))
}
