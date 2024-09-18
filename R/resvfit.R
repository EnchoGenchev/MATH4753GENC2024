#' plot residuals vs fitted values
#'
#' @param x a linear model object
#' @return plots residuals vs fitted values
#'
#' @export
resvfit <- function(x) {
  plot(residuals(x), fitted(x))
}
