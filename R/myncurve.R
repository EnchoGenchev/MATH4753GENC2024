#' displays area under curve
#'
#' @param a value at which probability less than will be calculated
#' @param mu mean of the normal distribution
#' @param sigma standard deviation of normal distribution
#'
#' @importFrom graphics curve
#' @importFrom graphics polygon
#' @importFrom stats dnorm
#' @importFrom stats pnorm
#'
#' @return shaded area, mean, standard deviation, and P(X < a)
#'
#' @export
myncurve = function(a, mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve = seq(mu-3*sigma, a, length = 1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(mu-3*sigma,xcurve, a), c(0, ycurve, 0), col = 'red')

  prob = pnorm(a, mu, sigma)

  list(mu = mu, sigma = sigma, prob = prob)
}
