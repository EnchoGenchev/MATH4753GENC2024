#' displays area under curve
#'
#' @importFrom graphics curve
#' @importFrom graphics polygon
#' @importFrom stats dnorm
#' @importFrom stats pnorm
#'
#'
#' @param mu mean of the normal distribution
#' @param sigma standard deviation of normal distribution
#' @param a value at which probability less than will be calculated
#'
#' @return shaded area, mean, standard deviation, and P(X < a)
#'
#' @examples
#' # Plotting and calculating probability for N(0, 1) where a = 1.5
#' myncurve(mu = 0, sigma = 1, a = 1.5)
#'
#' @export
myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve = seq(mu-3*sigma, a, length = 1000)
  ycurve = dnorm(xcurve, mu, sigma)
  polygon(c(mu-3*sigma,xcurve, a), c(0, ycurve, 0), col = 'red')

  prob = pnorm(a, mu, sigma)

  list(mu = mu, sigma = sigma, prob = prob)
}
