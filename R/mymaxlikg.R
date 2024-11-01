#' mymaxlikg: Graphical Maximum Likelihood Estimation
#'
#' @param lfun A log-likelihood function
#' @param theta vector of theta values
#'
#' @return The function returns the theta value that maximizes the likelihood and produces a plot
#'
#' @examples
#' logbin2 <- function(theta) dbinom(4, size = 10, prob = theta, log = TRUE)
#' theta_vals <- seq(0, 1, by = 0.01)
#' mymaxlikg(lfun = logbin2, theta = theta_vals)
#'
#' @importFrom graphics axis
#'
#' @export
mymaxlikg <- function(lfun = "logbin2", theta) {
  nth <- length(theta)  # Number of values in theta
  thmat <- matrix(theta, nrow = nth, ncol = 1, byrow = TRUE) # Matrix of theta
  z <- apply(thmat, 1, lfun) # Log likelihood values
  zmax <- max(which(z == max(z)))  # Index of the max likelihood

  plot(theta, exp(z), type = "l") # Plot likelihood
  abline(v = theta[zmax], col = "Blue")   # Vertical line at max likelihood
  axis(3, theta[zmax], round(theta[zmax], 4))  # Tick mark on the third axis
  theta[zmax]   # Theta corresponding to max likelihood
}

