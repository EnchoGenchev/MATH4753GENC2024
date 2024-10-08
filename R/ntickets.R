#' finds optimal number of plane tickets to sell
#'
#' @param N number of available seats
#' @param gamma probability of overbooking
#' @param p probability of passenger showing up
#'
#' @return list including nd, nc, N, p, and gamma
#'
#' @export
ntickets <- function(N, gamma, p) {

  n_vals <- seq(N, N * 1.5, length.out = 100) #range of plot

  discrete_vals <- 1 - gamma - pbinom(N, round(n_vals), p) #round because discrete

  mu_vals <- n_vals * p  #mean
  sigma_vals <- sqrt(n_vals * p * (1 - p)) #sd
  normal_vals <- 1 - gamma - pnorm(N + 0.5, mu_vals, sigma_vals) #objective function


  #discrete
  plot(n_vals, discrete_vals, type = "l",
       xlab = "Number of tickets sold (n)", ylab = "Objective function",
       main = "Objective vs n (discrete)",
       xlim = c(N, N * 1.1), ylim = range(c(discrete_vals)))
  points(n_vals, discrete_vals, pch = 19)  #add dots for discrete values
  abline(h = 0, col = "red", lty = 2)

  #find nd
  for (n in n_vals) {
    if (1 - gamma - pbinom(N, round(n), p) <= 0) {
      nd <- n
    }
  }

  #vertical line at nd
  abline(v = nd, col = "red", lty = 2)


  #continuous case
  plot(n_vals, normal_vals, type = "l",
       xlab = "Number of tickets sold (n)", ylab = "Objective function",
       main = "Objective vs n (continuous)",
       xlim = c(N, N * 1.1), ylim = range(c(normal_vals)))
  abline(h = 0, col = "red", lty = 2)

  #find nc
  for (n in n_vals) {
    mu <- n * p
    sigma <- sqrt(n * p * (1 - p))
    if (1 - gamma - pnorm(N, mu, sigma) <= 0) {
      nc <- n
    }
  }

  #vertical line at nc
  abline(v = nc, col = "red", lty = 2)

  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
