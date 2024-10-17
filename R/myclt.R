#' myclt: Central Limit Theorem Simulation for Uniform Distribution
#'
#' @importFrom stats runif
#' @importFrom graphics hist
#'
#' @param n number of random variables
#' @param iter number of iterations
#' @param a lower bound of the uniform distribution
#' @param b upper bound of the uniform distribution
#'
#' @return vector containing the sum of the `n` uniform random variables for each iteration and a histogram
#'
#' @examples
#' myclt(n = 10, iter = 1000, a = 0, b = 5)
#'
#' @export
myclt=function(n,iter,a=0,b=5){
  y=runif(n*iter,a,b)
  data = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  sm=apply(data,2,sum)
  h=hist(sm,plot=FALSE)
  hist(sm,col=rainbow(length(h$mids)),freq=FALSE,main="Distribution of the sum of uniforms")
  curve(dnorm(x,mean=n*(a+b)/2,sd=sqrt(n*(b-a)^2/12)),add=TRUE,lwd=2,col="Blue")
  sm
}
