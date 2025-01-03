% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myclt.R
\name{myclt}
\alias{myclt}
\title{myclt: Central Limit Theorem Simulation for Uniform Distribution}
\usage{
myclt(n, iter, a = 0, b = 5)
}
\arguments{
\item{n}{number of random variables}

\item{iter}{number of iterations}

\item{a}{lower bound of the uniform distribution}

\item{b}{upper bound of the uniform distribution}
}
\value{
histogram containing the sum of the `n` uniform random variables for each iteration
}
\description{
myclt: Central Limit Theorem Simulation for Uniform Distribution
}
\examples{
myclt(n = 10, iter = 1000, a = 0, b = 5)

}
