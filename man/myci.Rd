% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myci.R
\name{myci}
\alias{myci}
\title{Calculate a 95 percent Confidence Interval for the Mean}
\usage{
myci(x)
}
\arguments{
\item{x}{A numeric vector representing the sample data.}
}
\value{
A numeric vector of length 2 containing the lower and upper bounds of the 95% confidence interval.
}
\description{
This function calculates a 95 percent confidence interval for the mean of a single sample.
}
\examples{
sample_data <- c(5, 10, 15, 20, 25)
myci(sample_data)

}
