% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rnormW.R
\name{rnormW}
\alias{rnormW}
\title{Normal random number generation with close fit to expected mean and sd}
\usage{
rnormW(
  n,
  mean = 0,
  sd = 1,
  seed = NULL,
  digits = 8,
  silent = FALSE,
  callFrom = NULL
)
}
\arguments{
\item{n}{(integer, length=1)  number of observations. If \code{length(n) > 1}, the length is taken to be the number required.}

\item{mean}{(numeric, length=1) expected mean}

\item{sd}{(numeric, length=1) expected sd}

\item{seed}{(integer, length=1) seed for generating random numbers}

\item{digits}{(integer, length=1 or \code{NULL}) number of significant digits for output, set to \code{NULL} to get all digits}

\item{silent}{(logical) suppress messages}

\item{callFrom}{(character) allow easier tracking of messages produced}
}
\value{
This function returns a numeric vector of random values
}
\description{
This function allows creating a vector of random values similar to \code{rnorm}, but resulting value get recorrected to fit to expected mean and sd.
When the number of random values to generate is low, the mean and sd of the resultant values may deviate from the expected mean and sd when using the standard \code{rnorm} function.
In such cases the function \code{rnormW} helps getting much closer to the expected mean and sd.
}
\details{
For making result reproducible, a seed for generating random numbers can be set via the argument \code{seed}.
However, with \code{n=2} the resulting values are 'fixed' since no random component is possible at n <3.
}
\examples{
x1 <- (11:16)[-5]
mean(x1); sd(x1)
## the standard way
ra1 <- rnorm(n=length(x1), mean=mean(x1), sd=sd(x1))
## typically the random values deviate (slightly) from expected mean and sd
mean(ra1) -mean(x1) 
sd(ra1) -sd(x1)
## random numbers with close fit to expected mean and sd :
ra2 <- rnormW(length(x1), mean(x1), sd(x1))
mean(ra2) -mean(x1) 
sd(ra2) -sd(x1)          # much closer to expected value
}
\seealso{
\code{\link[stats]{Normal}}
}
