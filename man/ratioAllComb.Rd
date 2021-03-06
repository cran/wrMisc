% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ratioAllComb.R
\name{ratioAllComb}
\alias{ratioAllComb}
\title{Calculate all ratios between x and y}
\usage{
ratioAllComb(
  x,
  y,
  maxLim = 10000,
  isLog = FALSE,
  silent = FALSE,
  callFrom = NULL
)
}
\arguments{
\item{x}{(numeric) vector, numerator for constructing rations}

\item{y}{(numeric) vector, denominator for constructing rations}

\item{maxLim}{(integer) allows reducing complexity by drawing for very long x or y}

\item{isLog}{(logical) adjust ratio calculation to log-data}

\item{silent}{(logical) suppress (less important) messages}

\item{callFrom}{(character) allow easier tracking of message(s) produced}
}
\value{
(numeric) vector with all ratios
}
\description{
\code{ratioAllComb} calculates all possible pairwise ratios between all individual calues of x and y.
}
\examples{
set.seed(2014); ra1 <- c(rnorm(9,2,1),runif(8,1,2))
ratioAllComb(ra1[1:9],ra1[10:17])
boxplot(list(norm=ra1[1:9],unif=ra1[10:17],rat=ratioAllComb(ra1[1:9],ra1[10:17])))
}
