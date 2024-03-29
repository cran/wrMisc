% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nNonNumChar.R
\name{.setLowestTo}
\alias{.setLowestTo}
\title{Set lowest value to given value}
\usage{
.setLowestTo(x, setTo)
}
\arguments{
\item{x}{(numeric) main vector to be treated}

\item{setTo}{(numeric) replacement value}
}
\value{
This function returns a numeric vector
}
\description{
This function aims to set lowest value of x to value 'setTo'
}
\examples{
.setLowestTo(9:4, 6)
}
\seealso{
\code{\link[base]{nchar}}
}
