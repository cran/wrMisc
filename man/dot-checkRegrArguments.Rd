% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adjBy2ptReg.R
\name{.checkRegrArguments}
\alias{.checkRegrArguments}
\title{Check regression arguments}
\usage{
.checkRegrArguments(inData, refList, regreTo, callFrom = NULL)
}
\arguments{
\item{inData}{(numeric vector) main input}

\item{refList}{(list)}

\item{regreTo}{(numeric vector)}

\item{callFrom}{(character) allow easier tracking of messages produced}
}
\value{
list
}
\description{
This function allows to check arguments for linear regression. Used as argument checking for \code{regrBy1or2point} and \code{regrMultBy1or2point}
}
\examples{
.datSlope(c(3:6))
}
\seealso{
\code{\link[base]{append}}; \code{\link{lrbind}}
}
