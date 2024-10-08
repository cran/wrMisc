% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rowSds.R
\name{rowSds}
\alias{rowSds}
\title{sd for each row (fast execution)}
\usage{
rowSds(dat, silent = FALSE, debug = FALSE, callFrom = NULL)
}
\arguments{
\item{dat}{matrix (or data.frame) with numeric values (may contain NAs which will be ignored)}

\item{silent}{(logical) suppress messages}

\item{debug}{(logical) additional messages for debugging}

\item{callFrom}{(character) allows easier tracking of messages produced}
}
\value{
numeric vector of sd values
}
\description{
This function is speed optimized \code{sd} per row of a matrix or data.frame and treats each row as independent set of data for sd (equiv to \code{apply(dat,1,sd)}).
NAs are ignored from data unless entire line NA). Speed improvements may be seen at more than 100 lines.
Note: NaN instances will be transformed to NA
}
\examples{
set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),ncol=10)
rowSds(dat1)
}
\seealso{
\code{\link[stats]{sd}}
}
