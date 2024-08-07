% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeNRedMatr.R
\name{.sortMid}
\alias{.sortMid}
\title{Choose most frequent or middle of sorted vector}
\usage{
.sortMid(x, retVal = TRUE)
}
\arguments{
\item{x}{(numeric) main input}

\item{retVal}{(logical) return value of most frequent, if \code{FALSE} return index of (1st) 'x' for most frequent}
}
\value{
This function returns a numeric verctor
}
\description{
This function chooses the (first) most frequent or  middle of sorted vector, similar to the concept of \code{mode}
}
\examples{
.sortMid(11:14)
.sortMid(rep("b",3))
}
\seealso{
simple/partial functionality in \code{\link{summarizeCols}},  \code{\link{checkSimValueInSer}}
}
