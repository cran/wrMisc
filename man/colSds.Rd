% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colSds.R
\name{colSds}
\alias{colSds}
\title{sd for each column}
\usage{
colSds(dat)
}
\arguments{
\item{dat}{matrix (or data.frame) with numeric values (may contain NAs)}
}
\value{
numeric vector of sd values
}
\description{
\code{colSds} is a speed optimized \code{sd} for matrix or data.frames. 
It and treats each line as an independent set of data for calculating the sd (equiv to \code{apply(dat,1,sd)}).
NAs are ignored from data.
}
\examples{
set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)),nc=10)
colSds(dat1)
}
\seealso{
\code{\link[stats]{sd}}
}