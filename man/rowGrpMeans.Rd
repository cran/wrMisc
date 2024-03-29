% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rowGrpMeans.R
\name{rowGrpMeans}
\alias{rowGrpMeans}
\title{rowMeans with destinction of groups (of columns, eg groups of replicates)}
\usage{
rowGrpMeans(x, grp, na.rm = TRUE)
}
\arguments{
\item{x}{matrix or data.frame}

\item{grp}{(character or factor) defining which columns should be grouped (considered as replicates)}

\item{na.rm}{(logical) a logical value indicating whether \code{NA}-values should be stripped before the computation proceeds.}
}
\value{
matrix with mean values
}
\description{
\code{rowGrpMeans} calculates column-means for matrix with multiple groups of data, ie similar to rowMeans but one mean for each group of data.
Groups are specified as columns of 'x' in 'grp' (so length of grp should match number of columns of 'x', NAs are allowed).
}
\examples{
set.seed(2016); dat1 <- matrix(c(runif(200) +rep(1:10,20)), ncol=10)
head(rowGrpMeans(dat1, gr=gl(4, 3, labels=LETTERS[1:4])[2:11]))
}
\seealso{
\code{\link{rowSds}}, \code{\link[base]{colSums}}
}
