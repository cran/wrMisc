% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cbindNR.R
\name{cbindNR}
\alias{cbindNR}
\title{cbind to non-redundant}
\usage{
cbindNR(
  ...,
  convertDFtoMatr = TRUE,
  sortOutput = TRUE,
  summarizeAs = "sum",
  silent = FALSE,
  callFrom = NULL
)
}
\arguments{
\item{...}{all matrixes to get combined in cbind way}

\item{convertDFtoMatr}{(logical) decide if output should be converted to matrix}

\item{sortOutput}{(logical) optional sorting by column-names}

\item{summarizeAs}{(character) decide of combined values should get summed (default, 'sum') or averaged ('mean')}

\item{silent}{(logical) suppress messages}

\item{callFrom}{(character) allow easier tracking of messages produced}
}
\value{
This function returns a matrix or data.frame (as cbind would return)
}
\description{
\code{cbindNR} combines all matrixes given as arguments to non-redundant column names (by ADDING the number of 'duplicated' columns !). 
Thus, this function works similar to \code{cbind}, but allows combining multiple matrix-objects containing redundant column-names.
Of course, all input-matrixes must have the same number of rows !
By default, the output gets sorted by column-names.
Note, due to the use of '...' arguments must be given by their full argument-names, lazy evaluation might not recognize properly argument names.
}
\examples{
ma1 <- matrix(1:6, ncol=3, dimnames=list(1:2,LETTERS[3:1]))
ma2 <- matrix(11:16, ncol=3, dimnames=list(1:2,LETTERS[3:5]))
cbindNR(ma1, ma2)
cbindNR(ma1, ma2, summarizeAs="mean")
}
\seealso{
\code{\link[base]{cbind}}, \code{\link{nonAmbiguousNum}}, \code{\link{firstOfRepLines}}
}
