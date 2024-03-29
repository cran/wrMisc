% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairsAsPropensMatr.R
\name{pairsAsPropensMatr}
\alias{pairsAsPropensMatr}
\title{Convert Pairs of Node-Names to Non-Oriented Propensity Matrix}
\usage{
pairsAsPropensMatr(mat, silent = FALSE, debug = FALSE, callFrom = NULL)
}
\arguments{
\item{mat}{(matrix) main input, matrix of interaction partners with each line as a separate pair of nodes; 
the first two columns should contain identifiers of the nodes}

\item{silent}{(logical) suppress messages}

\item{debug}{(logical) additional messages for debugging}

\item{callFrom}{(character) allow easier tracking of messages produced}
}
\value{
This function returns matrix or data.frame
}
\description{
Numerous network query tools produce a listing of pairs of nodes (with one pair of nodes per line). 
Using this function such a \code{matrix} (or \code{data.frame}) can be combined to this more comprehensive view as propensity-matrix.
}
\details{
Note, this has been primarily developed for undirected interaction networks, the resulting propensity-matrix does not show any orientation any more.   
In a number of applications (eg in protein-protein interaction networks, PPI) the resulting matrix may be rather sparse.
}
\examples{
pairs3L <- matrix(LETTERS[c(1,3,3, 2,2,1)], ncol=2)    # loop of 3
(netw13pr <- pairsAsPropensMatr(pairs3L))              # as prop matr

}
\seealso{
uses typically input from \code{\link{filterNetw}}
}
