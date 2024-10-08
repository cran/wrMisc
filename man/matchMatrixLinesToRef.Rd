% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matchMatrixLinesToRef.R
\name{matchMatrixLinesToRef}
\alias{matchMatrixLinesToRef}
\title{Match All Lines of Matrix To Reference Note}
\usage{
matchMatrixLinesToRef(
  mat,
  ref,
  exclCol = NULL,
  addRef = TRUE,
  inclInfo = FALSE,
  silent = FALSE,
  debug = FALSE,
  callFrom = NULL
)
}
\arguments{
\item{mat}{(matrix or data.frame) main input, all columns of \code{mat} will be tested for (partial) matching of \code{ref}}

\item{ref}{(character, length must match ) reference for trying to match each of the columns of  \code{mat}}

\item{exclCol}{(character or integer) column-name or -index of column to ignore/exclude when looking for matches}

\item{addRef}{(logical), if \code{TRUE} the content of \code{ref} will be added to  \code{mat} as additional column}

\item{inclInfo}{(logical) allows returning list with new matrix and additional information}

\item{silent}{(logical) suppress messages}

\item{debug}{(logical) additional messages for debugging}

\item{callFrom}{(character) allows easier tracking of messages produced}
}
\value{
This function returns the input matrix in an adjusted order (plus an optional additional column showing the reference)
 or if \code{inclInfo=TRUE} a list with $mat (adjusted matrix), $byColumn, $newOrder and $method;
 the reference can bee added as additional last column if \code{addRef=TRUE}
}
\description{
This function allows adjusting the order of lines of a matrix \code{mat} to a reference character-vector \code{ref},
even when initial direct matching of character-strings using \code{match} is not possible/successful.
In this case, various variants of using \code{grep} will be used to see if unambiguous matching is possible of characteristic parts of the text.
All columns of \code{mat} will be tested an the column giving the bes resuts will be used.
}
\details{
This function tests all columns of \code{mat} to find perfect matching results to the reference \code{ref}.
In case of multiple results the
In case no direct matching is possible, \code{grep} will be used to find the best partial matching.
The orderof the rows of input \code{mat} will be adjusted according to the matching results.

If \code{addRef=TRUE}, the reference will be included as additional column to the results, too.
}
\examples{
## Note : columns b and e allow non-ambigous match, not all elements of e are present in a
mat0 <- cbind(a=c("mvvk","axxd","bxxd","vv"),b=c("iwwy","iyyu","kvvh","gxx"), c=rep(9,4),
  d=c("hgf","hgf","vxc","nvnn"), e=c("_vv_","_ww_","_xx_","_yy_"))
matchMatrixLinesToRef(mat0[,1:4], ref=mat0[,5])
matchMatrixLinesToRef(mat0[,1:4], ref=mat0[1:3,5], inclInfo=TRUE)

matchMatrixLinesToRef(mat0[,-2], ref=mat0[,2], inclInfo=TRUE)   # needs 'reverse grep'
}
\seealso{
\code{\link[base]{match}},  \code{\link[base]{grep}}, \code{\link{trimRedundText}}, \code{\link{replicateStructure}}
}
