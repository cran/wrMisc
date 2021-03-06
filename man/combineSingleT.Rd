% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/combineSingleT.R
\name{combineSingleT}
\alias{combineSingleT}
\title{Get all combinations with TRUE from each column}
\usage{
combineSingleT(mat)
}
\arguments{
\item{mat}{2-column matrix of logical values}
}
\value{
matrix with indexes of conbinations of \code{TRUE}
}
\description{
This function addresses the case when multiple alternatove ways exit to combine two elements. 
\code{combineSingleT} makes combinatory choices : if multiple \code{TRUE} in given column of 'mat' make all multiple selections with always one \code{TRUE} from each column
The resultant output contains index for first and second input columns elements to be combined.
}
\examples{
## Example: Fist column indicates which boys want to dance and second column  
## which girls want to dance. So if several boys want to dance each of the girls 
## will have the chance to dance with each of them.  
matr <- matrix(c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE),ncol=2)
combineSingleT(matr)
}
