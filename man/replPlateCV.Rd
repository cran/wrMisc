% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replPlateCV.R
\name{replPlateCV}
\alias{replPlateCV}
\title{CV of replicate plates (list of matrixes)}
\usage{
replPlateCV(lst, callFrom = NULL)
}
\arguments{
\item{lst}{list of matrixes : suppose lines are independent elements, colums are replicates of the 1st column. All matrixes must have same dimensions}

\item{callFrom}{(character) allows easier tracking of messages produced}
}
\value{
matrix or array (1st dim is intraplate-position, 2nd .. plate-group/type, 3rd .. channels)
}
\description{
\code{replPlateCV} gets CVs of replicates from list of 2 or 3-dim arrays (where 2nd dim is replicates, 3rd dim may be channel). 
Note : all list-elements of must MUST have SAME dimensions !
When treating data from microtiter plates (eg 8x12) data are typically spread over multiple plates, ie initial matrixes that are the organized into arrays.
Returns matrix or array (1st dim is intraplate-position, 2nd .. plate-group/type, 3rd .. channels)
}
\examples{
set.seed(2016); ra1 <- matrix(rnorm(3*96),nrow=8)
pla1 <- list(ra1[,1:12],ra1[,13:24],ra1[,25:36])
replPlateCV(pla1)
arrL1 <- list(a=array(as.numeric(ra1)[1:192],dim=c(8,12,2)),
  b=array(as.numeric(ra1)[97:288],dim=c(8,12,2)))
replPlateCV(arrL1)
}
\seealso{
\code{\link{rowCVs}}, @seealso \code{\link{arrayCV}}
}
