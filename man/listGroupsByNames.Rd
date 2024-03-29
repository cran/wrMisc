% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listGroupsByNames.R
\name{listGroupsByNames}
\alias{listGroupsByNames}
\title{Organize values into list and sort by names}
\usage{
listGroupsByNames(x, sep = ".", silent = FALSE, debug = FALSE, callFrom = NULL)
}
\arguments{
\item{x}{(list) main input}

\item{sep}{(character) separator (note that typcal separators may need to be protected, only automatically added for '.')}

\item{silent}{(logical) suppress messages}

\item{debug}{(logical) additional messages for debugging}

\item{callFrom}{(character) allow easier tracking of messages produced}
}
\value{
matrix or data.frame
}
\description{
Sort values of \code{'x'} by its names and organize as list by common names, the names until \code{'sep'} are used for (re)grouping. 
Note that typical spearators occuring the initial names may need protection by '\\' (this is automatically taken care of for the case of the dot ('.') separator).
}
\examples{
listGroupsByNames((1:10)/5)
ser1 <- 1:6; names(ser1) <- c("AA","BB","AA.1","CC","AA.b","BB.e")
listGroupsByNames(ser1)
}
\seealso{
\code{rbind} in \code{\link[base]{cbind}}
}
