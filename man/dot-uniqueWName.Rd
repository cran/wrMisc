% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correctToUnique.R
\name{.uniqueWName}
\alias{.uniqueWName}
\title{Check regression arguments}
\usage{
.uniqueWName(
  x,
  splitSameName = TRUE,
  silent = TRUE,
  debug = FALSE,
  callFrom = NULL
)
}
\arguments{
\item{x}{(numeric or character vector) main input}

\item{splitSameName}{(logical)}

\item{silent}{(logical) suppress messages}

\item{debug}{(logical) additional messages for debugging}

\item{callFrom}{(character) allow easier tracking of messages produced}
}
\value{
vector like input
}
\description{
This function is an enhanced version of \code{unique}, names of elements are maintained
}
\examples{
aa <- c(a=11, b=12,a=11,d=14, c=11)
.uniqueWName(aa)
.uniqueWName(aa[-1]) # value repeated but different name
}
\seealso{
\code{\link[base]{unique}}
}
