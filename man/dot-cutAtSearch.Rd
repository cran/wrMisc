% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/extrSpcText.R
\name{.cutAtSearch}
\alias{.cutAtSearch}
\title{Search character-string and cut either before or after}
\usage{
.cutAtSearch(
  x,
  searchChar,
  after = TRUE,
  silent = TRUE,
  debug = FALSE,
  callFrom = NULL
)
}
\arguments{
\item{x}{character vector to be treated}

\item{searchChar}{(character) text to look for}

\item{after}{(logical)}

\item{silent}{(logical) suppress messages}

\item{debug}{(logical) additional messages for debugging}

\item{callFrom}{(character) allow easier tracking of messages produced}
}
\value{
This function returns a modified character vector
}
\description{
This function extracts/cuts text-fragments out of \code{txt} following specific anchors defined by arguments \code{cutFrom} and \code{cutTo}.
}
\examples{
.cutAtSearch("abcdefg","de")
}
\seealso{
\code{\link[base]{grep}}
}
