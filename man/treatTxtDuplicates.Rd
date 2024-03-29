% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/treatTxtDuplicates.R
\name{treatTxtDuplicates}
\alias{treatTxtDuplicates}
\title{Locate duplicates in text and make non-redundant}
\usage{
treatTxtDuplicates(
  x,
  atEnd = TRUE,
  sep = "_",
  onlyCorrectToUnique = FALSE,
  silent = FALSE,
  debug = FALSE,
  callFrom = NULL
)
}
\arguments{
\item{x}{(character) vector with character-entries to identify (and remove) duplicates}

\item{atEnd}{(logical) decide location of placing the counter (at end or at beginning of ID) (see \code{\link{correctToUnique}})}

\item{sep}{(character) separator to add before counter when making non-redundant version}

\item{onlyCorrectToUnique}{(logical) if TRUE, return only vector of non-redundant}

\item{silent}{(logical) suppress messages}

\item{debug}{(logical) additional messages for debugging}

\item{callFrom}{(character) allow easier tracking of messages produced}
}
\value{
list with $init, $nRed, $nrLst
}
\description{
\code{treatTxtDuplicates} locates duplictes in character-vector 'x' and return list (length=3) : with $init (initial),
  $nRed .. non-redundant text by adding number at end or beginning, and $nrLst .. list-version with indexes per unique entry. 
 Note : NAs (if multiple) will be renamed to NA_1, NA_2
}
\examples{
treatTxtDuplicates(c("li0",NA,rep(c("li2","li3"),2)))
correctToUnique(c("li0",NA,rep(c("li2","li3"),2)))
}
\seealso{
For simple correction use \code{\link{correctToUnique}}
}
