% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/makeNRedMatr.R
\name{makeNRedMatr}
\alias{makeNRedMatr}
\title{Make non-redundant matrix}
\usage{
makeNRedMatr(
  dat,
  summarizeRedAs,
  iniID = "iniID",
  retDataFrame = TRUE,
  nEqu = FALSE,
  silent = FALSE,
  debug = FALSE,
  callFrom = NULL
)
}
\arguments{
\item{dat}{(matrix or data.frame) main input for making non-redundant}

\item{summarizeRedAs}{(character) summarization method(s), typical choices 'median','mean','min' or 'maxOfRef',
'maxAbsOfRef' for summarizing according to 1 specified column, may be single method for all or different method for each column (besides col 'iniID') or special method looking at column (if found, first of special methods used, everything else not considered).}

\item{iniID}{(character) column-name used as initial ID (default="iniID"), ie reference for determining groups of redundant lines}

\item{retDataFrame}{(logical) if \code{TRUE}, check if text-columns may be converted to data.frame with numeric}

\item{nEqu}{(logical) if \code{TRUE}, add additional column indicating the number of equal lines for choice (only with min or max)}

\item{silent}{(logical) suppress messages}

\item{debug}{(logical) additional messages for debugging}

\item{callFrom}{(character) allows easier tracking of messages produced}
}
\value{
This function returns a (numeric) matrix or data.frame with summarized data and add'l col with number of initial redundant lines
}
\description{
\code{makeNRedMatr} takes matrix or data.frame 'dat' to summarize redundant lines (column argument \code{iniID}) along method specified in \code{summarizeRedAs} 
to treat all lines with redundant \code{iniID} by same approach (ie for all columns the line where specified column is at eg max = 'maxOfRef' ).
If no name given, the function will take the last numeric (factors may be used - they will be read as levels).
}
\details{
When using for selection of single initial line give the character-string of argument \code{summarizeRedAs} a name (eg  \code{summ=c(X1="minOfRef")}
 so that the function will use ONLY the column specified via the name for determining which line should be used/kept.    

This function has been developed using Proline version 1.6.1 coupled with MS-Angel 1.6.1.
}
\examples{
t3 <- data.frame(ref=rep(11:15,3),tx=letters[1:15],
  matrix(round(runif(30,-3,2),1),nc=2),stringsAsFactors=FALSE)
by(t3,t3[,1],function(x) x)
t(sapply(by(t3,t3[,1],function(x) x), summarizeCols, me="maxAbsOfRef"))
# calculate mean for lines concerened of all columns :
(xt3 <- makeNRedMatr(t3, summ="mean", iniID="ref"))
# choose lines based only on content of column 'X1' (here: max):
(xt3 <- makeNRedMatr(t3, summ=c(X1="maxOfRef"), iniID="ref")) 
}
\seealso{
simple/partial functionality in \code{\link{summarizeCols}},  \code{\link{checkSimValueInSer}}
}
