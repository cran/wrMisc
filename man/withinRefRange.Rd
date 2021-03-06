% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/withinRefRange.R
\name{withinRefRange}
\alias{withinRefRange}
\title{Check for values within range of reference}
\usage{
withinRefRange(x, fa, ref = NULL, absRef = TRUE, asInd = FALSE)
}
\arguments{
\item{x}{matrix or data.frame}

\item{fa}{(numeric) absolute or relative tolerance value (numeric, length=1), interpreted according to 'absRef' as absolute or relative to 'x'(ie fa*ref)}

\item{ref}{(numeric) (center) reference value for comparison (numeric, length=1), if not given mean of 'x' (excluding NA or non-finite values) will be used}

\item{absRef}{(logical) return result as absolute or relative to 'x'(ie fa*ref)}

\item{asInd}{(logical) if TRUE return index of which values of 'x' are within range, otherwise return values if 'x' within range}
}
\value{
numeric vector (containing only the values within range of reference)
}
\description{
\code{withinRefRange} checks which values of numeric vector 'x' are within range +/- 'fa' x 'ref' (ie within range of reference).
}
\examples{
## within 2.5 +/- 0.7
withinRefRange(-5:6,fa=0.7,ref=2.5)                
## within 2.5 +/- (0.7*2.5)
withinRefRange(-5:6,fa=0.7,ref=2.5,absRef=FALSE)
}
