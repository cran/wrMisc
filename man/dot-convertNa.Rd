% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mergeW2.R
\name{.convertNa}
\alias{.convertNa}
\title{Convert/standardize names of 'query' to standard names from 'ref'}
\usage{
.convertNa(query, ref, partMatch = TRUE)
}
\arguments{
\item{query}{(matrix or data.frame, min 2 columns) main input}

\item{ref}{(list) list of multiple possible names associated to given group, reference name for each group is name of list}

\item{partMatch}{(logical) allows partial matching (ie name of 'ref' must be in head of 'query')}
}
\value{
This function returns a character vector
}
\description{
This function converts/standardizes names of 'query' to standard names from 'ref' (list of possible names (char vect) where names define standardized name).
It takes 'query' as character vector and return character vecor (same length as 'query') with 'converted/corrected' names
}
\examples{
daPa <- matrix(c(1:5,8,2:6,9), ncol=2)
}
