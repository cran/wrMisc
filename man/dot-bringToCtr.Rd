% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ratioAllComb.R
\name{.bringToCtr}
\alias{.bringToCtr}
\title{Bring most extreme to center}
\usage{
.bringToCtr(aa, ctr, ctrFa = 0.75)
}
\arguments{
\item{aa}{(numeric) main input}

\item{ctr}{(numeric) 'control'}

\item{ctrFa}{(numeric <1) modulate amplitude of effect}
}
\value{
This function returns an adjusted numeric vector
}
\description{
This function aims to bring most extreme value to center
}
\examples{
.bringToCtr(11:14, 9)
}
\seealso{
\code{\link[stats]{dist}}
}
