% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/colorAccording2.R
\name{.chooseGrpCol}
\alias{.chooseGrpCol}
\title{Automatic choice of colors}
\usage{
.chooseGrpCol(nGrp, paired = FALSE, alph = 0.2)
}
\arguments{
\item{nGrp}{(numeric vector) main input}

\item{paired}{(logical)}

\item{alph}{(numeric vector)}
}
\value{
character vector with color codes
}
\description{
This function allows to do automatic choice of colors: if single-> grey, if few -> RColorBrewer, if many : gradient green -> grey/red
}
\examples{
.chooseGrpCol(4)	
}
\seealso{
\code{\link[grDevices]{rgb}}; \code{\link{colorAccording2}}
}
