% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cutAtMultSites.R
\name{cutAtMultSites}
\alias{cutAtMultSites}
\title{Cut character-vector at multiple sites}
\usage{
cutAtMultSites(y, cutAt)
}
\arguments{
\item{y}{character vector (better if of length=1, otherwise one won't know which fragment stems from which input)}

\item{cutAt}{(character) search subtsting, ie 'cutting rule'}
}
\value{
modified (ie cut) character vector
}
\description{
This function cuts character vector after 'cutAt' (ie keep the search subtsting 'cutAt', different to \code{strsplit}). 
Used for theoretical enzymatic digestion (eg in proteomics)
}
\examples{
tmp <- "MSVSRTMEDSCELDLVYVTERIIAVSFPSTANEENFRSNLREVAQMLKSKHGGNYLLFNLSERRPDITKLHAKVLEFGWPDLHTPALEKI"
cutAtMultSites(c(tmp,"ojioRij"),c("R","K"))
}
\seealso{
\code{\link[base]{strsplit}}, \code{\link[wrMisc]{nFragments0}}, \code{\link[wrMisc]{nFragments}}
}
