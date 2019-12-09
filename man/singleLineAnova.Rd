% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/singleLineAnova.R
\name{singleLineAnova}
\alias{singleLineAnova}
\title{2-factorial Anova on single line of data}
\usage{
singleLineAnova(dat, fac1, fac2, inclInteraction = TRUE)
}
\arguments{
\item{dat}{numeric vector}

\item{fac1}{(character or factor) vector describing grouping elements of dat for first factor, must be of same langth as fac2}

\item{fac2}{(character or factor) vector describing grouping elements of dat for second factor, must be of same langth as fac1}

\item{inclInteraction}{(logical) decide if factor-interactions (eg synergy) should be included to model}
}
\value{
(uncorrected) p for factor 'Pr(>F)' (see \code{\link[stats]{aov}})
}
\description{
\code{singleLineAnova} runs 2-factorial Anova on a single line of data (using \code{\link[stats]{aov}} from package \code{stats}) 
using a model with two factors (without factor-interaction) and extracts the correpsonding p-value.
}
\examples{
set.seed(2012); dat <- round(runif(8),1)
singleLineAnova(dat,gl(2,4),rep(1:2,4))
}
\seealso{
\code{\link[stats]{aov}}, \code{\link[stats]{anova}}; for repeated tests including \code{\link[limma]{eBayes}} see \code{\link{test2factLimma}}
}