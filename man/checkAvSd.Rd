% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/checkAvSd.R
\name{checkAvSd}
\alias{checkAvSd}
\title{Check how multiple groups of data separate or overlap based on mean +/- sd}
\usage{
checkAvSd(
  daAv,
  daSd,
  nByGr = NULL,
  multSd = 2,
  codeConst = "const",
  extSearch = FALSE,
  outAsLogical = TRUE,
  silent = FALSE,
  callFrom = NULL
)
}
\arguments{
\item{daAv}{matrix or data.frame}

\item{daSd}{matrix or data.frame}

\item{nByGr}{optinal specifying number of Elements per group, allows rather using SEM (adopt to variable n of different groups)}

\item{multSd}{(numeric) the factor specifyin how many sd values should be used as margin}

\item{codeConst}{(character) which term/word to use when specifying 'constant'}

\item{extSearch}{(logical) if TRUE, extend search to one group further (will call result 'nearUp' or 'nearDw')}

\item{outAsLogical}{to switch between 2col-output (separate col for 'up' and 'down') or simple categorical vector ('const','okDw','okUp')}

\item{silent}{(logical) suppress messages}

\item{callFrom}{(character) allow easier tracking of message(s) produced}
}
\value{
vector describing character as 'const' or 'okUp','okDw' (or if extSearch=TRUE 'nearUp','nearDw')
}
\description{
\code{checkAvSd} compares if/how neighbour groups separate/overlap via the 'engineering approach' (+/- 2 standard-deviations is similar to a=0.05 \code{t.test}).
This approach may be used as less elegant alternative to (multi-group) logistic regression.
The function uses 'daAv' as matrix of means (rows are tested for up/down character/progression) which get compared with boundaries taken from daSd (for Sd values of each mean in 'daAv').
}
\examples{
mat1 <- matrix(rep(11:24,3)[1:40],byrow=TRUE,ncol=8)
checkGrpOrderSEM(mat1,grp=gl(3,3)[-1])
checkAvSd(rowGrpMeans(mat1,gl(3,3)[-1]),rowGrpSds(mat1,gl(3,3)[-1]) )
# consider variable n :
checkAvSd(rowGrpMeans(mat1,gl(3,3)[-1]),rowGrpSds(mat1,gl(3,3)[-1]),nByGr=c(2,3,3)) 
}
\seealso{
\code{\link[wrMisc]{rowGrpMeans}}
}
