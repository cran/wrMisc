#' Compare means of two vectors by permutation test
#'
#' Run coin-flipping like permutation tests (to compare difference of 2 means: 'x1' and 'x2') without any distribution-assumptions.
#' Uses the package \href{https://CRAN.R-project.org/package=coin}{coin}.
#' 
#' @param x1 numeric vector (to be compared with vector 'x2')
#' @param x2 numeric vector (to be compared with vector 'x1')
#' @param orient (character) may be "two.sided","greater" or "less"
#' @param nPerm (integer) number of permutations
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return "MCp" class numeric output with p-values 
#' @seealso \code{oneway_test} in \code{\link[coin]{LocationTests}} 
#' @examples
#' coinPermTest(2,3)
#' @export
coinPermTest <- function(x1,x2,orient="two.sided",nPerm=5000,silent=FALSE,callFrom=NULL){
  tmp <- if(identical(orient,"greater")) coin::oneway_test(c(x1,x2) ~ factor(rep(c("A", "B"), c(length(x1),length(x2)))),
    alternative="greater", distribution=coin::approximate(B=nPerm)) else if(any(identical(orient,"fewer"),identical(orient,"less"))) {
      coin::oneway_test(c(x1,x2) ~ factor(rep(c("A", "B"), c(length(x1),length(x2)))),
      alternative="less", distribution=coin::approximate(B=nPerm)) } else {
      coin::oneway_test(c(x1,x2) ~ factor(rep(c("A", "B"), c(length(x1),length(x2)))), distribution=coin::approximate(B=nPerm)) }
  out <- coin::pvalue(tmp)
  out }
    