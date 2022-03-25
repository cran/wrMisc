#' Compare means of two vectors by permutation test
#'
#' Run coin-flipping like permutation tests (to compare difference of 2 means: 'x1' and 'x2') without any distribution-assumptions.
#' This function uses the package \href{https://CRAN.R-project.org/package=coin}{coin}, if not installed, the function will return NULL and give a warning.
#' 
#' @param x1 numeric vector (to be compared with vector 'x2')
#' @param x2 numeric vector (to be compared with vector 'x1')
#' @param orient (character) may be "two.sided","greater" or "less"
#' @param nPerm (integer) number of permutations
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This  function returns an object of "MCp" class numeric output with p-values 
#' @seealso \code{oneway_test} in \code{\link[coin]{LocationTests}} 
#' @examples
#' coinPermTest(2, 3, nPerm=200)
#' @export
coinPermTest <- function(x1, x2, orient="two.sided", nPerm=5000, silent=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="coinPermTest")
  if(!requireNamespace("coin", quietly=TRUE)) {
    warning(fxNa,"Package 'coin' not found, please install from CRAN; retruning NULL")
  } else {  
    tmp <- if(identical(orient,"greater")) try(coin::oneway_test(c(x1,x2) ~ factor(rep(c("A","B"), c(length(x1), length(x2)))),
      alternative="greater", distribution=coin::approximate(B=nPerm)), silent=TRUE) else if(any(identical(orient, "fewer"), identical(orient, "less"))) {
        try(coin::oneway_test(c(x1, x2) ~ factor(rep(c("A", "B"), c(length(x1), length(x2)))),
          alternative="less", distribution=coin::approximate(B=nPerm)), silent=TRUE) } else {
        try(coin::oneway_test(c(x1, x2) ~ factor(rep(c("A", "B"), c(length(x1), length(x2)))), distribution=coin::approximate(B=nPerm)),silent=TRUE) }
    if(inherits(tmp, "try-error")) { warning(fxNa,"Unable to run coin::oneway_test() !"); out <- NULL    
    } else out <- coin::pvalue(tmp)
    out } }
    
