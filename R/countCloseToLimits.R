#' Count from two vectors number of values close within given limits
#'
#' This functions summarizes the serach of similar (or identical) numeric values from 2 initial vectors, it 
#' evaluates the result from initial search run by findCloseMatch(), whose output is a less convenient list.
#' \code{countCloseToLimits} checks furthermore how many results within additional (more stringent)
#' distance-limits may be found and returns the number of distance values within the limits tested.
#' Designed for checking if threshold used with findCloseMatch() may be set more stringent, eg  when searching reasonable FDR limits ...
#'
#' @param closeMatch (list) output from findCloseMatch(), ie list indicating which instances of 2 series of data have close matches
#' @param limitIdent (numeric) max limit or panel of threshold values to test (if single value, in addtion a panel with values below will be tested)
#' @param prefix (character)  prefix for names of output
#' @return integer vector with counts for number of list-elements with at least one absolue value below threshold, names
#' @seealso \code{\link[wrMisc]{findCloseMatch}}
#' @examples
#' set.seed(2019); aa <- sample(12:15,20,repl=TRUE) +round(runif(20),2)-0.5
#' bb <- 11:18
#' match1 <- findCloseMatch(aa,bb,com="diff",lim=0.65)
#' head(match1)
#' (tmp3 <- countCloseToLimits(match1,lim=c(0.5,0.35,0.2)))
#' (tmp4 <- countCloseToLimits(match1,lim=0.7))
#' @export
countCloseToLimits <- function(closeMatch,limitIdent=5,prefix="lim_") {
  limitIdent <- unique(limitIdent)
  if(length(limitIdent) ==1) {x <- floor(log10(signif(limitIdent,1)))
    x <- c(10^c((x-1):x),10^c((x-1):x)/5,4*round(seq(limitIdent/40,limitIdent/4,length.out=20),2),limitIdent)     # default series of limits
    limitIdent <- unique(signif(sort(x),digits=5)) }
  if(length(closeMatch) <1 | length(limitIdent) <1) { out <- rep(NA,length(limitIdent))
  } else {
    out <- rowSums(sapply(closeMatch,function(z) (min(abs(z)) <= limitIdent)))}
  if(is.null(names(out))) names(out) <- paste(prefix,limitIdent,sep="")  
  out }
  
