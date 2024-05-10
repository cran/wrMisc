#' Estimate sd Of Median For Each Row By Bootstrap
#'
#' This function determines the stand error (sd) of the median for each row by bootstraping each row of 'dat'.
#' Note: requires package \href{https://CRAN.R-project.org/package=boot}{boot}
#'
#' @param dat (numeric) matix, main input 
#' @param nBoot (integer) number if iterations for bootstrap
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of messages produced
#' @param debug (logical) display additional messages for debugging
#' @return This functions returns a (numeric) vector with estimated sd values
#' @seealso For a more flexible version able to handle lists please look at \code{\link{colMedSds}} ,  based on \code{\link[boot]{boot}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200)+rep(1:10,20)), ncol=10)
#' rowMedSds(dat1) ; plot(rowSds(dat1), rowMedSds(dat1))
#' @export
rowMedSds <- function(dat, nBoot=99, silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="rowMedSds")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  msg <- "'dat' should be matrix or data.frame with "
  if(is.null(ncol(dat))) stop(fxNa,msg,"multiple columns !") else if(ncol(dat) < 2) stop(msg,"at least 2 columns !")
  if(requireNamespace("boot", quietly=TRUE)) {
    median.fun <- function(dat,indices) stats::median(dat[indices],na.rm=TRUE)
    out <- try(apply(dat,1,function(x) stats::sd(boot::boot(data=x, statistic=median.fun, R=nBoot)$t)))
    if(inherits(out, "try-error")) stop(fxNa,"Did not succeed in running boot()")
    out 
  } else if(!silent) message(fxNa, "NOTE : Package 'boot' not found ! Please install first from CRAN")   
  }
   
