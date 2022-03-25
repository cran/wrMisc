#' Standard error of median for each column by bootstrap
#'
#' Determine standard error (sd) of median by bootstraping for multiple sets of data (rows in input matrix 'dat').
#' Note: The package \href{https://CRAN.R-project.org/package=boot}{boot} must be installed from CRAN.
#' @param dat (numeric) matix 
#' @param nBoot  (integer) number if iterations
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a (numeric) vector with estimated standard errors 
#' @seealso \code{\link[boot]{boot}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200) +rep(1:10,20)), ncol=10)
#' colMedSds(dat1) 
#' @export
colMedSds <- function(dat, nBoot=99, silent=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="colMedSds")
  if(!isTRUE(silent)) silent <- FALSE  
  msg <- "'dat' should be matrix or data.frame with "
  if(length(dat) <1) { warning(fxNa,"Argument 'dat' seems to be empty !"); out <- NULL
  } else {
    if(is.null(ncol(dat))) stop(msg,"multiple columns !") else if(ncol(dat) < 2) stop(msg,"at least 2 columns !")
    chPa <- requireNamespace("boot", quietly=TRUE)
    if(!chPa) { out - NULL
      warning(fxNa,"Package 'boot' not found (please install first from CRAN), returning NULL")
    } else {
      median.fun <- function(dat,indices) stats::median(dat[indices], na.rm=TRUE)
      out <- try(apply(dat, 2, function(x) stats::sd(boot::boot(data=x, statistic=median.fun, R=nBoot)$t)), silent=TRUE)
      if(inherits(out, "try-error")) { warning(fxNa,"Unable to run boot::boot()"); out <- NULL } } }
  out }
 
