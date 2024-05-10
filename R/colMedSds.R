#' Standard Error Of Median For Each Column By Bootstrap
#'
#' Determine standard error (sd) of median by bootstraping for multiple sets of data (rows in input matrix 'dat').
#' Note: The package \href{https://CRAN.R-project.org/package=boot}{boot} must be installed from CRAN.
#' @param dat (numeric) matix 
#' @param nBoot  (integer, length=1) number if iterations
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a (numeric) vector with estimated standard errors 
#' @seealso \code{\link[boot]{boot}}
#' @examples
#' set.seed(2016); dat1 <- matrix(c(runif(200) +rep(1:10,20)), ncol=10)
#' colMedSds(dat1) 
#' @export
colMedSds <- function(dat, nBoot=99, silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="colMedSds")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  out <- NULL
  if(length(dat) <1) { if(!silent) message(fxNa,"Argument 'dat' seems to be empty")
  } else {
    if(requireNamespace("boot", quietly=TRUE)) {
      median.fun <- function(dat,indices) stats::median(dat[indices], na.rm=TRUE)      
      chDi <- dim(dat)
      if(is.numeric(dat) && length(chDi) <1) dat <- list(dat)
      if(debug) {message(fxNa,"cMS2"); cMS2 <- list() }
      if(length(chDi) >1) {
        out <- if(chDi[1] >1) try(apply(dat, 2, function(x) stats::sd(boot::boot(data=x, statistic=median.fun, R=nBoot)$t)), silent=TRUE) else rep(NA, chDi[2])
      } else {
        if(is.list(dat)) { 
          out <- sapply(dat, function(x, nB) try(stats::sd(boot::boot(data=x, statistic=median.fun, R=nB)$t), silent=TRUE), nB=nBoot) 
          chO <- sapply(out, inherits, "try-error")
          if(all(chO)) out <- NULL else if(any(chO)) out[which(chO)] <- NA    
        }
      }
      if(inherits(out, "try-error")) { warning(fxNa,"Unable to run boot::boot()"); out <- NULL } 
    } else { out - NULL
      if(!silent) message(fxNa,"NOTE: Package 'boot' not found (please install first from CRAN), returning NULL")
    }
  }  
  out }


 
