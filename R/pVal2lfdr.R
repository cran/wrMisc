#' Convert p-values to lfdr
#'
#' This function takes a numeric vector of p-values and returns a vector of lfdr-values (local false discovery) using 
#' the package \href{https://CRAN.R-project.org/package=fdrtool}{fdrtool}.
#' Multiple testing correction should be performed with caution, short series of p-values typically pose problems for transforming to lfdr. 
#' The transformation to lfdr values may give warning messages, in this case the resultant lfdr values may be invalid ! 
#' @param x (numeric) vector of p.values
#' @param silent (logical) suppres messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return (numeric) vector of lfdr values (or \code{NULL} if data insufficient to run the function 'fdrtool')
#' @seealso lfdr from \code{\link[fdrtool]{fdrtool}}, other p-adjustments (multiple test correction, eg FDR) in \code{\link[stats]{p.adjust}} 
#' @examples
#' ## Note that this example is too small for estimating really meaningful fdr values
#' ## In consequence, a warning will be issued.
#' set.seed(2017); t8 <- matrix(round(rnorm(160,10,0.4),2), ncol=8,
#'   dimnames=list(letters[1:20], c("AA1","BB1","CC1","DD1","AA2","BB2","CC2","DD2")))
#' t8[3:6,1:2] <- t8[3:6,1:2]+3   # augment lines 3:6 (c-f) for AA1&BB1
#' t8[5:8,5:6] <- t8[5:8,5:6]+3   # augment lines 5:8 (e-h) for AA2&BB2 (c,d,g,h should be found)
#' head(pVal2lfdr(apply(t8, 1, function(x) t.test(x[1:4], x[5:8])$p.value)))
#' @export
pVal2lfdr <- function(x, silent=TRUE, callFrom=NULL) {    ## take vector of p-values and return vector of lfdr-values
  fxNa <- .composeCallName(callFrom, newNa="pVal2lfdr")
  if(!isTRUE(silent)) silent <- FALSE
  if(!requireNamespace("fdrtool", quietly = TRUE)) {
    warning("package 'fdrtool' not found ! Please install from CRAN  ... (returning NULL)")
    return(NULL) 
  } else {
    if(sum(is.na(x)) >0 & !silent) message(fxNa," omitting ",sum(is.na(x))," NAs !")
    z <- as.numeric(naOmit(x)) 
    z <- try(fdrtool::fdrtool(z, statistic="pvalue", plot=FALSE, verbose=!silent)$lfdr)
    if(any(class(z) == "try-error")) { message(fxNa," FAILED to calulate lfdr !  Check how to use package 'fdrtool' (data too small ?)")
      return(NULL) 
    } else { z <- as.numeric(z)
      lfdr <- rep(NA,length(x))
      lfdr[!is.na(x)] <- z                          #  for returning NA at place of initial NAs
      if(!is.null(names(x))) names(lfdr) <- names(x)
      lfdr }}}
      
