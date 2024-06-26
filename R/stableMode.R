#' Estimate Mode (Most Frequent Value)   
#' 
#' Estimate mode, ie most frequent value. In case of continuous numeric data, the most frequent values may not be the most frequently repeated exact term.
#' This function offers various approches to estimate the mode of a numeric vector. 
#' Besides, it can also be used to identify the most frequentexact term (in this case also from character vectors). 
#'
#' @details
#' The argument \code{method} allows to choose among (so far) 4 different methods available.
#' If "density" is chosen, the most dense region of sqrt(n) values will be chosen; 
#' if "binning", the data will be binned (like in histograms) via rounding to a user-defined number of significant values ("rangeSign").
#' If \code{method} is set to "BBmisc", the function \code{computeMode()} from package \href{https://CRAN.R-project.org/package=BBmisc}{BBmisc} will be used.
#' If "mode" is chosen, the first most frequently occuring (exact) value will be returned, if "allModes", all ties will be returned. This last mode also works with character input. 
#'	 
#' @param x (numeric, or character if 'method='mode') data to find/estimate most frequent value
#' @param method (character) There are 3 options : BBmisc, binning and density (default). If "binning" the function will search context dependent, ie like most frequent class of histogram.
#'  Using "binning" mode the search will be refined if either 80 percent of values in single class or >50 percent in single class.
#' @param finiteOnly (logical) suppress non-finite values; allows avoiding \code{NULL} as result in presence of some \code{Inf} values; \code{NA} will be ignored in any case
#' @param bandw (integer) only used when \code{method="binning"} or  \code{method="density"} : defines the number of points to look for density or number of classes used; 
#'  very "critical" parameter, may change results in strong way. Note: with \code{method="binning"}: At higher values for "bandw" you will finally loose advantage of histLike-type search of mode !
#' @param rangeSign (integer) only used when \code{method="binning"}: range of numbers used as number of significant values

#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of messages produced
#' @param debug (logical) additional messages for debugging 
#' @return This function returns a numeric vector with value of mode, the name of the value indicates it's position
#' @seealso \code{computeMode()} in package \href{https://CRAN.R-project.org/package=BBmisc}{BBmisc}
#' @examples
#' set.seed(2012); dat <- round(c(rnorm(50), runif(100)),3)
#' stableMode(dat)
#' @export
stableMode <- function(x, method="density", finiteOnly=TRUE, bandw=NULL, rangeSign=1:6, silent=FALSE, callFrom=NULL, debug=FALSE) {
  ## stable mode  
  fxNa <- .composeCallName(callFrom, newNa="stableMode")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  ## prepare data: treat NA or non-finite values
  if(finiteOnly) { chFin <- is.finite(x)
    if(all(!chFin)) x <- NULL else if(any(!chFin)) {x <- x[which(chFin)]
      if(!silent) message(fxNa,"Removing ",sum(!chFin)," (out of ",length(chFin),") non-finite values") }
  } else x <- naOmit(x)
  if(length(unique(x)) < 0) {
    method <- NULL
    return(NULL)
    if(!silent)  message(fxNa, "NO numeric values (nothing to do)") 
  } else if(length(unique(x)) == 1) {
    method <- NULL
    return(x[1])
    if(!silent && length(x) >1) message(fxNa, "All values are the same (= mode)") }
  if(identical(method, "dens"))  method <- "density"
  if(identical(method, "bin")) method <- "binning"
  if(identical(method, "histLike")) {
    method <- "binning"
    if(!silent) message(fxNa, "Note: argument option 'histLike' has been depreciated and replaced by 'binning'") }
  out <- NULL
  isNum <- is.numeric(x)
  ## check type of input
  if(any(sapply(c("BBmisc","density","binning"), identical, method)) && !isNum) {    
    chNum <- try(as.numeric(if(is.factor(x)) as.character(x) else x), silent=TRUE)
    if(inherits(chNum, "try-error")) {
      if(!silent) message(fxNa,"Note : Input is NOT numeric, not compatible with method chosen, thus setting method='mode' !")
     method <- "mode"
  }}
  ## find simply most frequent exact value(s)
  if(any(sapply(c("allModes","mode","asIs"), identical, method))) {
    if(!is.factor(x)) x <- factor(as.character(x))
    tabX <- tabulate(x)
    out <- if(identical(method, "allModes")) {
      levels(x)[which(tabX == max(tabX))]
    } else levels(x)[which.max(tabX)]
    if(isNum) out <- as.numeric(out)
    if(debug) {message(fxNa," stM1"); stM1 <- list(method=method,bandw=bandw,x=x,tabX=tabX) }
  } else  chDu <- sum(duplicated(x)) 
  ## BBmisc
  if(identical(method, "BBmisc")) {
    if(requireNamespace("BBmisc")) {
      mo <- try(sapply(rangeSign, function(y) BBmisc::computeMode(signif(x, y))), silent=TRUE)
      if(inherits(mo, "try-error")) { method <- "density"
        warning(fxNa,"UNABLE to calulate BBmisc::computeMode(),  setting 'method' to 'density'")
      } else  { posi <- .firstMin(diff(mo)/mo[-length(mo)], positionOnly = TRUE)
        out <- mo[posi] }
      if(debug) {message(fxNa," stM2"); stM2 <- list(method=method,bandw=bandw,x=x,mo=mo) }
    } else { method <- "density"
      if(!silent) message(fxNa,"NOTE: Package 'BBmisc' not found ! Please install first from CRAN \n   setting 'method' to 'density'")
    }  
  }  
  ## density 
  if(identical(method, "density")) {
    if(length(bandw) <1) {bandw <- round(1.4* sqrt(length(x)))
      if(!silent) message(fxNa,"Method='density',  length of x =",length(x),", 'bandw' has been set to ",bandw)}
    x <- sort(x)
    raX <- max(x) - min(x)
    nExt <- bandw - 1
    x <- c(min(x) - raX * (nExt:1)/nExt, x, max(x) + raX * (1:nExt)/nExt)
    dif <- x[(bandw + 1):length(x)] - x[1:(length(x) - bandw)]
    maxDi <- which(dif == min(dif) & is.finite(dif))
    if(length(maxDi) > 1) maxDi <- maxDi[round(length(maxDi)/2)]
    out <- x[maxDi +nExt]
    names(out) <- maxDi
    if(debug) {message(fxNa," stM3"); stM3 <- list(method=method,bandw=bandw,x=x,raX=raX,nExt=nExt,maxDi=maxDi,dif=dif) }
  }
  ## binning
  if(identical(method, "binning")) {
    if(!all(length(bandw) >0, is.numeric(bandw))) bandw <- ceiling(sqrt(length(x)))
    if(70 * bandw > length(x) && !silent)  message(fxNa,"Method='binning', value of 'bandw'=", bandw, " may be too high for good functioning !")
    xRa <- range(x[which(is.finite(x))])
    frq <- table(cut(x, breaks=seq(xRa[1], xRa[2], length.out=bandw)))
    che <- max(frq, na.rm=TRUE) > c(0.5, 0.8) * length(x)
    if (che[2]) {
      if (!silent)  message(fxNa, ">80% of values in class no ", which.max(frq), ", refining mode estimation")
      mxF <- which.max(frq)
      mxF <- signif(seq(xRa[1], xRa[2], length.out = bandw)[c(max(mxF -3, 1), min(mxF +3, bandw))], 4)
      frq <- table(cut(x, breaks=seq(mxF[1], mxF[2], length.out=bandw)))
    } else {
      if(che[1] && sum(frq < length(x)/5000) > 0.5 * bandw) {
        if(!silent) message(fxNa, ">50% of values in class no ", 
          which.max(frq), " & >50% of other classes almost empty, refining result")
        useBr <- range(which(frq > 0.05 * length(x))) +c(-1, 1)
        useBr <- c(max(useBr[1], 1), min(useBr[2], bandw))
        mxF <- signif(seq(xRa[1], xRa[2], length.out=bandw)[useBr], 4)
        frq <- table(cut(x, breaks=seq(mxF[1], mxF[2], length.out=bandw)))
      }
    }
    out <- which.max(frq)
    out <- names(frq)[which.max(frq)]
    out <- as.numeric(unlist(strsplit(substr(out, 2, nchar(out) -1), ",")))
    out <- sum(out)/length(out)
  }
  out
}
  
