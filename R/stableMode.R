#' Estimate mode (most frequent value)   
#' 
#' Estimate mode, ie most frequent value. The argument \code{method} allows to choose among (so far) 3 different methods available.
#' If "density" is chosen, the most dense region of sqrt(n) values will be chosen; 
#' if "binning", the data will be binned (like in histograms) via rounding to a user-defined number of significant values ("rangeSign").
#' If \code{method} is set to "BBmisc", the function \code{computeMode()} from package \href{https://CRAN.R-project.org/package=BBmisc}{BBmisc} will be used.
#'	 
#' @param x (numeric) data to treat
#' @param method (character) There are 3 options : BBmisc, binning and density (default). If "binning" the function will search context dependent, ie like most frequent class of histogram.
#'  Using "binning" mode the search will be refined if either 80 percent of values in single class or >50 percent in single class.
#' @param bandw (integer) only used when \code{method="binning"} or  \code{method="density"} : defines the number of points to look for density or number of classes used; 
#'  very "critical" parameter, may change results in strong way. Note: with \code{method="binning"}: At higher values for "bandw" one will finally loose advantage of histLike-type search of mode !
#' @param rangeSign (integer) only used when \code{method="binning"}: range of numbers used as number of significant values
#' @param nCl (integer) depreciated argument, please use \code{bandw} instead
#' @param histLike (logical) depreciated, please use argument \code{method} instead
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @param silent (logical) suppress messages
#' @return MA-plot only
#' @seealso \code{computeMode()} in package \href{https://CRAN.R-project.org/package=BBmisc}{BBmisc}
#' @examples
#' set.seed(2012); dat <- round(c(rnorm(50), runif(100)),3)
#' stableMode(dat)
#' @export
stableMode <- function(x, method="density", bandw=NULL, rangeSign=1:6, nCl=NULL, histLike=NULL, callFrom=NULL, silent=FALSE) {
  ## stable mode  
  fxNa <- .composeCallName(callFrom, newNa="stableMode")
  if(length(histLike) > 0) message(fxNa, " argument 'histLike' is depreciated, please use argument 'method' instead !")
  x <- naOmit(x)
  if(length(unique(x)) < 0) {
    method <- NULL
    return(NULL)
    if(!silent)  message(fxNa, " no numeric values (nothing to do)") 
  } else if(length(unique(x)) == 1) {
    method <- NULL
    return(x[1])
    if(!silent) message(fxNa, " all values are the same (= mode)") }
  if(identical(method, "dens"))  method <- "density"
  if(identical(method, "bin")) method <- "binning"
  if(identical(method, "histLike")) {
    method <- "binning"
    if(!silent) message(fxNa, " Note: argument option 'histLike' has been depreciated and replaced by 'binning'") }
  out <- NULL
  chDu <- sum(duplicated(x))
  ## BBmisc
  if(identical(method, "BBmisc")) {
    chPa <- try(find.package("BBmisc"), silent = TRUE)
    if("try-error" %in% class(chPa)) { method <- "density"
      message(fxNa," package 'BBmisc' not found ! Please install first \n   setting 'method' to 'density'") }
  }
  ## density 
  if(identical(method, "density")) {
    if(length(bandw) <1) {bandw <- round(1.4* sqrt(length(x)))
      if(!silent) message(fxNa," method='density',  length of x =",length(x),", 'bandw' has been set to ",bandw)}
    x <- sort(x)
    raX <- max(x) - min(x)
    nExt <- bandw - 1
    x <- c(min(x) - raX * (nExt:1)/nExt, x, max(x) + raX * (1:nExt)/nExt)
    dif <- x[(bandw + 1):length(x)] - x[1:(length(x) - bandw)]
    maxDi <- which(dif == min(dif))
    if(length(maxDi) > 1) maxDi <- maxDi[round(length(maxDi)/2)]
    out <- x[maxDi +nExt]
    names(out) <- maxDi
  }
  if(identical(method, "BBmisc")) {
    mo <- sapply(rangeSign, function(y) BBmisc::computeMode(signif(x, y)))
    posi <- .firstMin(diff(mo)/mo[-length(mo)], positionOnly = TRUE)
    out <- mo[posi]
  }
  if(identical(method, "binning")) {
    if(length(nCl) >0) message(fxNa," method='binning', argument 'nCl' is depreciated and will be ignored, please use 'bandw' instead !")
    if(!all(length(bandw) >0, is.numeric(bandw))) bandw <- ceiling(sqrt(length(x)))
    if(70 * bandw > length(x) & !silent)  message(fxNa," method='binning', value of 'bandw'=", bandw, " may be too high for good functioning !")
    xRa <- range(x[which(is.finite(x))])
    frq <- table(cut(x, breaks = seq(xRa[1], xRa[2], length.out = bandw)))
    che <- max(frq, na.rm=TRUE) > c(0.5, 0.8) * length(x)
    if (che[2]) {
      if (!silent)  message(fxNa, ">80% of values in class no ", which.max(frq), ", refining mode estimation")
      mxF <- which.max(frq)
      mxF <- signif(seq(xRa[1], xRa[2], length.out = bandw)[c(max(mxF -3, 1), min(mxF +3, bandw))], 4)
      frq <- table(cut(x, breaks = seq(mxF[1], mxF[2], length.out = bandw)))
    } else {
      if(che[1] & sum(frq < length(x)/5000) > 0.5 * bandw) {
        if(!silent) message(fxNa, ">50% of values in class no ", 
          which.max(frq), " & >50% of other classes almost empty, refining result")
        useBr <- range(which(frq > 0.05 * length(x))) +c(-1, 1)
        useBr <- c(max(useBr[1], 1), min(useBr[2], bandw))
        mxF <- signif(seq(xRa[1], xRa[2], length.out = bandw)[useBr], 4)
        frq <- table(cut(x, breaks = seq(mxF[1], mxF[2], length.out = bandw)))
      }
    }
    out <- which.max(frq)
    out <- names(frq)[which.max(frq)]
    out <- as.numeric(unlist(strsplit(substr(out, 2, nchar(out) -1), ",")))
    out <- sum(out)/length(out)
  }
  out
}
  
