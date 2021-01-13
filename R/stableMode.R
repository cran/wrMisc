#' Estimate mode (most frequent value)
#'
#' Estimate mode (ie most frequent value). The argument \code{method} allows to choos among the so far 3 different methods available.
#' If 'denisty' is chosen, the most dense region of sqrt(n) values will be chosen; 
#' if 'binning', the data will be binned (like in histograms) via rounding to a user-defined number of significant values ('rangeSign').
#' If \code{method} is set to 'BBmisc', this function will use \code{computeMode()} from package \href{https://CRAN.R-project.org/package=BBmisc}{BBmisc}.
#'  
#'
#' @param x numeric vector
#' @param method (character) There are 3 options : 'BBmisc', 'binning' and 'density';  
#'  If 'binning' the function will search context dependent, ie like most frequent class of histogram. Using this mode the search will be refined if either 80 percent of values in single class or >50 percent in single
#' @param histLike (logical) depreciated, please use argument \code{method} instead
#' @param rangeSign (integer) only used when \code{method='binning'}: range of numbers used as number of significant values
#' @param nCl (integer) only used when \code{method='binning'}: defines the number of classes used, very 'critical' parameter, may change results in strong way !! Note: higher values for 'nCl' will finally loose advantage of histLike-type search of mode !
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @param silent (logical) suppress messages
#' @return mode value (numeric)
#' @seealso \code{\link[base]{cut}}, \code{\link[graphics]{hist}}
#' @examples
#' set.seed(2012); dat <- round(c(rnorm(50),runif(100)),3)
#' stableMode(dat)
#' @export
stableMode <- function(x, method="binning", histLike=NULL, rangeSign=1:6, nCl=50, callFrom=NULL, silent=FALSE){
  fxNa <- .composeCallName(callFrom,newNa="stableMode")
  if(length(histLike) >0) message(fxNa," argument 'histLike' is depreciated, please use argument 'method' instead !") 
  x <- naOmit(x)
  if(length(unique(x)) <0) { method <- NULL; return(NULL)
    if(!silent) message(fxNa," no numeric values (nothing to do)")  
  } else if(length(unique(x))==1) { method <- NULL; return(x[1]); if(!silent) message(fxNa," all values are the same (= mode)")} 
  if(identical(method,"dens")) method <- "density"
  if(identical(method,"bin")) method <- "binning"
  if(identical(method,"histLike")) { method <- "binning"
    if(!silent) message(fxNa," Note: argument option 'histLike' has been depreciated and replaced by 'binning'") }
  out <- NULL
  ## first check how many values are indeed rep-/duplicated
  chDu <- sum(duplicated(x))                   # how to best make use ?
  ##
  if(identical(method,"BBmisc")) {
    chPa <- try(find.package("BBmisc"), silent=TRUE)
    if("try-error" %in% class(chPa)) message(fxNa," package 'BBmisc' not found ! Please install first \n   setting 'method' to 'density'") 
    method <- "density" }
  if(identical(method,"density")) {
    x <- sort(x)
    bandw <- round(sqrt(length(x)))                     # sets "nervousness" - of key importance (integer)
    ## need to extend to better capture mode at ends of distrib
    raX <- max(x) -min(x)
    nExt <- bandw -1 
    x <- c(min(x) - raX*(nExt:1)/nExt, x, max(x) + raX*(1:nExt)/nExt)
    ## main density
    dif <- x[(bandw+1):length(x)] - x[1:(length(x) -bandw)]
    maxDi <- which(dif==min(dif))
    if(length(maxDi) >1) {
      ## check if multiple results are same numeric value
      y <- x[maxDi]
      yTab <- table(y)
      maxDi <- as.integer(names(yTab)[which.max(yTab)])   # not yet optimal if no single best value
    }  
   out <- x[maxDi] 
  }
  if(identical(method,"BBmisc")) {
    mo <- sapply(rangeSign, function(y) BBmisc::computeMode(signif(x,y)))
    posi <- .firstMin(diff(mo)/mo[-length(mo)], positionOnly=TRUE)
    out <- mo[posi] }
  if(identical(method,"binning")) {
    if(70*nCl > length(x) & !silent) message(fxNa," value of 'nCl'=",nCl," may be too high for good functioning !")
    xRa <- range(x[which(is.finite(x))])
    frq <- table(cut(x, breaks=seq(xRa[1], xRa[2], length.out=nCl)))
    che <-  max(frq,na.rm=TRUE) > c(0.5,0.8)*length(x)
    if(che[2]) {                                                 # if >80% of values in one class
      if(!silent) message(fxNa,">80% of values in class no ",which.max(frq),", refining mode estimation")
      mxF <- which.max(frq)
      mxF <- signif(seq(xRa[1], xRa[2], length.out=nCl)[c(max(mxF-3,1), min(mxF+3,nCl))],4)    # new segm: 3 classes below & 3 classes above max
      frq <- table(cut(x, breaks=seq(mxF[1], mxF[2], length.out=nCl)))
    } else { if(che[1] & sum(frq <length(x)/5000) > 0.5*nCl) {    # if >50% of values in one class & >50% of other classes ~empty (<0.2%)
      if(!silent) message(fxNa,">50% of values in class no ",which.max(frq)," & >50% of other classes almost empty, refining result")
      useBr <- range(which(frq > 0.05*length(x))) + c(-1,1)
      useBr <- c(max(useBr[1],1), min(useBr[2],nCl))                # (boundary position) make sure to satay within bounders
      mxF <- signif(seq(xRa[1], xRa[2], length.out=nCl)[useBr],4)           # convert to boundery values
      frq <- table(cut(x, breaks=seq(mxF[1], mxF[2], length.out=nCl))) } }
    out <- which.max(frq)
    out <- names(frq)[which.max(frq)]
    out <- as.numeric(unlist(strsplit(substr(out, 2, nchar(out) -1), ",")))
    out <- sum(out)/length(out) }
  out }
   
