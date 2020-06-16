#' Estimate mode (most frequent value)
#'
#' Estimate mode (most frequent value); rounding to range of numbers used as number of significant values ('rangeSign') to better estimate continuous data.
#' This function uses the package \href{https://CRAN.R-project.org/package=BBmisc}{BBmisc}.
#'
#' @param x numeric vector
#' @param histLike (logical) if TRUE, will search context dependent, ie like most frequent class of histogram. Using this mode the search will be refined if either 80 percent of values in single class or >50 percent in single
#' @param rangeSign (integer) range of numbers used as number of significant values
#' @param nCl (integer) defines the number of classes used (if 'histLike'=TRUE), very 'critical' parameter, may change results in strong way !! Note: higher values for 'nCl' will finally loose advantage of histLike-type search of mode !
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @param silent (logical) suppress messages
#' @return mode value (numeric)
#' @seealso \code{\link[base]{cut}}, \code{\link[graphics]{hist}}
#' @examples
#' set.seed(2012); dat <- round(c(rnorm(50),runif(100)),3)
#' stableMode(dat)
#' @export
stableMode <- function(x,histLike=FALSE,rangeSign=1:6,nCl=50,callFrom=NULL,silent=FALSE){
  fxNa <- .composeCallName(callFrom,newNa="stableMode")
  x <- naOmit(x)
  if(!histLike) {  
    chPa <- try(find.package("BBmisc"),silent=TRUE)
    if("try-error" %in% class(chPa)) message("package 'BBmisc' not found ! Please install first \n   setting 'histLike' to TRUE") 
    histLike <- TRUE }
  if(!histLike) {
    mo <- sapply(rangeSign,function(y) BBmisc::computeMode(signif(x,y)))
    posi <- .firstMin(diff(mo)/mo[-length(mo)],positionOnly=TRUE)
    out <- mo[posi]
  } else {                                                      # histogram like search for mode
    if(70*nCl > length(x) & !silent) message(fxNa,"value of 'nCl'=",nCl," may be too high for good functioning !")
    xRa <- range(x[which(is.finite(x))])
    frq <- table(cut(x,breaks=seq(xRa[1],xRa[2],length.out=nCl)))
    che <-  max(frq,na.rm=TRUE) > c(0.5,0.8)*length(x)
    if(che[2]) {                                                # if >80% of values in one class
      if(!silent) message(fxNa,">80% of values in class no ",which.max(frq),", refining mode estimation")
      mxF <- which.max(frq)
      mxF <- signif(seq(xRa[1],xRa[2],length.out=nCl)[c(max(mxF-3,1),min(mxF+3,nCl))],4)    # new segm: 3 classes below & 3 classes above max
      frq <- table(cut(x,breaks=seq(mxF[1],mxF[2],length.out=nCl)))
    } else { if(che[1] & sum(frq <length(x)/5000) > 0.5*nCl) {  # if >50% of values in one class & >50% of other classes ~empty (<0.2%)
      if(!silent) message(fxNa,">50% of values in class no ",which.max(frq)," & >50% of other classes almost empty, refining result")
      useBr <- range(which(frq > 0.05*length(x)))+ c(-1,1)
      useBr <- c(max(useBr[1],1),min(useBr[2],nCl))                # (boundary position) make sure to satay within bounders
      mxF <- signif(seq(xRa[1],xRa[2],length.out=nCl)[useBr],4)           # convert to boundery values
      frq <- table(cut(x,breaks=seq(mxF[1],mxF[2],length.out=nCl))) } }
    out <- which.max(frq)
    out <- names(frq)[which.max(frq)]
    out <- as.numeric(unlist(strsplit(substr(out,2,nchar(out)-1),",")))
    out <- sum(out)/length(out) }
  out }
   
