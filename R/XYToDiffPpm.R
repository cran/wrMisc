#' Express difference as ppm
#'
#' This function transforms offset (pariwise-difference) between 'x' & 'y' to ppm (as normalized difference ppm, parts per million, ie (x-y)/y ).
#' This type of expressiong differences is used eg in mass-spectrometry.
#' @param x (numeric) typically for measured variable
#' @param y (numeric) typically for theoretical/expected value (vector must be of same length as 'x')
#' @param nSign (integer) number of significant digits in output
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a numeric vector of (ratio-) ppm values
#' @seealso \code{\link{ratioToPpm}} for classical ppm
#' @examples
#' set.seed(2017); aa <- runif(10,50,900)
#' cbind(x=aa,y=aa+1e-3,ppm=XYToDiffPpm(aa,aa+1e-3,nSign=4)) 
#' @export
XYToDiffPpm <- function(x, y, nSign=NULL, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="XYToDiffPpm")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE

  if(any(is.na(x))) {if(!silent) message(fxNa," 'x' contains ",sum(is.na(x))," out of ",length(x)," NA alike, omit !"); x <- naOmit(x)}
  if(length(y) >0) if(any(is.na(y))) {if(!silent) message(fxNa," 'y' contains ",sum(is.na(y))," NA alike, omit !"); y <- naOmit(y)}
  if(any(length(x) <1, length(y) <1, length(x)!= length(y))) stop("Length of 'y' (",length(y),") not corresponding to 'x' (",length(x),")")
  x <- 1e6*(x-y)/y
  if(length(nSign)>0) signif(x,as.numeric(nSign)) else x }
   
