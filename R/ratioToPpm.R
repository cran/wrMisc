#' Convert ratio to ppm 
#'
#' \code{ratioToPpm} transforms ratio 'x' to ppm (parts per million). 
#' If 'y' not given (or different length as 'x'), then 'x' is assumed as ratio otherise rations are constructed as x/y is used lateron.
#' Does additional checking : negative values not expected - will be made absolute !
#' @param x (numeric) main input
#' @param y (numeric) optional value to construct ratios (x/y). If NULL (or different length as 'x'), then 'x' will be considered as ratio.
#' @param nSign (numeric) number of significan digits
#' @param silent (logical) suppres messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return numeric vector of ppm values
#' @seealso \code{\link{XYToDiffPpm}} for ppm of difference as used in mass spectrometrie
#' @examples
#' set.seed(2017); aa <- c(1.000001,0.999999,1+rnorm(10,0,0.001))
#' cbind(x=aa,ppm=ratioToPpm(aa,nSign=4))
#' @export
ratioToPpm <- function(x,y=NULL,nSign=NULL,silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="ratioToPpm")
  if(any(is.na(x))) {if(!silent) message(fxNa," 'x' contains ",sum(is.na(x))," out of ",length(x)," NA alike, omit !"); x <- naOmit(x)}
  if(length(y) >0) if(any(is.na(y))) {if(!silent) message(fxNa," 'y' contains ",sum(is.na(y))," NA alike, omit !"); y <- naOmit(y)}
  if(length(y) >1 & length(x)!= length(x)) {
    if(!silent) message(fxNa," length of 'y' (",length(y),") not corresponding to 'x' (",length(x),"), ignore 'y'")
    y <- NULL}
  if(length(y) == length(x) | length(y) ==1) {                      # get ration of  x / Y
   if(any(y==0) & !silent) message(fxNa," division by 0 !")
    x <- x/y }
  if(any(x <0)) {if(!silent) message(fxNa," negative values not possible"); x <- abs(x)}
  whNeg <- which(x <1)
  x[whNeg] <- 1/x[whNeg]
  x <- 1e6*(1-x)
  x[-1*whNeg] <- abs(x[-1*whNeg])
  if(length(nSign) >0) signif(x,as.numeric(nSign)) else x }
   
