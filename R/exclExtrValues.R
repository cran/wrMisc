#' Exclude extreme values (based on distance to mean)
#'
#' This function aims to identify extreme values (values most distant to mean, thus potential outlyers), mark them as NA or directely exclude them (depending on '\code{showNAs}').
#' Note that every set of non-identical values will have at least one most extreme value. Extreme values are part of many distributions, they are not necessarily true outliers. 
#'
#' @param dat numeric vector, main input
#' @param result (character) may be 'val' for returning data without extreme values or 'pos' for returning position/index of extreme values   
#' @param CVlim (NULL or numeric) allows to retain extreme values only if a certain CV (for all 'dat') is exceeded (to avoid calling extreme values form homogenous data-sets)
#' @param maxExcl (integer) max number of elments to explude
#' @param showNA (logical) will display extrelme values as NA
#' @param goodValues (logical) allows to display rather the good values instead of the extreme values
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return numeric vector wo extremle values or index-position of extreme values 
#' @seealso \code{\link{firstOfRepLines}},  \code{\link{get1stOfRepeatedByCol}} for treatment of matrix
#' @examples
#' x <- c(rnorm(30),-6,20)
#' exclExtrValues(x)
#' @export  
exclExtrValues <- function(dat,result="val",CVlim=NULL,maxExcl=1,showNA=FALSE,goodValues=TRUE,silent=FALSE,callFrom=NULL) {
   ## return position of extreme value (potential outlyer)
   ## results may be given as position of extreme values ('pos') or as values ('val') without the extremes identified
  fxNa <- .composeCallName(callFrom,newNa="exclExtrValues")
  extrVal <- which(abs(dat-sum(dat,na.rm=TRUE)/sum(!is.na(dat))) == max(abs(dat-mean(dat,na.rm=TRUE))))
  if(length(extrVal) == length(dat) & !silent) message(" all values are equidistant, not possible to distinguish one/some as 'extreme'")
  if(length(extrVal) > maxExcl & !silent) message(" more extreme values are equidistant than available for display with 'maxExcl' = ",maxExcl)
  if(!is.null(CVlim)) if(!is.finite(as.numeric(CVlim))) {
    warning(" 'CVlim' should be numeric of length 1, ignoring current value of 'CVlim'"); CVlim <- NULL}
  if(!is.null(CVlim)) {
    if(stats::sd(as.numeric(dat),na.rm=TRUE)/mean(as.numeric(dat),na.rm=TRUE) < as.numeric(CVlim)[1]) extrVal <- NA }
  extrVal <- naOmit(extrVal)[1:maxExcl]
  if(tolower(result) %in% c("val","value")) {
    if(showNA) {out <- dat; out[extrVal] <- NA } else out <- dat[-1*extrVal]}
  if(tolower(result) %in% c("pos","position")) out <- if(goodValues) (1:length(dat))[-1*extrVal]  else extrVal
  out }
   
