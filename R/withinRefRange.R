#' Check for values within range of reference
#'
#' \code{withinRefRange} checks which values of numeric vector 'x' are within range +/- 'fa' x 'ref' (ie within range of reference). 
#' @param x matrix or data.frame
#' @param fa (numeric) absolute or relative tolerance value (numeric, length=1), interpreted according to 'absRef' as absolute or relative to 'x'(ie fa*ref)
#' @param ref (numeric) (center) reference value for comparison (numeric, length=1), if not given mean of 'x' (excluding NA or non-finite values) will be used
#' @param absRef (logical) return result as absolute or relative to 'x'(ie fa*ref)
#' @param asInd (logical) if TRUE return index of which values of 'x' are within range, otherwise return values if 'x' within range
#' @return numeric vector (containing only the values within range of reference)
#' @examples
#' ## within 2.5 +/- 0.7
#' withinRefRange(-5:6,fa=0.7,ref=2.5)                
#' ## within 2.5 +/- (0.7*2.5)
#' withinRefRange(-5:6,fa=0.7,ref=2.5,absRef=FALSE)
#' @export
withinRefRange <- function(x,fa,ref=NULL,absRef=TRUE,asInd=FALSE) {
  xIni <- x
  if(any(length(fa) !=1,!is.finite(fa),!is.finite(ref))) stop(" 'fa' and 'ref' must be finite !")
  chFin <- is.finite(as.numeric(x))
  if(sum(chFin) <1) stop(" no finite values found in 'x' !")
  if(sum(!chFin) >0) x[which(!chFin)] <- NA
  x <- as.numeric(x)
  if(is.null(ref)) ref <- sum(x[which(chFin)],na.rm=TRUE)/sum(chFin)
  ## main
  out <- if(absRef) which(abs(x -ref) < fa) else which(abs(x/ref -1) < fa)
  if(asInd) out else xIni[out] }
   
