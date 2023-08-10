#' Check for similar values in series
#'
#' This function checks all values of 'x' for similar neighbour values within (relative) range of 'ppm' (ie parts per milion as measure of distance).
#' By default values will be sorted internally, so if a given value of \code{x} has anywhere in  \code{x}  another value close enough, this will be detected.
#' However, if \code{sortX=FALSE} only the values next to left and right will be considered.
#' Return logical vector : FALSE for each entry of 'x' if value inside of ppm range to neighbour (of sorted values) 
#' @param x numeric vector
#' @param ppm (numeric, length=1) ppm-range for considering as similar
#' @param sortX (logical) allows speeding up function when set to FALSE, for large data that are already sorted
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a logical vector : \code{TRUE} for each entry of \code{x} where at least one neighbour is inside of ppm distance/range
#' @seealso similar with more options \code{\link{withinRefRange}}
#' @examples
#' va1 <- c(4:7,7,7,7,7,8:10)+(1:11)/28600; checkSimValueInSer(va1)
#' data.frame(va=sort(va1),simil=checkSimValueInSer(va1))
#' @export
checkSimValueInSer <- function(x, ppm=5, sortX=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  fxNa <- .composeCallName(callFrom, newNa="checkSimValueInSer")
  if(isTRUE(debug)) silent <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  nNA <- sum(is.na(x))
  if(nNA >0 && !silent) message(fxNa,"Found ",nNA," NA values")
  so <- if(!isFALSE(sortX)) sort(x) else x
  di <- diff(so)
  diLim <- so *ppm *1e-6
  out <- rep(FALSE, length(x))
  names(out) <- names(so)
  out[-length(x)] <- abs(di) < abs(diLim[-length(x)])  # close to right neighbour
  out[-1] <- out[-1] | abs(di) < abs(diLim[-1])        # close to left neighbour
  if(sortX) out[rank(x, ties.method="first")] else out }

