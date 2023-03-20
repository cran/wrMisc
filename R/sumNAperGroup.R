#' Count number of NAs per sub-set of columns
#'
#' This function will count the number of \code{NA}s per group (defined by argument \code{grp}) while summing over all lines of a matrix or data.frame.
#' The row-position has no influence on the counting.
#' Using the argument \code{asRelative=TRUE} the result will be given as (average) number of \code{NA}s per row and group.
#' 
#' @param x matrix or data.frame which may contain \code{NA}s
#' @param grp factor describing which column of 'dat' belongs to which group
#' @param asRelative (logical) return as count of \code{NA}s per row and group
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns an integer vector with count of \code{NA}s per group
#' @seealso \code{\link[base]{NA}}, filter \code{NA}s by line \code{\link{presenceFilt}}
#' @examples
#' mat <- matrix(1:25, ncol=5) 
#' mat[lower.tri(mat)] <- NA
#' sumNAperGroup(mat, rep(1:2,c(3,2)))
#' sumNAperGroup(mat, rep(1:2,c(3,2)), asRelative=TRUE)
#' 
#' @export
sumNAperGroup <- function(x, grp, asRelative=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## count number of NAs per set of columns defined by grp
  fxNa <- .composeCallName(callFrom, newNa="sumNAperGroup")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE

  if(length(dim(x)) <2) stop("Argument 'x' should be matrix or data.frame")
  if(length(grp) != ncol(x)) stop("Length of argument 'x' should match number of columns in 'x'")
  if(is.data.frame(x)) x <- as.matrix(x)
  if(debug) message(fxNa,"sNAG1")
  grpLev <- unique(naOmit(grp))
  out <- as.integer(by(t(x), grp, function(y) sum(is.na(y))))[rank(grpLev)]
  names(out) <- grpLev
  if(isTRUE(asRelative)) {
    nGrp <- table(grp)[rank(grpLev)] 
    out <- out/(nrow(x)*nGrp) }
  out }   
  
