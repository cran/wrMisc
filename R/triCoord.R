#' Pairwise x,y Combinations
#'
#' This function gets pairwise combinations for 'n' elements; returns matrix with x & y coordinates to form all pairwise groups for 1:n elements
#' 
#' @param n (integer) number of elements for making all pair-wise combinations 
#' @param side (character) "upper" or "lower"
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a 2-column matrix with indexes for all pairwise combinations of 1:n
#' @seealso \code{\link{upperMaCoord}}, \code{\link{getPWseparator}}, \code{\link{indexGroupsFromPW}}, \code{\link{diffCombin}}; \code{\link[utils]{combn}}, \code{\link[base]{lower.tri}} or \code{upper.tri}, simpler version \code{\link{upperMaCoord}}
#' @examples
#' triCoord(4)
#' @export
triCoord <- function(n, side="upper", silent=FALSE, debug=FALSE, callFrom=NULL) {
  ma <- matrix(1:n, ncol=n, nrow=n)
  out <- if(identical(side,"upper")) cbind(x=ma[upper.tri(ma)], y=t(ma)[upper.tri(ma)])  else {
    cbind(x=ma[lower.tri(ma)], y=t(ma)[lower.tri(ma)])}
  out }
     
