#' (upper) pairwise x,y combinations
#' 
#' \code{upperMaCoord} gets pairwise combinations for 'n' elements; return matrix with x & y coordinates to form all pairwise groups for n elements.
#' But no distinction of 'upper' or 'lower' possible like in \code{\link{triCoord}}
#' @param n (integer) number of elements for making all pair-wise combinations 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging 
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a 2-column matrix wiyh indexes for all pairwise combinations of 1:n
#' @seealso \code{\link[base]{lower.tri}}, more evolved version \code{\link{triCoord}}
#' @examples
#' upperMaCoord(4)
#' @export
upperMaCoord <- function(n, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ma <- matrix(1:n,ncol=n,nrow=n)
  cbind(x=ma[upper.tri(ma)],y=t(ma)[upper.tri(ma)]) }
    
