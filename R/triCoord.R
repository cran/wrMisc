#' Pairwise x,y combinations
#'
#' \code{triCoord} gets pairwise combinations for 'n' elements; returns matrix with x & y coordinates to form all pairwise groups for 1:n elements
#' @param n (integer) number of elements for making all pair-wise combinations 
#' @param side (character) "upper" or "lower"
#' @return 2-column matrix wiyh indexes for all pairwise combnations of 1:n
#' @seealso \code{\link[base]{lower.tri}} or \code{upper.tri}, simpler version \code{\link{upperMaCoord}}
#' @examples
#' triCoord(4)
#' @export
triCoord <- function(n,side="upper") {
  ma <- matrix(1:n,ncol=n,nrow=n)
  out <- if(identical(side,"upper")) cbind(x=ma[upper.tri(ma)],y=t(ma)[upper.tri(ma)])  else {
    cbind(x=ma[lower.tri(ma)],y=t(ma)[lower.tri(ma)])}
  out }
     
