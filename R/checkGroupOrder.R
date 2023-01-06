#' checkGrpOrder
#'
#' \code{checkGrpOrder} tests each line of 'x' if expected order appears. 
#' Used for comparing groups of measures with expected profile (simply by mataching expected order)
#' @param x matrix or data.frame
#' @param rankExp (numeric) expected order for values in columns, default 'rankExp' =1:ncol(x)
#' @param revRank (logical) if 'revRank'=TRUE, the initial ranks & reversed ranks will be tested
#' @param silent (logical) suppress messages
#' @param debug (logical) display additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return vector of logical values 
#' @seealso \code{\link[wrMisc]{checkGrpOrderSEM}}
#' @examples
#' set.seed(2005); mat1 <- rbind(matrix(round(runif(40),1),nc=4), rep(1,4))
#' checkGrpOrder(mat1)
#' checkGrpOrder(mat1,c(1,4,3,2))
#' @export
checkGrpOrder <- function(x, rankExp=NULL, revRank=TRUE, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="checkGrpOrder")
  if(length(x) <1 | length(dim(x)) !=2) stop(" 'x' should be data.frame or matrix of 2 dimensions")
  if(is.null(rankExp)) rankExp <- 1:ncol(x)
  if(length(rankExp) != ncol(x)) stop("Number of elements in 'rankExp' doesn't match number of columns in 'x'")
  ## main
  rankExp <- as.numeric(rankExp)
  out <- if(revRank) {
    apply(x, 1, function(x) {y <- as.numeric(order(x)); identical(rankExp,y) | identical(rev(rankExp),y)})
  } else {
    apply(x, 1, function(x) identical(rankExp, as.numeric(order(x))) )}
  out }
  
