#' checkGrpOrder
#'
#' \code{checkGrpOrder} tests each line of 'x' if expected order appears. 
#' Used for comparing groups of measures with expected profile (simply by mataching expected order)
#' @param x matrix or data.frame
#' @param rankExp (numeric) expected order for values in columns, default 'rankExp' =1:ncol(x)
#' @param revRank (logical) if 'revRank'=TRUE, the initial ranks & reversed ranks will be tested
#' @return vector of logical values 
#' @seealso \code{\link[wrMisc]{checkGrpOrderSEM}}
#' @examples
#' set.seed(2005); mat <- matrix(round(runif(40),1),ncol=4)
#' checkGrpOrder(mat)
#' checkGrpOrder(mat,c(1,4,3,2))
#' @export
checkGrpOrder <- function(x,rankExp=NULL,revRank=TRUE){
  if(length(dim(x)) !=2) stop(" 'x' should be data.frame or matrix of 2 dimensions")
  if(is.null(rankExp)) rankExp <- 1:ncol(x)
  if(length(rankExp) != ncol(x)) stop(" number of elements in 'rankExp' doesn't match number of columns in 'x'")
  ## main
  rankExp <- as.numeric(rankExp)
  out <- if(revRank) {
    apply(x,1,function(x) {y <- as.numeric(order(x)); identical(rankExp,y) | identical(rev(rankExp),y)})
  } else {
    apply(x,1,function(x) identical(rankExp,as.numeric(order(x))) )}
  out }
  
