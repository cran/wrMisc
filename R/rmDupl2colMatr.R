#' Remove lines of matrix redundant /duplicated for 1st and 2nd column
#'
#' \code{rmDupl2colMatr} removes lines of matrix that are redundant /duplicated for 1st and 2nd column (irrespective of content of their columns).
#' The first occurance of redundant /duplicated elements is kept.
#'
#' @param mat (matrix or data.frame) main input
#' @param useCol (integer, length=2) columns to consider/use when looking for duplicated entries
#' @return matrix with duplictaed lines removed
#' @seealso \code{\link[base]{unlist}}
#' @examples
#' mat <- matrix(1:12,ncol=3)
#' mat[3,1:2] <- mat[1,1:2]
#' rmDupl2colMatr(mat)
#' @export
rmDupl2colMatr <- function(mat,useCol=c(1,2)) {
  ## remove entries with same value in 1st & 2nd col of mat
  msg <- " 'mat' should be matrix with at least 2 columns"
  if(length(dim(mat)) <2) stop(msg) else if(ncol(mat) <2) stop(msg)
  tx <- paste(mat[,useCol[1]],mat[,useCol[2]],sep="_")
  dup <- duplicated(tx,fromLast=FALSE)
  if(any(dup)) {if(sum(!dup)==1) matrix(mat[which(!dup),],dimnames=list(rownames(mat)[which(!dup)],colnames(mat))) else mat[which(!dup),]} else mat }
 
