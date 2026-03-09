#' Remove Lines Of Matrix Redundant /Duplicated For 1st And 2nd Column
#'
#' This function removes lines of matrix that are redundant /duplicated for 1st and 2nd column (irrespective of content of their columns).
#' The first occurance of redundant /duplicated elements is kept.
#'
#' @param mat (matrix or data.frame) main input
#' @param useCol (integer, length=2) columns to consider/use when looking for duplicated entries
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a matrix where duplictaed lines are removed
#' @seealso \code{\link[base]{unlist}}
#' @examples
#' mat <- matrix(1:12,ncol=3)
#' mat[3,1:2] <- mat[1,1:2]
#' rmDupl2colMatr(mat)
#' @export
rmDupl2colMatr <- function(mat,useCol=c(1,2), silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## remove entries with same value in 1st & 2nd col of mat
  fxNa <- .composeCallName(callFrom, newNa="rmDupl2colMatr")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  msg <- " 'mat' should be matrix with at least 2 columns"
  if(length(dim(mat)) <2) stop(fxNa, msg) else if(ncol(mat) <2) stop(fxNa, msg)
  tx <- paste(mat[,useCol[1]], mat[,useCol[2]], sep="_")
  dup <- duplicated(tx, fromLast=FALSE)
  if(any(dup)) {mat[which(!dup),, drop=FALSE]} else mat }
   
