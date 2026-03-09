#' Get All Combinations With TRUE From Each Column 
#'
#' This function addresses the case when multiple alternatove ways exit to combine two elements. 
#' \code{combineSingleT} makes combinatory choices : if multiple \code{TRUE} in given column of 'mat' make all multiple selections with always one \code{TRUE} from each column
#' The resultant output contains index for first and second input columns elements to be combined.
#' @param mat 2-column matrix of logical values
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allows easier tracking of messages produced
#' @return This function returns a matrix with indexes of conbinations of \code{TRUE}
#' @examples
#' ## Example: Fist column indicates which boys want to dance and second column  
#' ## which girls want to dance. So if several boys want to dance each of the girls 
#' ## will have the chance to dance with each of them.  
#' matr <- matrix(c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE),ncol=2)
#' combineSingleT(matr)
#' @export
combineSingleT <- function(mat, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="combineSingleT")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
 
  msg <- "Expecting matrix of logical entries with 2 columns and at least 1 'TRUE' per column "
  if(length(dim(mat)) !=2) stop(msg) else if(nrow(mat) <1 | ncol(mat)<2) stop(msg)
  if(!is.logical(mat)) msg <- matrix(as.logical(mat), ncol=ncol(mat))
  chMu <- apply(mat, 2, which)
  if(any(sapply(chMu,length)<1)) stop(msg)
  if(is.matrix(chMu)) chMu <- list(chMu[,1],chMu[,2])
  out <- matrix(c(rep(chMu[[1]], sum(mat[,2])), rep(chMu[[2]], each=sum(mat[,1]))), ncol=2)
  colnames(out) <- if(is.null(colnames(mat))) paste0("de",1:2) else colnames(mat)
  out}
   
