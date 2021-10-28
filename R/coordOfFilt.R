#' get coordinates of values/points in matrix according to filtering condition
#'
#' Get coordinates of values/points in matrix according to filtering condition
#'
#' @param mat (matrix or data.frame) matrix or data.frame 
#' @param cond (logical or integer) condition/test to see which values of \code{mat} fulfull test, or integer of index passing  
#' @param sortByRows (logical) optional sorting of results by row-index
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced 
#' @return matrix columns 'row' and 'col'
#' @seealso \code{\link[base]{which}}
#' @examples
#' set.seed(2021); ma1 <- matrix(sample.int(n=40,size=27,replace=TRUE), ncol=9)
#' ## let's test which values are >37
#' which(ma1 >37)      # doesn't tell which row & col
#' coordOfFilt(ma1, ma1 >37)
#' 
#' @export
coordOfFilt <- function(mat, cond, sortByRows=FALSE, silent=FALSE, callFrom=NULL) {
  ## get coordinates of values/points in matrix according to filtering condition
  fxNa <- .composeCallName(callFrom, newNa="coordOfFilt")
  if(any(length(dim(mat)) !=2, dim(mat) < 2:1)) stop("Invalid argument 'mat'; must be matrix or data.frame with min 2 lines and 1 col")
  cond <- if(is.logical(cond)) which(cond) else as.integer(cond) 
  chNa <- is.na(cond)
  if(any(chNa)) cond <- cond[which(!chNa)]
  if(min(cond) <1 | max(cond) > prod(dim(mat))) stop("invalid entry for 'cond'")
  ## main
  out <- cbind(row=cond %% nrow(mat), col=(cond +nrow(mat) -1) %/% nrow(mat))
  ch0 <- out[,1]==0        # need to replace row=0
  if(any(ch0)) out[which(ch0)] <- nrow(mat)
  if(length(unique(names(cond)))==length(cond)) rownames(out) <- names(cond)
  if(sortByRows) out <- out[order(out[,1], decreasing=FALSE), ]
  out }
  
