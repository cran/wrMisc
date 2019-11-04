#' Non-redundant lines of matrix
#'
#' \code{nonRedundLines} reduces complexity of matrix (or data.frame) if multiple consectuive (!) lines with same values.
#' Return matrix (or data.frame) without repeated lines (keep 1st occurance)
#' @param dat (matrix or data.frame) main input
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return matrix (or data.frame) without repeated lines (keep 1st occurance)..
#' @seealso \code{\link{firstLineOfDat}}, \code{\link{firstOfRepLines}}, \code{\link{findRepeated}}, \code{\link{firstOfRepeated}}, \code{\link{get1stOfRepeatedByCol}}, \code{\link{combineRedBasedOnCol}}, \code{\link{correctToUnique}} 
#' @examples
#' mat2 <- matrix(rep(c(1,1:3,3,1),2),ncol=2,dimnames=list(letters[1:6],LETTERS[1:2]))
#' nonRedundLines(mat2)
#' @export
nonRedundLines <- function(dat,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="nonRedundLines")
  msg <- c(fxNa," expecting matrix or data.frame with >1 lines and >= 1 column(s)")
  if(length(dim(dat)) <2) stop(msg)
  if(any(nrow(dat)<2, ncol(dat) <1)) stop(msg)
  if(is.null(rownames(dat))) rownames(dat) <- 1:nrow(dat)
  exclLi <- which(rowSums(dat[-nrow(dat),] == dat[-1,]) ==ncol(dat)) +1
  out <- if(length(exclLi) >0) dat[-exclLi,] else dat
  if(length(dim(out)) <2) out <- matrix(out,ncol=ncol(dat),
    dimnames=list(rownames(dat)[-1*exclLi],colnames(dat)))
  out }
   
