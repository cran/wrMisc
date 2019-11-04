#' Filter matrix to keep only first of repeated lines
#'
#' This function aims to reduce the complexity of a matrix (or data.frame) in case column 'refCol' has multiple lines with same value. 
#' In this case, it reduces the input-data to 1st line of redundant entries and returns a matrix (or data.frame) without lines identified as redundant entries for 'refCol').
#' in sum, this functions works lile useng \code{unique} on a given column, and propagates the same treatment to all other columns.
#' @param dat (matrix or data.frame) main input
#' @param refCol (integer) column number of reference-column
#' @param silent (logical) suppress messages
#' @param callFrom (character) allosw easier tracking of message(s) produced
#' @return matrix (same number of columns as input)
#' @seealso \code{\link{firstOfRepeated}}, \code{\link[base]{unique}}, \code{\link[base]{duplicated}} 
#' @examples
#' (mat1 <- matrix(c(1:6,rep(1:3,1:3)),ncol=2,dimnames=list(letters[1:6],LETTERS[1:2])))
#' firstLineOfDat(mat1)
#' @export
firstLineOfDat <- function(dat,refCol=2,silent=FALSE,callFrom=NULL){
  fxNa <- .composeCallName(callFrom,newNa="firstLineOfDat")
  msg <- " expecting matrix or data.frame with >= 2 columns"
  if(length(dim(dat)) <2) stop(msg)
  if(ncol(dat) < 2) stop(" expecting matrix or data.frame with >= 2 columns")
  if(is.character(refCol)) refCol <- which(refCol==colnames(dat))
  if(refCol > ncol(dat)) {
    if(!silent) message(fxNa," 'refCol' was set too high, reset to last column of 'dat'")
    refCol <- ncol(dat) }
  .getFirst <- function(x) x[1]     # value at 1st position
  useCol <- (1:ncol(dat))[-1*refCol]
  useLi <- tapply(1:nrow(dat),as.factor(dat[,refCol]),.getFirst)
  out <- if(length(useLi) >1) dat[useLi,] else matrix(dat[useLi,],ncol=ncol(dat),dimnames=list(rownames(dat)[useLi],colnames(dat)))
  out }
   
