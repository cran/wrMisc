#' Flexible extraction of columns
#'
#' This function provides flexible checking if a set of columns may be extracted from a matrix or data.frame 'x'.
#' 
#' @param x (matrix or data.frame) min input
#' @param extrCol (character or integer) columns to be extracted, may be column-names or column index 
#' @param doExtractCols (logical) if default \code{FALSE} only the column indexes will be returned
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return integer-vector (if\code{doExtractCols=FALSE} return depending on input \code{matrix} or \code{data.frame})  
#' @seealso \code{\link[utils]{read.table}}, \code{\link{filterList}}
#' @examples
#' dFr <- data.frame(a=11:14,b=24:21,cc=LETTERS[1:4],dd=rep(c(TRUE,FALSE),2)) 
#' extrColsDeX(dFr,c("b","cc","notThere")) 
#' extrColsDeX(dFr,c("b","cc","notThere"),doExtractCols=TRUE) 
#' @export
extrColsDeX <- function(x,extrCol,doExtractCols=FALSE,callFrom=NULL,silent=FALSE) {
  ## flexible extraction of columns from x
  ## remove any NA
  fxNa <- .composeCallName(callFrom,newNa="extrColsDeX")
  if(length(dim(x)) <2) stop("argument 'x' should be matrix or data.frame")
  if(any(dim(x) <1)) stop("nothing to do, 'x' has no lines or columns")
  chNa <- is.na(extrCol)
  if(any(chNa)) extrCol <- extrCol[which(!chNa)]
  ## check if use as integer (or match)
  chExtrCol <- sub("^[[:digit:]]+","",extrCol)
  chExtrCol <- nchar(chExtrCol) >0
  if(length(chExtrCol) <1) stop("nothing to do")
  if(all(!chExtrCol)) extrCol <- as.integer(extrCol) else {
    extrCo2 <- match(extrCol,colnames(x))
    ## check for NA due to match
    chNa <- is.na(extrCo2)
    if(any(chNa)) { 
      if(all(chNa)) stop(fxNa," Did not find ANY of the names given for extracting !")
      if(!silent) message(fxNa," Can't find column(s) ",wrMisc::pasteC(extrCol[which(chNa)],quote="'"))
      extrCo2 <- extrCo2[which(!chNa)] }
    extrCol <- extrCo2   
    }
  if(doExtractCols) x[,extrCol] else extrCol }   
   
