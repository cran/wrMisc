#' Flexible extraction of columns
#'
#' This function provides flexible checking if a set of columns may be extracted from a matrix or data.frame 'x'.
#' If argument \code{extrCol} is list of character vectors, this allows to search among given options, the first matching name for each vector will be identified. 
#' 
#' @param x (matrix or data.frame) main input (where data should be extracted from)
#' @param extrCol (character, integer or list) columns to be extracted, may be column-names or column index; if is \code{list} each first-level element will be considered as options for one choice
#' @param doExtractCols (logical) if default \code{FALSE} only the column indexes will be returned
#' @param silent (logical) suppress messages
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @return integer-vector (if\code{doExtractCols=FALSE} return depending on input \code{matrix} or \code{data.frame})  
#' @seealso \code{\link[utils]{read.table}}, \code{\link{filterList}}
#' @examples
#' dFr <- data.frame(a=11:14,b=24:21,cc=LETTERS[1:4],dd=rep(c(TRUE,FALSE),2)) 
#' extrColsDeX(dFr,c("b","cc","notThere")) 
#' extrColsDeX(dFr,c("b","cc","notThere"),doExtractCols=TRUE) 
#' extrColsDeX(dFr,list(c("nn","b","a"),c("cc","a"),"notThere")) 
#' @export
extrColsDeX <- function(x,extrCol,doExtractCols=FALSE,callFrom=NULL,silent=FALSE) {
  ## flexible extraction of columns from x
  ## remove any NA
  fxNa <- .composeCallName(callFrom, newNa="extrColsDeX")
  nameX <- deparse(substitute(x))
  nameCol <- deparse(substitute(extrCol))
  if(length(dim(x)) <2) stop("argument ",nameX," should be matrix or data.frame")
  if(any(dim(x) <1)) stop("nothing to do, ",nameX,"  has no lines or columns")
  chNa <- is.na(extrCol)
  if(any(chNa)) extrCol <- extrCol[which(!chNa)]
  ## check if use as integer (or match)
  chExtrCol <- sub("^[[:digit:]]+", "", extrCol)
  chExtrCol <- nchar(chExtrCol) >0
  if(length(chExtrCol) <1) stop("nothing to do")
  if(all(!chExtrCol)) extrCol <- as.integer(extrCol) else {  ## try to find text in colnames
    if(is.null(colnames(x))) stop(" Problem: ",nameX,"  has no colnames !")
    if(!is.list(extrCol)) {
      ## single choice for each column to extract
      extrCo2 <- match(extrCol, colnames(x))
    } else  {
      ## try to locate first of multi-choices
      extrCo2 <- unlist(lapply(extrCol, function(z) naOmit(match(z,colnames(x)))[1]))      
    }  
    ## check for NA due to match
    chNa <- is.na(extrCo2)
    if(any(chNa)) { 
      if(all(chNa)) stop(fxNa," Did not find ANY of the names given in ",nameCol," for extracting !")
      if(!silent) message(fxNa," Can't find column(s) ",pasteC(extrCol[which(chNa)],quote="'")," in ",nameCol)
      extrCo2 <- extrCo2[which(!chNa)] }
    extrCol <- extrCo2   
    }
  if(doExtractCols) x[,extrCol] else extrCol }   
   
