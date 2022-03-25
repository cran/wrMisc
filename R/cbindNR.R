#' cbind to non-redundant
#'
#' \code{cbindNR} combines all matrixes given as arguments to non-redundant column names (by ADDING the number of 'duplicated' columns !). 
#' Thus, this function works similar to \code{cbind}, but allows combining multiple matrix-objects containing redundant column-names.
#' Of course, all input-matrixes must have the same number of rows !
#' By default, the output gets sorted by column-names.
#' Note, due to the use of '...' arguments must be given by their full argument-names, lazy evaluation might not recognize properly argument names.
#'
#' @param ... all matrixes to get combined in cbind way
#' @param convertDFtoMatr (logical) decide if output should be converted to matrix
#' @param sortOutput (logical) optional sorting by column-names
#' @param summarizeAs (character) decide of combined values should get summed (default, 'sum') or averaged ('mean')
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a matrix or data.frame (as cbind would return)
#' @seealso \code{\link[base]{cbind}}, \code{\link{nonAmbiguousNum}}, \code{\link{firstOfRepLines}}
#' @examples
#' ma1 <- matrix(1:6, ncol=3, dimnames=list(1:2,LETTERS[3:1]))
#' ma2 <- matrix(11:16, ncol=3, dimnames=list(1:2,LETTERS[3:5]))
#' cbindNR(ma1, ma2)
#' cbindNR(ma1, ma2, summarizeAs="mean")
#' @export
cbindNR <- function(..., convertDFtoMatr=TRUE, sortOutput=TRUE, summarizeAs="sum", silent=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="cbindNR")
  inpL <- list(...)
  isDatafr <- sapply(inpL, is.data.frame)
  if(convertDFtoMatr & any(isDatafr) >0) {
    inpL[isDatafr] <- lapply(inpL[isDatafr], as.matrix)     
    }
  isMatr <- sapply(inpL,is.matrix)
  if(any(!isMatr) & !silent) {
    message(fxNa," removing ",sum(!isMatr, na.rm=TRUE)," entries from input which are apparently not matrixes")
    inpL <- inpL[which(isMatr)]} 
  if(length(inpL) <1) stop(" It seems the input doesn't contain any (valid) matrixes")
  matrDim <- sapply(inpL,dim)
  if(length(unique(matrDim[1,])) >1) stop(" Some matrixes don't have the same number of rows (found ",paste(matrDim[1,],collapse=" "),")")
  nrColNa <- unique(unlist(lapply(inpL,colnames)))
  if(!silent) message(fxNa," treating ",length(nrColNa)," different (types of) columns : ",paste(utils::head(nrColNa),collapse=" "))
  out <- matrix(0,nrow=nrow(inpL[[1]]), ncol=length(nrColNa), dimnames=list(rownames(inpL[[1]]),nrColNa))
  startCol <- 1
  nByCol <- rep(0,length(nrColNa))
  names(nByCol) <- nrColNa
  for(i in 1:length(inpL)) { useCol <- match(colnames(inpL[[i]]), colnames(out))
    out[,useCol] <- out[,useCol] + inpL[[i]]
    nByCol[useCol] <- nByCol[useCol] +1 
    startCol <- startCol + ncol(inpL[[i]])}  
  if(identical(summarizeAs,"mean") & any(nByCol >1)) { useCol <- which(nByCol >1)
    out[,useCol] <- out[,useCol] / matrix(rep(nByCol[useCol], each=nrow(out)), nrow=nrow(out))
  }
  if(sortOutput) { out <- out[,order(colnames(out))]
    if(!silent) message(fxNa,"  sorting columns of output") }
  out }
  
