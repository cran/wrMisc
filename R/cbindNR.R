#' cbind to non-redundant
#'
#' \code{cbindNR} combines all matrixes given as arguments to non-redundant column names (by ADDING the number of 'duplicated' columns !). 
#' Thus, this function works similar to \code{cbind}, but allows combining multiple matrix-objects containing redundant column-names.
#' The output may be optional sorted.
#'
#' @param ... all matrixes to get combined in cbind way
#' @param convertDFtoMatr (logical) decide if output should be converted to matrix
#' @param sortOutput (logical) optional sorting by column-names
#' @param silent (logical) suppres messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return matrix or data.frame (as cbind would return)
#' @seealso \code{\link[base]{cbind}}, \code{\link{nonAmbiguousNum}}, \code{\link{firstOfRepLines}}
#' @examples
#' ma1 <- matrix(1:6,ncol=3,dimnames=list(1:2,LETTERS[3:1]))
#' ma2 <- matrix(11:16,ncol=3,dimnames=list(1:2,LETTERS[3:5]))
#' cbindNR(ma1,ma2)
#' @export
cbindNR <- function(...,convertDFtoMatr=TRUE,sortOutput=TRUE,silent=FALSE,callFrom=NULL){
  inpL <- list(...)
  fxNa <- .composeCallName(callFrom,newNa="cbindNR")
  isDatafr <- sapply(inpL,is.data.frame)
  if(convertDFtoMatr & sum(isDatafr,na.rm=TRUE) >0) {
    inpL[isDatafr] <- lapply(inpL[isDatafr],as.matrix)     
    }
  isMatr <- sapply(inpL,is.matrix)
  if(sum(!isMatr,na.rm=TRUE) >0 & !silent) message("  removing ",sum(!isMatr)," entries from input which are apparently not matrixes")
  inpL <- inpL[which(sapply(inpL,is.matrix))]
  matrDim <- sapply(inpL,dim)
  if(length(inpL) <1) stop(" It seems the input doesn't contain any (valid) matrixes")
  if(length(unique(matrDim[1,])) >1) stop(" Some matrixes don't have the same number of rows (found ",paste(matrDim[1,],collapse=" "),")")
  nrColNa <- unique(unlist(lapply(inpL,colnames)))
  if(!silent) message("  treating ",length(nrColNa)," different (types of) columns : ",paste(utils::head(nrColNa),collapse=" "))
  out <- matrix(0,nrow=nrow(inpL[[1]]),ncol=length(nrColNa),dimnames=list(rownames(inpL[[1]]),nrColNa))
  startCol <- 1
  for(i in 1:length(inpL)) {out[,match(colnames(inpL[[i]]),colnames(out))] <- out[,match(colnames(inpL[[i]]),colnames(out))] + inpL[[i]];
    startCol <- startCol + ncol(inpL[[i]])}
  if(sortOutput) { out <- out[,order(colnames(out))]
    if(!silent) message("  sorting columns of output") }
  out }
   
