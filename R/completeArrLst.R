#' Complete list of arrays for same dimensions
#'
#' This functions aims to inspect repeaing structues of data given as list of arrays and will try to complete 
#' arrays with fewer lines or columns (as this may appear eg with the very last set of high-thourput sceening data 
#' if fewer measures remain in the last set). Thus, the dimensions of the arrays are compared and 
#' cases with fewer (lost) columns (eg fewer experimental replicates) will be adjust/complete  by adding column(s) of NA.
#' Used eg when at reading mircotiterplate data the last set is not complete.
#' @param arrLst (list) list of arrays (typically 1st and 2nd dim for specific genes/objects, 3rd for different measures associated with)
#' @param silent (logical) suppress messages
#' @return list of arrays, now with same dimension of arrays 
#' @param callFrom (character) allows easier tracking of message(s) produced
#' @seealso \code{\link[wrMisc]{organizeAsListOfRepl}}, \code{\link[wrMisc]{extr1chan}}
#' @examples
#' arr1 <- array(1:24,dim=c(4,3,2),dimnames=list(c(LETTERS[1:4]),
#'   paste("col",1:3,sep=""),c("ch1","ch2")))
#' arr3 <- array(81:96,dim=c(4,2,2),dimnames=list(c(LETTERS[1:4]),
#'   paste("col",1:2,sep=""),c("ch1","ch2")))
#' arrL3 <- list(pl1=arr1,pl3=arr3)
#' completeArrLst(arrL3)
#' @export
completeArrLst <- function(arrLst,silent=FALSE,callFrom=NULL){
  ## adjust/complete cases with fewer (lost) columns (replicates) by adding column(s) of NA
  ## to obtain same dimension of arrays in list
  fxNa <- .composeCallName(callFrom,newNa="completeArrLst")
  dims <- sapply(arrLst,dim)
  corPos <- apply(dims,1,function(x) which(x < max(x,na.rm=TRUE)))
  maxDim <- apply(dims,1,max,na.rm=TRUE)
  if(!silent) message(fxNa," max dimensions ",paste(maxDim,collapse=" "))
  if(length(corPos[[1]]) >0) for(i in corPos[[1]]) message("filling along rows not yet implemented")
  if(length(corPos[[2]]) >0) for(i in corPos[[2]]) {
    if(!silent) message(fxNa," +filling ",names(arrLst)[i],"   from ",paste(dim(arrLst[[i]]),collapse=" "),"  to ",paste(maxDim,collapse=" "))
    add <- array(NA,dim=c(maxDim[1],maxDim[2]-dim(arrLst[[i]])[2],maxDim[3]))
    orgDimNa <- dimnames(arrLst[[i]])
    arrLst[[i]] <- .fuse2ArrBy2ndDim(arrLst[[i]],add)
    dimnames(arrLst[[i]]) <- list(orgDimNa[[1]],c(orgDimNa[[2]],paste("NA",1:(maxDim[2]-length(orgDimNa[[2]])),sep=".")),orgDimNa[[3]])
    }
  if(length(corPos[[3]]) >0) for(i in corPos[[3]]) message(fxNa,"filling along channels not yet implemented")
  arrLst }
  
