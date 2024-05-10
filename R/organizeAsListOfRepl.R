#' (re)organize data of (3-dim) array as list of replicates
#'
#' Organize array of all data ('arrIn', long table) into list of (replicate-)arrays (of similar type/layout) based on dimension number 'byDim' of 'arrIn' (eg 2nd or 3rd dim).
#' Argument \code{inspNChar} defines the number of characters to consider, so if the beginning of names is the same they will be separated as list of multiple arrays.
#' Default will search for '_' separator or trim from end if not found in the relevant dimnames
#' @param arrIn (array) main input
#' @param inspNChar (interger) if inspNChar=0 the array-names (2nd dim of 'arrIn') will be cut before last '_'
#' @param byDim (integer, length=1) dimension number along which data will be split in separate elements (considering the first inspNChar characters)
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This function returns a list of arrays (typically 1st and 2nd dim for specific genes/objects, 3rd for different measures associated with)
#' @seealso \code{\link[base]{array}}
#' @examples
#' arr1 <- array(1:24,dim=c(4,3,2),dimnames=list(c(LETTERS[1:4]),
#'   paste("col",1:3,sep=""), c("ch1","ch2")))
#' organizeAsListOfRepl(arr1)
#' @export
organizeAsListOfRepl <- function(arrIn, inspNChar=0, byDim=3, silent=TRUE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="organizeAsListOfRepl")
  if(isTRUE(debug)) silent <- FALSE else { debug <- FALSE
    if(!isTRUE(silent)) silent <- FALSE }
  if(length(dim(arrIn)) !=3) stop(fxNa,"Expecting data organized as 3 dims, where last dimension should represent different plates")
  msg <- "Expecting 2nd dim to have names where characters 1 to 6 describe the plate-type- to identify replicates"
  if(sum(nchar(dimnames(arrIn)[[byDim]]) <1) >1) stop(fxNa,msg)
  msg <- "Expecting 'byDim' to be single integer between 1 an 3"
  if(!is.numeric(byDim)) stop(msg) else byDim <- as.integer(byDim)[1]  
  if(byDim <1 || byDim >3) stop(msg) 
  if(length(inspNChar) != ncol(arrIn)) inspNChar <- rep(inspNChar,3)[1:ncol(arrIn)]
  chNch <- inspNChar <1
  if(any(chNch)) inspNChar[which(chNch)] <- as.integer(gregexpr("_",dimnames(arrIn)[[byDim]][which(chNch)])) -1 
  chNch <- inspNChar <0
  if(any(inspNChar <1)) inspNChar[which(chNch)] <- nchar(.trimRight(dimnames(arrIn)[[byDim]])[which(chNch)])
  if(!silent) message(fxNa," inspect ",inspNChar," characters of  ",pasteC(dimnames(arrIn)[[byDim]]))
  plateTy <- substr(dimnames(arrIn)[[byDim]],1,inspNChar)
  if(length(unique(plateTy)) ==dim(arrIn)[byDim]) message(fxNa," names of rd dim of 'arrIn' seem all different")
  replPlaArr <- list()
  if(!silent) message(fxNa," inspect the following ",length(unique(plateTy))," plate-types : ",
    paste(utils::head(unique(plateTy),n=20),collapse=" "),if(length(plateTy) >20) " ...")
  uniPlaTy <- unique(plateTy)
  if(length(uniPlaTy) >0) for(i in 1:length(uniPlaTy)) {
    replPlaArr[[i]] <- array(as.numeric(arrIn[,which(plateTy==uniPlaTy[i]),]), dim=c(dim(arrIn)[1],sum(plateTy %in% uniPlaTy[i],na.rm=TRUE),dim(arrIn)[3]))
    dimnames(replPlaArr[[i]]) <- list(dimnames(arrIn)[[1]],dimnames(arrIn)[[byDim]][which(plateTy==uniPlaTy[i])],dimnames(arrIn)[[3]])
    }
  names(replPlaArr) <- uniPlaTy  
  replPlaArr }

#' fuse 2 instances of 3dim arr as mult cols in 3dim array
#'
#' This function allows fusing 2 instances of 3dim arr as mult cols in 3dim array (ie fuse along 2nd dim, increase cols)
#' 
#' @param arr1 (array) 
#' @param arr2 (array) 
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return This functuin returns a numeric vector with  numer of non-numeric characters (ie not '.' or 0-9))
#' @seealso \code{\link[base]{array}}
#' @examples
#' aa <- 11:15
#' @export
.fuse2ArrBy2ndDim <- function(arr1,arr2,silent=FALSE, debug=FALSE, callFrom=NULL){
  ## fuse 2 instances of 3dim arr as mult cols in 3dim array (ie fuse along 2nd dim, increase cols)
  fxNa <- .composeCallName(callFrom,newNa=".fuse2ArrBy2ndDim")
  msg <- "Problem with input : needs to have same number of rows and levels of 3rd dim"
  if(!identical(dim(arr1)[c(1,3)], dim(arr2)[c(1,3)])) stop(fxNa,msg)
  array(rbind(matrix(as.numeric(arr1), ncol=dim(arr1)[3]), matrix(as.numeric(arr2),ncol=dim(arr2)[3])), 
    dim=c(dim(arr1)[1], dim(arr1)[2] +dim(arr2)[2], dim(arr1)[3])) }
     
