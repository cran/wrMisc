#' Cut 3-dim array in list of matrixes (or arrays) similar to organizing into clusters
#'
#' \code{cutArrayInCluLike} cuts 'dat' (matrix,data.frame or 3-dim array) in list (of appended lines) according to 'cluOrg', 
#'  which serves as instruction which line of 'dat' should be placed in which list-element (like sorting according to cluster-numbers). 
#' @param dat array (3 dim)
#' @param cluOrg (factor) organization of lines to clusters
#' @param silent (logical) suppress messages
#' @param debug (logical) display additional messages for debugging
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @return This function retruns a list of matrixes (or arrays)
#' @examples
#' mat1 <- matrix(1:30,nc=3,dimnames=list(letters[1:10],1:3))
#' cutArrayInCluLike(mat1,cluOrg=factor(c(2,rep(1:4,2),5)))
#' @export
cutArrayInCluLike <- function(dat, cluOrg, silent=FALSE, debug=FALSE, callFrom=NULL){
  fxNa <- .composeCallName(callFrom, newNa="cutArrayInCluLike")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  msg2 <- " 'cluOrg' should be factor or numeric vector corresponding to lines of 'dat', ie instruction which line of 'dat' to put in which list-element (cluster)"
  che <- dim(dat)
  if(!length(che) %in% c(2,3)) stop(fxNa,"Expecting matrix or data.frame of 2 or array of 3 dimensions")
  if(is.null(cluOrg)) stop(fxNa,msg2)
  if(length(cluOrg) != nrow(dat)) stop(fxNa,msg2)
  uniqNa <- unique(naOmit(cluOrg))
  out <- list()
  for(i in 1:length(uniqNa)) out[[i]] <- if(length(che) ==3){
    dat[which(cluOrg==uniqNa[i]),,]
  } else {
    dat[which(cluOrg==uniqNa[i]),] }
  names(out) <- uniqNa
  out }
  
#' Combine annotation information from list of matrixes
#'
#' This function allows to combine information (annotation) from list of matrixes (ie replace when NA), using always the columns specified in 'useCol' (numeric)
#' 
#' @param lst (list) main input
#' @param useCol (numeric vector) which columns should be used
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) additional messages for debugging
#' @return This function returns a single matrix of combined (non-redundant) info
#' @seealso  used in \code{\link{cutArrayInCluLike}}
#' @examples
#' .datSlope(c(3:6))
#' @export
.combineListAnnot <- function(lst, useCol=1:2, silent=FALSE, debug=FALSE, callFrom=NULL){
  ## combine information (annotation) from list of matrixes (ie replace when NA), using always the columns specified in 'useCol' (numeric)
  ## the 1st elment of 'useCol' will be used as to select column used as key for merging
  ## return single matrix of combined (non-redundant) info
  fxNa <- .composeCallName(callFrom, newNa=".combineListAnnot")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(length(useCol) <2) stop(fxNa,"Need at least 2 columns numbers in 'useCol'  (1st col specified used as key)")
  cheDim <- sapply(lst, dim)
  if(is.list(cheDim)) {
    lst <- lst[sapply(cheDim, length) >1]
    cheDim <- sapply(lst, dim)}
  if(is.list(cheDim)) {
    cheDim3 <- sapply(cheDim, length)
    if(any(cheDim3 >2)) message(fxNa,"So far not coded for arrays with >2 dims")
    lst <- lst[sapply(cheDim,length) <3] }
  if(length(lst) <1) stop(fxNa,"No (valuable) matrixes or data.frames found in 'lst'")
  cheDim <- sapply(lst, dim)
  if(any(cheDim[1,] <0)) {message(fxNa,"Omiting 0-row elements"); lst <- lst[cheDim[1,] >0]}
  if(any(max(useCol) > cheDim[2,])) stop(fxNa,"Column no specified in 'useCol' seems not to exist")
  ## main
  colNa1 <- colnames(lst[[1]])[useCol]
  if(!silent) message(fxNa," .combineListAnnot() .. checking ",length(lst)," list-elements;   '",
    colNa1[1],"' will be used as key")
  out <- as.matrix(lst[[1]][,useCol])
  if(length(lst) >1) for(i in 2:length(lst)) {
    compColNa <- sort(colnames(lst[[i]])[useCol]) == sort(colNa1)    # compare col-names (too strict ??)
    if(all(compColNa)) {              # for optional testing if col-names do indeed match
      out <- as.matrix(merge(out,lst[[i]][,useCol], by=colNa1[1],all=TRUE))
      NaLi <- which(is.na(out[,2]))
      for(j in 2:length(useCol)) out[NaLi,j] <- out[NaLi,length(useCol)+j-1] }
    out <- out[,1:length(useCol)] }
  stripColNa <- sub(".x$","",colnames(out))
  if(length(unique(stripColNa)) ==ncol(out)) colnames(out) <- stripColNa
  out }

#' Check list of arrays for consistent dimensions of all arrays
#'
#' This function allows to check list of arrays for consistent dimensions of all arrays
#' 
#' @param arrLst (list) main input
#' @param arrNDim (integer) number of dimensions for arrays
#' @param fxName (character) this name will be given in message
#' @param varName (character) 
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) additional messages for debugging
#' @return list
#' @seealso  used in \code{\link{cutArrayInCluLike}}
#' @examples
#' .datSlope(c(3:6))
#' @export
.checkConsistentArrList <- function(arrLst, arrNDim=3, fxName=NULL, varName=NULL, silent=FALSE, debug=FALSE, callFrom=NULL) {
  ## check list of arrays for consistent dimensions of all arrays
  ## arrNDim .. number of dimensions for arrays
  ## fxName will be given in message
  fxNa <- .composeCallName(callFrom, newNa=".checkConsistentArrList")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  che <- sapply(arrLst, dim)
  if(!is.null(fxName)) fxName <- paste(fxName,": ")
  if(!is.null(varName)) varName <- "'arrLst'"
  if(is.list(che)) stop(fxName,"Format of '",varName,"' seems not consistent")
  if(any(nrow(che) != arrNDim)) stop(fxName,"Expecting list of ",paste(arrNDim,collapse=", ")," dim arrays in ",varName)
  if(any(apply(che, 1, function(x) length(unique(x)))) !=1) message(fxName,"Format of ",varName," seems not consistent")
  }

#' Summarize along columns of multiple arrays in list
#'
#' This function allows summarizing along columns of multiple arrays in list
#' 
#' @param arrLst (list) main input
#' @param sumType (character)
#' @param arrOutp (logical)
#' @param signifDig (integer)
#' @param formatCheck (logical)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) additional messages for debugging
#' @return array (1st dim will be summary along cols, rows will be layers of 3rd array-dim 
#' @seealso used in \code{\link{cutArrayInCluLike}}
#' @examples
#' .datSlope(c(3:6))
#' @export
.arrLstMean <- function(arrLst, sumType="mean", arrOutp=FALSE, signifDig=3,formatCheck=FALSE,silent=FALSE, debug=FALSE, callFrom=NULL){
  ## summarize along columns of mult arrays in list
  ##  can summarize as median or mean
  ## resultant 1st dim will be summary along cols, rows will be layers of 3rd array-dim ie dim(arrLst[[1]])[3]
  ## eg expr data as clusters (list-elements)
  fxNa <- .composeCallName(callFrom, newNa=".arrLstMean")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(formatCheck) .checkConsistentArrList(arrLst, fxName=fxNa)   
  diNa <- dimnames(arrLst[[1]])
  out <- if(sumType=="median") {
    lapply(arrLst, function(x) t(signif(apply(x,c(2,3), stats::median, na.rm=TRUE), signifDig)) )
   } else {
    lapply(arrLst, function(x) t(signif(apply(x, 3, colMeans, signifDig))) ) }
  if(is.list(out)) names(out) <- names(arrLst)
  if(arrOutp) out <- aperm(array(unlist(lapply(out,t)), dim=c(dim(arrLst[[1]])[-1], length(arrLst)),
    dimnames=list(diNa[[2]], diNa[[3]], names(arrLst))), c(2,1,3))  
  out }

#' Summarize along columns of mult arrays in list
#'
#' This function allows summarizing along columns of mult arrays in list
#' 
#' @param arrLst (list) main input
#' @param arrOutp (logical)
#' @param signifDig (integer)
#' @param formatCheck (logical)
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) additional messages for debugging
#' @return array (1st dim will be summary along cols, rows will be layers of 3rd array-dim ie dim(arrLst[[1]])[3])
#' @seealso used in \code{\link{cutArrayInCluLike}}
#' @examples
#' .datSlope(c(3:6))
#' @export
.arrLstSEM <- function(arrLst, arrOutp=FALSE, signifDig=3, formatCheck=FALSE, silent=FALSE, debug=FALSE, callFrom=NULL){
  ## summarize along columns of mult arrays in list
  ##  can summarize as median or mean
  ## resultant 1st dim will be summary along cols, rows will be layers of 3rd array-dim ie dim(arrLst[[1]])[3]
  ## eg expr data as clusters (list-elements)
  fxNa <- .composeCallName(callFrom, newNa=".arrLstSEM")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(formatCheck) .checkConsistentArrList(arrLst, fxName=fxNa)
  diNa <- dimnames(arrLst[[1]])
  out <- lapply(arrLst, function(x) t(apply(aperm(x,c(2,1,3)), 3, rowSEMs)) )
  if(is.list(out)) names(out) <- names(arrLst)
  if(arrOutp) out <- aperm(array(unlist(lapply(out,t)), dim=c(dim(arrLst[[1]])[-1],length(arrLst)),
    dimnames=list(diNa[[2]], diNa[[3]], names(arrLst))), c(2,1,3))  
  out }
  
