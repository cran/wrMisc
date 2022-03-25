#' Organize data as separate list-entries
#'
#' \code{asSepList} allows reorganizing list into separate numeric vectors. For example, matrixes or data.frames will be split into separate columns 
#' (differnt to \code{\link[wrMisc]{partUnlist}} which maintains the original structure). This function also works with lists of lists.
#' This function may be helpful for reorganizing data for plots.
#'
#' @param y list to be separated/split in vectors
#' @param asNumeric (logical) to transform all list-elements in simple numeric vectors (won't work if some entries are character)
#' @param exclElem (character) optinal names to exclude if any (lazy matching) matches (to exclude other arguments be misinterpreted as data)
#' @param fxArg depreciated, replaced by \code{exclElem}
#' @param minLen (integer) (currently use of this argument not implemeneted!) min length (or number of rows), as add'l element to eliminate arguments given wo names when asSepList is called in vioplot2
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of message(s) produced
#' @param debug (logical) display additional messages for debugging
#' @return This function returns a list, partially unlisted to vectors
#' @seealso \code{\link[wrMisc]{partUnlist}}, \code{\link[base]{unlist}}
#' @examples
#' bb <- list(fa=gl(2,2),c=31:33,L2=matrix(21:28,nc=2),li=list(li1=11:14,li2=data.frame(41:44)))
#' asSepList(bb)
#' ## multi data-frame examples
#' ca <- data.frame(a=11:15,b=21:25,c=31:35)
#' cb <- data.frame(a=51:53,b=61:63)
#' cc <- list(gl(3,2),ca, cb, 91:94, short=81:82, letters[1:5])
#' asSepList(cc)
#' cd <- list(e1=gl(3,2),e2=ca, e3=cb, e4=91:94,short=81:82, e6=letters[1:5])
#' asSepList(cd)
#' @export
asSepList <- function(y, asNumeric=TRUE, minLen=4, exclElem=NULL, fxArg=NULL, silent=FALSE, callFrom=NULL, debug=FALSE) {
  ## convert all data-series of list (ie all list elements or columns) in separate list-elements (OK with list of lists) eg for plots
  ## 'asNumeric'.. to transform all list-elements in simple numeric vectors (won't work if some entries are character)
  ## 'minLen' .. min length (or number of rows), as add'l element to eliminate arguments given wo names when asSepList is called in vioplot2
  ## 'fxArg' .. optinal, names to exclude if any (lazy matching) matches (to exclude other arguments be mis-interpreted as data, used in vioplot2)
  fxNa <- .composeCallName(callFrom, newNa="asSepList")
  f1 <- function(x,lim=1) if(length(dim(x)) ==2) ncol(x) >lim else FALSE   # locate elements with multiple cols
  if(is.matrix(y)) y <- list(y) else if(!is.list(y)) y <- as.list(y)
  chSubLi <- sapply(y, is.list) & !sapply(y, is.data.frame)
  if(debug) {silent <- FALSE
    message(fxNa," ini length of 'y' ",length(y)) }
  w <- NULL
  ## try to separate sub-lists
  if(length(y) >0 & any(chSubLi)) {                        # run partUnlist() on all sub-lists
    for(i in 1:sum(chSubLi)) {
      w <- partUnlist(y[which(chSubLi)[i]])
      iniNa <- names(y[which(chSubLi)[i]])
      y <- y[-which(chSubLi)[i]]           # remove orig
      if(nchar(iniNa) >0) names(w) <- paste(iniNa, 1:length(w), sep="_")
      y[length(y) +1:length(w)] <- w  } }
  ## depreciate fxArg  (v 1.6.2)
  if(length(fxArg) >0) {
    if(isFALSE(silent)) message(fxNa, "Argument 'fxArg' is depreciated, please use argument 'exclElem'")
  }
  ## check for conflicting names to 'exclElem'
  if(length(exclElem) >0 & length(y) >0) {
    chNa <- names(y) %in% exclElem
    if(debug) message(fxNa,"head chNa ",pasteC(utils::head(chNa)))    # problem using .checkArgNa() ? 
    if(any(chNa)) { if(isFALSE(silent)) message(fxNa," reducing list from ",length(y)," to ",sum(!chNa,na.rm=TRUE))
      y <- y[which(!chNa)]} }
  ## filter minLen
  chCol <- sapply(y, f1)
  if(any(chCol) & length(y) >0) {                                  # has multi-col
    chLe <- sapply(y[which(chCol)], nrow) < minLen
    if(any(chLe)) y[which(chCol)[which(chLe)]] <- NULL }
  chCol <- sapply(y, f1, 0)
  if(any(chCol) & length(y) >0) { 
      chLe <- sapply(y, length) < minLen
      chDf <- sapply(y, is.data.frame)                          # need to correct in case of df
      if(any(chDf)) chLe[which(chDf)] <- sapply(y[which(chDf)],nrow) < minLen
      if(any(chLe)) y[which(chLe)] <- NULL  
  } else y <- y[which(sapply(y,length) > minLen)]
  if(length(y) <1 & isFALSE(silent)) message(fxNa," Note, NOTHING passed filtering for min ",minLen," lines/values")  

  ## split matrixes or data.frames in separate lists
  chCol <- sapply(y, f1)   # refresh
  if(any(chCol)) if(length(y)==1) y <- as.list(as.data.frame(y[[1]])) else {
    while(any(chCol)) {
      i <- which(chCol)[1]
      x <- y[[i]]; xNa <- names(y)[i];
      y <- y[-i]
      dimNa <- dimnames(x)
      if(is.null(dimNa[[2]])) colnames(x) <- dimNa[[2]] <- paste0(xNa,1:ncol(x))
      x <- as.list(as.data.frame(x))
      if(any(xNa %in% names(y)) & nchar(xNa) >0) xNa <- paste0(xNa,"_2") 
      y[length(y) +(1:length(x))] <- x
      names(y)[length(y) +1 -(length(x):1)] <- if(nchar(xNa) >0) paste(xNa,names(x),sep="_") else names(x)
      chCol <- sapply(y,f1) }}
  ## transform to numeric (if possible)
  if(isTRUE(asNumeric)  & length(y) >0) y <- lapply(y, convToNum, autoConv=TRUE)
  y }
  
#' @export
.asDF2 <- function(z) { if(is.factor(z)) as.data.frame(as.character(z)) else {
  if(length(dim(z))==2) as.data.frame(z) else as.data.frame(as.matrix(z)) }}   # convert anything to data.frame-like
  
