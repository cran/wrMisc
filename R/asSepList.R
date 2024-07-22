#' Organize Data as Separate List-Entries
#'
#' \code{asSepList} allows reorganizing most types of input into a list with separate numeric vectors. For example, matrixes or data.frames will be split into separate columns
#' (differnt to \code{\link[wrMisc]{partUnlist}} which maintains the original structure). This function also works with lists of lists.
#' This function may be helpful for reorganizing data for plots.
#'
#' @param y list to be separated/split in vectors
#' @param minLen (integer) min length (or number of rows), as add'l element to eliminate arguments given without names when asSepList is called in vioplot2
#' @param asNumeric (logical) to transform all list-elements in simple numeric vectors (won't work if some entries are character)
#' @param exclElem (character) optinal names to exclude if any (lazy matching) matches (to exclude other arguments be misinterpreted as data)
#' @param sep (character) separator when combining name of list-element to colames
#' @param fillNames (logical) add names for list-elements/ series when not given
#' @param silent (logical) suppress messages
#' @param callFrom (character) allow easier tracking of messages produced
#' @param debug (logical) display additional messages for debugging
#' @return This function returns a list, partially unlisted to vectors
#' @seealso \code{\link[wrMisc]{partUnlist}}, \code{\link[base]{unlist}}
#' @examples
#' bb <- list(fa=gl(2,2), c=31:33, L2=matrix(21:28,nc=2),
#'   li=list(li1=11:14, li2=data.frame(41:44)))
#' asSepList(bb)
#' ## multi data-frame examples
#' ca <- data.frame(a=11:15, b=21:25, c=31:35)
#' cb <- data.frame(a=51:53, b=61:63)
#' cc <- list(gl(3,2), ca, cb, 91:94, short=81:82, letters[1:5])
#' asSepList(cc)
#' cd <- list(e1=gl(3,2), e2=ca, e3=cb, e4=91:94, short=81:82, e6=letters[1:5])
#' asSepList(cd)
#' @export
asSepList <- function(y, minLen=4, asNumeric=TRUE, exclElem=NULL, sep="_", fillNames=TRUE, silent=FALSE, callFrom=NULL, debug=FALSE) {
  ## convert all data-series of list (ie all list elements or columns) in separate list-elements (OK with list of lists) eg for plots
  ## 'asNumeric'.. to transform all list-elements in simple numeric vectors (won't work if some entries are character)
  ## 'minLen' .. min length (or number of rows), as add'l element to eliminate arguments given wo names when asSepList is called in vioplot2
  ## 'fxArg' .. optinal, names to exclude if any (lazy matching) matches (to exclude other arguments be mis-interpreted as data, used in vioplot2)
  fxNa <- .composeCallName(callFrom, newNa="asSepList")
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE
  if(!isTRUE(silent)) silent <- FALSE
  namesY <- sub("[[:punct:]].*|[[:space:]].*","",deparse(substitute(y)))   # reduce to alphanum content
   aSL0 <- list(y=y,asNumeric=asNumeric,minLen=minLen,namesY=namesY)
  if(length(y) >0) {
    if(!is.list(y) || is.data.frame(y)) {    # for data.frame
      y <- .asDF2(y)
      chNam <- if(length(colnames(y)) <1) rep(TRUE, ncol(y)) else colnames(y) %in% ""
      if(any(chNam)) colnames(y)[which(chNam)] <- paste0(namesY,sep,which(chNam))
      y <- as.list(y)
      if(debug) {message(fxNa,"aSL1  Non-list concert to list of length ",length(y)); aSL1 <- list(y=y,asNumeric=asNumeric,minLen=minLen) }
    } else {
        aSL0b <- list()
      .matr2List <- function(z) as.list(as.data.frame(z))
      chSubLi <- sapply(y, is.list) & !sapply(y, is.data.frame)
      if(debug) {message(fxNa,"aSL1  list-entry; ini length of 'y' ",length(y)); aSL1 <- list(y=y,asNumeric=asNumeric,chSubLi=chSubLi) }  
      
      w <- NULL
      ## try to separate sub-lists
      if(length(y) >0 & any(chSubLi)) {                        # run partUnlist() on all sub-lists
        if(length(y)==1) { y <- .asDF2(y[[1]])
          if(debug) {message(fxNa,"aSL1b")}
          chNam <- if(length(colnames(y)) <1) rep(TRUE, ncol(y)) else colnames(y) %in% ""
          if(any(chNam)) colnames(y)[which(chNam)] <- paste0(namesY,sep,which(chNam))
          y <- as.list(y)
        } else {
          isLi <- sapply(y, inherits, "list")
          chNam <- if(length(names(y)) >0) names(y) =="" else rep(FALSE, length(y))
          if(any(chNam) & isTRUE(fillNames)) {newNa <- paste(namesY,which(chNam),sep=sep)
            if(any(newNa %in% names(y))) newNa <- paste0(namesY,sep,"_",which(chNam))
              names(y)[which(chNam)] <- newNa }
          if(debug) {message(fxNa,"aSL2  "); aSL2 <- list(isLi=isLi,chNam=chNam)}

          if(any(isLi)) y <- partUnlist(y, silent=silent, debug=debug,callFrom=fxNa)  #[which(chSubLi)])
          ## now need to separate matrix-columns (& check names)
          iniDim <- lapply(y, ncol)
          ch2d <- sapply(iniDim, function(x) length(x)==1)
          if(any(ch2d)) {             ## contains matrix or data.frame, need to separate cols
            w <- lapply(y[which(ch2d)], .matr2List)
            w <- partUnlist(w, silent=silent,debug=debug,callFrom=fxNa)
            names(w) <- paste0(rep(names(y)[which(ch2d)],unlist(iniDim[which(ch2d)])),sep, unlist(lapply(unlist(iniDim[which(ch2d)]), function(x) if(x >1) 1:x else ""))) }
          y <- y[-which(ch2d)]                        # remove matrix parts
          y[length(y) +(1:length(w))] <- w            # attach separated columns
          names(y)[1 +length(y) -(length(w):1)] <- names(w)
          if(debug) {message(fxNa,"aSL3   Length of basic part ",length(y),"   length of matrix-part ",length(w))}

          ## adjust order
          newOr <- as.list(match(names(iniDim), names(y)))
          chNa <- is.na(match(names(iniDim), names(y)))
          if(any(chNa, na.rm=TRUE)) newOr[which(chNa)] <- lapply(paste0("^",names(iniDim)[which(is.na(match(names(iniDim), names(y))))],sep), grep, names(y))
          y <- y[unlist(newOr)]
          names(y) <- sub(paste0(sep,"$"),"", names(y))     # remove tailing sep from single-column matrices
        }
      }
    }
    if(debug) {message(fxNa,"aSL4   Length of list output (befor minLen-filter) ",length(y))}
    ## check length
    chLe <- sapply(y, length) < minLen
    if(any(chLe, na.rm=TRUE)) { y <- y[which(!chLe)]
      if(all(chLe, na.rm=TRUE) & !silent) message(fxNa,"All elements of ',namesY,' below length-limit (",minLen,") - nothing remains") }
    ## convert to numeric
    if(isTRUE(asNumeric)) { chMode <- sapply(y, function(x) "numeric" %in% mode(x))
      if(any(!chMode)) y[which(!chMode)] <- lapply(y, convToNum, callFrom=fxNa,silent=silent) }
    if(debug) {message(fxNa,"aSL5   Length of list output (after minLen-filter) ",length(y))}
  } else if(debug(fxNa,"Empty input, nothing to do"))
  if(debug) message(fxNa,"Returning list of length ",length(y))
  y }


#' Convert anything to data.frame
#'
#' This function allows converting anything to data.frame
#' 
#' @param z (numeric vector, factor, matrix or list) main input
#' @return data.frame
#' @seealso  \code{\link[base]{as.data.frame}}
#' @examples
#' .asDF2(c(3:6))
#' @export
.asDF2 <- function(z) if(is.factor(z)) as.data.frame(as.character(z)) else as.data.frame(z)  # convert anything to data.frame-like
  
