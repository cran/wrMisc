#' Equal character-length number 
#'
#' \code{equLenNumber} convert numeric entry 'x' to text, with all elements getting the same number of characters (ie  by adding preceeding or tailing 0s, if needed).
#' So far, the function cannot handle scientific annotations. 
#'
#' @param x (caracter) input vector
#' @param silent (logical) suppress messages
#' @param debug (logical) additional messages for debugging
#' @param callFrom (character) allow easier tracking of messages produced
#' @return character vector formated as equal number of characters per value
#' @seealso \code{\link[base]{sprintf}}
#' @examples
#' equLenNumber(c(12,-3,321))
#' equLenNumber(c(12,-3.3,321))
#' @export
equLenNumber <- function(x, silent=FALSE, callFrom=NULL, debug=FALSE){
  fxNa <- .composeCallName(callFrom, newNa="equLenNumber")
  if(!isTRUE(silent)) silent <- FALSE
  if(isTRUE(debug)) silent <- FALSE else debug <- FALSE

  x <- sub("^ +","",sub(" +$","",x))              # remove heading or tailing spaces
  check1 <- grep("^[[:digit:]]+$|^\\-[[:digit:]]+$",x)                          # use as integer
  check2 <- grep("^[[:digit:]]+\\.[[:digit:]]+$|^[[:digit:]]+$|^-[[:digit:]]+$|^-[[:digit:]]+\\.[[:digit:]]+$",x)   # other numeric (wo exponent)
  if(length(check2) < length(x) && !silent) message(fxNa," ",length(x)-length(check2)," out of ",length(x)," entries can't be transformed to numeric")
  x <- as.numeric(x)
  if(length(check1) ==length(x)) sprintf(paste("%0",max(nchar(as.character(x))),"d",sep=""),x) else {
    sprintf(paste("%0",max(nchar(as.character(x))+1),".",max(nchar(gsub("\\.","",gsub("^[[:digit:]]+","",gsub("^-","",x))))),"f",sep=""),x)}
}

#' Convert to simple vector (similar to unlist)
#'
#' This function allows converting 'dat' (may be list, data.frame etc) to simple vector, more elaborate than unlist()
#' 
#' @param dat (list, data.frame) main input
#' @param toNumeric (logical)
#' @return character (or numeric) vector
#' @seealso  \code{\link[base]{unlist}}; used in  \code{\link{equLenNumber}}
#' @examples
#' aa <- matrix(11:14, ncol=2)
#' .checkConvt2Vect(aa)
#' @export
.checkConvt2Vect <- function(dat, toNumeric=TRUE) {
  ## convert 'dat' (may be list, data.frame etc) to simple vector, more elaborate unlist()
  ## if 'toNumeric'=FALSE only the 1st column of a matrix or data.frame will be extracted !!
  ## list names will be passed on (list-elements longer than 1 will get number-extesions)
  if(is.data.frame(dat)) dat <- as.matrix(dat)
  if(is.list(dat)) {
    datNa <- names(dat)
    dat <- unlist(dat)
    if(length(dat) ==length(datNa)) names(dat) <- datNa}
  if(is.matrix(dat)) {
    rowNa <- rownames(dat)
    dat <- if(toNumeric) as.numeric(dat) else dat[,1]
    if(!is.null(rowNa)) names(dat) <- rowNa }
  dat }

#' Compose sequence of (function-)calls
#'
#' This function was designed for tracing the hierarchy of function-calls.
#' It allows to remove any tailing space or ': ' from 'callFrom' (character vector) and return with added 'newNa' (+ 'add2Tail')
#' 
#' @param newNa (character vector) main input
#' @param add2Head (character)
#' @param add2Tail (character)
#' @param callFrom (character) may also contain multiple separate names (ie length >1), will be concatenated using ' -> '
#' @return character vector (history of who called whom)
#' @seealso  \code{\link[base]{paste}}
#' @examples
#' .composeCallName("newFunction", callFrom="initFunction")
#' @export
.composeCallName <- function(newNa, add2Head="", add2Tail=" : ", callFrom=NULL) {
  ## remove any tailing space or ': ' from 'callFrom' (character vector) and return with added 'newNa' (+ 'add2Tail')
  ## 'callFrom' may also contain multiple separate names (ie length >1), will be concatenated using ' -> '
  ## used for building history of who called whom ...
  if(length(callFrom) >0) {
    callFrom <- sub("[[:blank:]]+$","", callFrom)      
    callFrom <- sub("^[[:blank:]]+","", callFrom)
    callFrom <- sub("[[:blank:]]:+$","", callFrom)  
    paste(callFrom, collapse=" -> ")}
  fxNa <- paste(c(add2Head, callFrom, if(!is.null(callFrom)) " -> ", newNa, add2Tail), collapse="")
  fxNa }
       
